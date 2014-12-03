(* Copyright (C) 2014, Marco Stronati, All Rights Reserved.
   This file is distributed under the terms of the
   GNU General Public License version 3 or later. *)

(**
Implements queries for OSM database in Postgresql.
 *)

open Geo
open Printf
open Postgresql


(* most of amentity tags *)
(* leisure, sport, tourism may be of interest *)
let financial_tags = ["atm"; "bank"; "bureau_de_change"]
let food_tags = ["bar"; "biergarten"; "cafe"; "drinking_water"; "fast_food"; "food_court"; "ice_cream"; "pub"; "restaurant"]
let education_tags = ["college"; "kindergarten"; "library"; "school"; "university"]
let healthcare_tags = ["clinic"; "dentist"; "doctors"; "hospital"; "nuersing_home"; "pharmacy"; "social_facility"]
let entertainment_tags = ["arts_centre"; "brothel"; "cinema"; "community_centre"; "nightclub"; "planetarium"; "social_centre"; "stripclub"; "swingerclub"; "theatre"]
let other_tags = ["courthouse"; "crypt"; "embassy"; "fire_station"; "grave_yard"; "marketplace"; "place_of_worship"; "police"; "post_office"; "prison"; "townhall"]
let transportation_tags = ["bicycle_parking"; "bicycle_rental"; "bus_station"; "car_rental"; "ferry_terminal"; "fuel"; "taxi"]

let osm_tags = food_tags @ education_tags @ entertainment_tags @ other_tags @ transportation_tags @ financial_tags @ healthcare_tags


(* in case the db_srid is different from the srid of the grid *)
(* let string_of (x,y) =  *)
(*     sprintf "ST_Transform(ST_GeomFromText('POINT(%f %f)',%i),%i)" Cartesian.srid x y db_srid *)
let string_of p = 
  let x,y = Utm.tuple p in
  sprintf "ST_GeomFromText('POINT(%f %f)',%i)" x y Utm.srid

let string_of_polygon coord radius =
  let dist_center_edge = radius *. (sqrt 2.) in
  let p1 = Utm.destination coord 45. dist_center_edge in
  let p2 = Utm.destination coord 135. dist_center_edge in
  let p3 = Utm.destination coord 225. dist_center_edge in
  let p4 = Utm.destination coord 315. dist_center_edge in
  let lat1,lon1 = Wgs.tuple (Wgs.of_xy p1) in
  let lat2,lon2 = Wgs.tuple (Wgs.of_xy p2) in
  let lat3,lon3 = Wgs.tuple (Wgs.of_xy p3) in
  let lat4,lon4 = Wgs.tuple (Wgs.of_xy p4) in
     
  sprintf "ST_Transform(ST_GeomFromText('POLYGON((%f %f, %f %f, %f %f, %f %f, %f %f))',%i),%i)" 
          lon1 lat1 
          lon2 lat2 
          lon3 lat3 
          lon4 lat4 
          lon1 lat1
          Wgs.srid
          Conf.DB.db_srid


(* TODO maybe this transform to 4326 are not necessary *)
          
(* without the cast to geography it doesn't work *)
let distance s1 s2 = 
  sprintf "ST_Distance(ST_Transform(%s,4326)::geography,
                       ST_Transform(%s,4326)::geography)" s1 s2

(* should be faster than distance *)
let within s1 s2 dist = 
  sprintf "ST_DWithin(ST_Transform(%s,4326)::geography,
                      ST_Transform(%s,4326)::geography,
                      %f)" s1 s2 dist

(* let intersects s1 s2 =  *)
(*   sprintf "ST_Intersects(ST_Transform(%s,4326)::geography, *)
(*                          ST_Transform(%s,4326)::geography)" s1 s2 *)
let intersects s1 s2 = 
  sprintf "ST_Intersects(%s,
                         %s)" s1 s2




(* given a list of tags return a string "amenity = 'tag1' OR amenity = tag2 ..." suitable for a WHERE filter *)
let amenity_filter_of_list l = 
  if l = [] then failwith "empty list";
  let add = "\nOR " in
  let s = List.fold_left (fun tmp tag -> Printf.sprintf 
                                           (* "%s%samenity = \'%s\'"  *) (* planet_osm *)
                                           "%s%stype = \'%s\'" (* imposm *)
                                           tmp add tag) "" l in
  String.sub s (String.length add) (String.length s -(String.length add))


(* SELECT place,population,way  *)
(* FROM planet_osm_point  *)
(* WHERE population != '' AND place != ''; *)


(* planet_osm_version *)
(* let make_building_query coord radius = sprintf *)
(* "SELECT count(\*\) *)
(* FROM planet_osm_polygon *)
(* WHERE %s *)
(* AND building != ''" *)
(* (within "way" (string_of coord) radius) *)

(* this consistently reports smaller numbers *)
let make_building_query_within coord radius = sprintf
"SELECT count(*)
FROM osm_buildings
WHERE %s"
(within "geometry" (string_of coord) radius)

let make_building_query coord radius = sprintf
"SELECT count(*)
FROM osm_buildings
WHERE %s"
(intersects "geometry" (string_of_polygon coord radius))


(* this query is not correct, it takes the value of population from a close point
typically the problem is that paris is a point with huge population, arr14 is a point with small population that is a subset of paris
the solution could be understanding the different between paris and arr14 with some other tags.
The proper way to to this would be to compute the population density, which requires computing the area.
 *)
let make_population_query coord radius = sprintf
"SELECT population
FROM planet_osm_point
WHERE population != ''
ORDER BY %s
LIMIT 1"
(distance "way" (string_of coord))


let make_poi_query tag_list coord radius = sprintf
"SELECT count(*)
FROM osm_amenities
WHERE %s
AND (%s)" (within "geometry" (string_of coord) radius) (amenity_filter_of_list tag_list)

(* planet_osm version *)
(* let make_poi_query tag_list coord radius = sprintf *)
(* "SELECT count(\*\) *)
(* FROM planet_osm_point *)
(* WHERE %s *)
(* AND (%s)" (within "way" (string_of coord) radius) (amenity_filter_of_list tag_list) *)


let int_parser res = 
  match res with 
    res::[] -> Scanf.sscanf res "%i" (fun x -> x)
  | _ -> failwith "unsupported result"


let print_res conn res =
  match res#status with
  | Empty_query -> printf "Empty query\n"
  | Command_ok -> printf "Command ok [%s]\n" res#cmd_status
  | Tuples_ok ->
      printf "Tuples ok\n";
      printf "%i tuples with %i fields\n" res#ntuples res#nfields;
      print_endline (String.concat ";" res#get_fnames_lst);
      for tuple = 0 to res#ntuples - 1 do
        for field = 0 to res#nfields - 1  do
          printf "%s, " (res#getvalue tuple field)
        done;
        print_newline ()
      done
  | Copy_out -> printf "Copy out:\n"; conn#copy_out print_endline
  | Copy_in -> printf "Copy in, not handled!\n"; exit 1
  | Bad_response -> printf "Bad response: %s\n" res#error; conn#reset
  | Nonfatal_error -> printf "Non fatal error: %s\n" res#error
  | Fatal_error -> printf "Fatal error: %s\n" res#error

let rec dump_res conn =
  match conn#get_result with
  | Some res -> print_res conn res; flush stdout; dump_res conn
  | None -> ()

(* let rec dump_notification conn = *)
(*   match conn#notifies with *)
(*   | Some (msg, pid) -> *)
(*       printf "Notication from backend %i: [%s]\n" pid msg; *)
(*       flush stdout; *)
(*       dump_notification conn *)
(*   | None -> () *)


let parse tuple_parser conn = 
  match conn#get_result with
  | Some res -> 
    (match res#status with 
    | Tuples_ok -> List.map (tuple_parser) res#get_all_lst
    | _ -> failwith "failed query")
  | None -> failwith "failed query"

let run q p =
  let conn = new connection ~conninfo:Conf.DB.conn () in
  conn#set_notice_processor (fun s -> eprintf "postgresql error [%s]\n" s);
  conn#send_query q;
  let res = Some (parse p conn) in
  conn#finish;
  res


let get_population coord radius =
  (* osm_admin population can be null, geometry is point *)
  let q = make_population_query coord radius in
  let p = int_parser in
  let res = run q p in
  match res with
    Some [i] -> Printf.printf "get pop %i\n" i; float_of_int i
  | _ -> failwith "failed population"


let get_poi tags coord radius =
  let q = make_poi_query tags coord radius in
  let p = int_parser in
  let res = run q p in
  match res with
    Some [i] -> 
    float_of_int i
  | _ -> failwith "failed poi"

let get_buildings coord radius =
  let q = make_building_query coord radius in
  let p = int_parser in
  let res = run q p in
  match res with
    Some [i] -> 
    float_of_int i
  | _ -> failwith "failed poi"


let get_weight id coord radius = 

  let (lat,lon) = Wgs.tuple (Wgs.of_xy coord) in

  let poi_multiplier = 3. in
  let poi = get_poi (osm_tags) coord radius in

  let oc = open_out_gen [Open_wronly; Open_append; Open_creat] 0o666 "poi.query" in
  Printf.fprintf oc "%05i,%+e,%+e,%+e,%+e\n" id lat lon radius poi;
  close_out oc;
            
  let build = get_buildings coord radius in

  let oc = open_out_gen [Open_wronly; Open_append; Open_creat] 0o666  "build.query" in
  Printf.fprintf oc "%05i,%+e,%+e,%+e,%+e\n" id lat lon radius build;
  close_out oc;

   poi *. poi_multiplier +. build
