open Geo
open Printf
open Query
open Postgresql
       
let curie = Wgs.make 48.8426448372624 2.34351961315083
let other = Wgs.make 48.8567 2.3487
let _ = printf "distance paris curie: \n latlon %f\n xy %f \n"
  (Wgs.distance curie other)
  (Utm.distance (Utm.of_latlon curie) (Utm.of_latlon other))

(* test distance *)
let q1 = sprintf
"SELECT ST_Distance(ST_GeographyFromText('SRID=4326;POINT(2.3487 48.8567)'),
                    ST_GeographyFromText('SRID=4326;POINT(2.34351961315083 48.8426448372624)'))"
(* Result: 1608 *)

(* let q = sprintf *)
(* "SELECT ST_Distance(ST_Transform(ST_GeomFromText('POINT(2.3487 48.8567)',4326),900913), *)
(*                     ST_Transform(ST_GeomFromText('POINT(2.34351961315083 48.8426448372624)',4326),900913))" *)
(* Result: 2446 *)

(* let q = sprintf *)
(* "SELECT ST_Distance(ST_Transform(ST_GeomFromText('POINT(2.3487 48.8567)',4326),32631), *)
(*                     ST_Transform(ST_GeomFromText('POINT(2.34351961315083 48.8426448372624)',4326),32631))" *)
(* Result: 1608 *)


let q2 = make_poi_query (osm_tags) (Utm.of_latlon Geo.paris) 500.
let q3 = make_building_query (Utm.of_latlon Geo.paris) 500.


let test_query q =
  Printf.printf "Query:\n%s\n" q;
  let res =
    try
      run q int_parser
    with
    | Error e -> prerr_endline (string_of_error e); None
    | e -> prerr_endline (Printexc.to_string e); None
  in
  match res with
    Some [i] -> Printf.printf "Query:\n%s\n\nResult: %i\n" q i
  | _ -> Printf.printf "Nada\n"

let _ = List.iter (test_query) [q1;q2;q3]
