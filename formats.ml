(* Copyright (C) 2014, Marco Stronati, All Rights Reserved.
   This file is distributed under the terms of the
   GNU General Public License version 3 or later. *)

(**
  Parser/Printer for geojson, gpx, plt formats.
 *)

open Batteries
open Geo

let default_timestamp = CalendarLib.Printer.Calendar.from_fstring "%FT%TZ" "1900-01-01T00:00:00Z"

(* Type of a freshly parsed trace *)
type point = {
  coord: Utm.t; 
  idx: int; 
  time: CalendarLib.Calendar.t}

(* meta information attached to an elaborated trace, to be dumped *)
type pred_params = {
  d: int; 
  turn: bool; 
  pred_e: float
}

type meta = {
  i: int; 
  t: CalendarLib.Calendar.t; 
  h: bool; 
  e: float; 
  et: float; 
  l: float; 
  pred: pred_params
}

(*                   *)
(* GPX PARSER/DUMPER *)
(*                   *)

open Xml

(* parse a gpx and return a list of list of points
   latitude and longitude are in decimal degrees and converted to planar coordinates

   the head of the list are the most recent points, the last of the gpx file
*)

let rec track_of_trk node =
  match node with 
  | PCData _ -> failwith "PCData"
  | Element (t,a,c) ->  match t with
    | "trk" 
    | "trkseg" -> BatList.fold_left (fun list child -> BatList.append (track_of_trk child) list) [] c
    | "trkpt" -> 
      let latlon = Wgs.make (float_of_string (snd (BatList.nth a 0))) (float_of_string (snd (BatList.nth a 1))) in

      let time = 
        
        let elements_time = (BatList.filter (fun ele -> match ele with Element ("time",_,_) -> true | _ -> false) c) in
        if BatList.length elements_time > 0 
        then
          let time_string = match (BatList.hd elements_time) with Element ("time",_,[PCData value]) -> value | _ -> failwith "track_of_trk" in
          CalendarLib.Printer.Calendar.from_fstring "%FT%TZ" time_string 
        else
          CalendarLib.Printer.Calendar.from_fstring "%FT%TZ" "1900-01-01T00:00:00Z"
      in

      (Utm.of_latlon latlon, time)::[]
    | _ -> []


let rec track_of_gpx node = 
  match node with 
  | PCData _ -> failwith "PCData"
  | Element(t,_,c) ->  
    match t with
    | "gpx" -> 
      let tracks_metaless = BatList.fold_left 
        (fun list child -> 
          let res = (track_of_trk child) in 
          if (BatList.length res) > 1 then res::list else list) 
        [] c
      in
      let track_metaless = BatList.flatten tracks_metaless in
      let l = BatList.length track_metaless in
      BatList.mapi (fun idx pt -> {coord = (fst pt); idx=l-idx; time=(snd pt)} ) track_metaless
    | _ -> []




(* parse a list of list of points and return a gpx
   latitude and longitude are in planar coordinates and converted to decimal degrees

   the head of the list are the most recent points
*)


let gpx_of_track track = 
  let element_of_point pt = 
    let time = pt.time in
    let (lat,lon) = Wgs.tuple (Wgs.of_xy pt.coord) in
    Element("trkpt",
            [("lat", (Printf.sprintf "%8.6f" lat));
             ("lon", (Printf.sprintf "%8.6f" lon))],
            [Element("time",[],[PCData (CalendarLib.Printer.Calendar.sprint "%FT%TZ" time)])]) in
  
  let element_of_track track =          
    let rtrack = List.rev track in
    let trkpt_list = List.map (element_of_point) rtrack in
    Element("trkseg",[],trkpt_list) in

  let trk_list = element_of_track track in (* todo this need to be inverted? *)
  Element("gpx",
          [("version", "1.0"); ("creator", "pdp"); 
           ("xmlns", "http://www.topografix.com/GPX/1/0")],
          [Element("trk",[],[trk_list])])


let gpx_of_rich_track rich_track = 
  let track = List.map (fun (pt,meta) -> {coord=pt; idx=meta.i; time=meta.t}) rich_track in
  gpx_of_track track


let xml_to_file file gpx = 
  let header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" in
  let out = open_out file in
  output_string out (header^(Xml.to_string_fmt gpx));
  close_out out




(* 
   GEO JSON DUMPER
*)
let geojson_of_rich_track track = 
  (* note: order for geo_json points is: longitude, latitude, altitude *)
  let features = List.fold_left
    (fun tmp (pt,meta) -> 
        let (lat,lon) = Wgs.tuple (Wgs.of_xy pt) in
        let json_point = Printf.sprintf
        "{ \"type\" : \"Feature\",
           \"geometry\": {
              \"type\": \"Point\",
              \"coordinates\": [%f, %f]
            },
           \"properties\": {
             \"idx\" : %i,
             \"time\" : \"%s\",
             \"hard\" : %b,
             \"epsilon\" : %f,
             \"epsilon_theta\" : %f,
             \"alpha\" : %s,
             \"depth\" : %i,
             \"err\" : %f,
             \"turn\": %b
            }
         }," 
        lon lat meta.i (CalendarLib.Printer.Calendar.sprint "%F %T" meta.t) meta.h meta.e meta.et 
        (if meta.l = infinity then "\"Infinity\"" else Printf.sprintf "%f" meta.l)
        meta.pred.d meta.pred.pred_e meta.pred.turn in
      tmp^json_point) 
    "" track in
  let features_without_last_coma = String.sub features 0 (String.length features -1) in 

  "{ \"type\": \"FeatureCollection\",
    \"features\": [" ^ features_without_last_coma ^ "]}"


let geojson_of_simple_track track = 
  (* note: order for geo_json points is: longitude, latitude, altitude *)

  let features = List.fold_left
    (fun tmp pt -> 
        let (lat,lon) = Wgs.tuple (Wgs.of_xy pt.coord) in
        let json_point = Printf.sprintf
        "{ \"type\" : \"Feature\",
           \"geometry\": {
              \"type\": \"Point\",
              \"coordinates\": [%f, %f]
            },
           \"properties\": {
             \"idx\" : %i,
             \"time\" : \"%s\"
            }
         }," 
        lon lat pt.idx (CalendarLib.Printer.Calendar.sprint "%F %T" pt.time) 
        in
      tmp^json_point) 
    "" track in
  let features_without_last_coma = String.sub features 0 (String.length features -1) in 

  "{ \"type\": \"FeatureCollection\",
    \"features\": [" ^ features_without_last_coma ^ "]}"


let geojson_to_file geojson name = 
  let out = open_out name in
  output_string out geojson;
  close_out out




(* plt parser *)

let track_of_plt filename = 
  let track = ref [] in

  let filelines = File.lines_of filename in
  Enum.drop 6 filelines;
  Enum.iteri (fun i line ->
    (* Printf.printf "%i %s\n" i line; *)

    let pt = Scanf.sscanf line "%f,%f,0,%f,%f,%s@,%s"
      (fun lat lon alt timeweird date time ->
        (* Printf.printf "lat %f lon %f time %s\n" lat lon date; *)
        let coord = Utm.of_latlon (Wgs.make lat lon) in
        let time = CalendarLib.Printer.Calendar.from_fstring "%FT%TZ" (date^"T"^time^"Z") in
        {coord = coord; idx = i; time = time} )
    in
    track := pt::!track)
    filelines;
  !track


let gpx_of_plt filename_src filename_dst =
  let track = track_of_plt filename_src in
  xml_to_file filename_dst (gpx_of_track track)
  


(* tdrive custom format parser *)


let track_of_tdrive filename = 
  let track = ref [] in

  let filelines = File.lines_of filename in
  (* Enum.drop 6 filelines; *)
  Enum.iteri (fun i line ->
    (* Printf.printf "%i %s\n" i line; *)

    let pt = Scanf.sscanf line "%i,%s %s@,%f,%f"
      (fun id date time lon lat ->
        (* Printf.printf "lat %f lon %f date %s time %s\n" lat lon date time; *)
        let coord = Utm.of_latlon (Wgs.make lat lon) in
        let time = CalendarLib.Printer.Calendar.from_fstring "%FT%TZ" (date^"T"^time^"Z") in
        {coord = coord; idx = i; time = time} )
    in
    track := pt::!track)
    filelines;
  !track


let gpx_of_tdrive filename_src filename_dst =
  let track = track_of_tdrive filename_src in
  xml_to_file filename_dst (gpx_of_track track)
