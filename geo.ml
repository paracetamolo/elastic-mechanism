(* Copyright (C) 2014, Marco Stronati, All Rights Reserved.
   This file is distributed under the terms of the
   GNU General Public License version 3 or later. *)

open Batteries

let pi = 4. *. atan 1.                (* 3.14... *)
let earth_radius = 6378137.           (* earth radius in meters *)

let rad_of_deg ang = ang *. pi /. 180.
let deg_of_rad ang = ang *. 180. /. pi


let pj_latlon = Proj4.init_plus "+proj=latlong +ellps=WGS84"

let pj_merc = Proj4.init_plus "+proj=utm +zone=50N" (* for beijing *)
let srid_merc = 32650              (* beijing *)

(* let pj_merc_init_string = "+proj=utm +zone=31N" (\* for paris *\) *)
(* let srid_merc = 32631              (\* for paris: WGS 84 / UTM zone 31N : projection in meters with distance in meters *\) *)


module rec Wgs : sig
    type t
    val srid : int
    val make : float -> float -> t
    val distance : t -> t -> float
    val destination : t -> float -> float -> t
    val of_xy : Utm.t -> t
    val tuple : t -> float * float
    val string_of : t -> string
end
=
struct
  (* TODO write everywhere if they take/return degrees/radiants *)

  type t = float * float

  let srid = 4326               (* WGS84 *)

  (* check bound of lat/lon expressed in degrees *)
  let valid_lon lon = if -180. < lon && lon <= 180. then true else false
  let valid_lat lat = if -90. <= lat && lat <= 90. then true else false
                                                                  
  let make lat lon =
    if valid_lat lat && valid_lon lon
    then (lat,lon)
    else raise (Invalid_argument (Printf.sprintf "Invalid latitude or longitude (%f,%f)" lat lon))


  let of_xy (x,y) =
    let lon_rad,lat_rad = Proj4.transform_one_tuple pj_merc pj_latlon x y in
    let lat = deg_of_rad lat_rad in
    let lon = deg_of_rad lon_rad in
    make lat lon

  let tuple c = c

  let string_of c = Printf.sprintf "%f %f" (fst c) (snd c)
     
(* uses the ‘haversine’ formula to calculate the great-circle distance between two points. 
   that is, the shortest distance over the earth’s surface 
   answer is in meters
   http://www.movable-type.co.uk/scripts/latlong.html
*)
  let distance (lat1,lon1) (lat2,lon2) =
  
  let dLat = rad_of_deg (lat2 -. lat1) in
  let dLon = rad_of_deg (lon2 -. lon1) in
  let lat1 = rad_of_deg lat1 in
  let lat2 = rad_of_deg lat2 in
  
  let a = (sin (dLat /. 2.)) *. (sin (dLat /. 2.)) +.
    (sin (dLon /. 2.)) *. (sin (dLon /. 2.)) *. (cos lat1) *. (cos lat2) in 
  let c = 2. *. (atan2 (sqrt a) (sqrt (1. -. a))) in 
  let d = earth_radius *. c in
  d                             (* d is in meters *)


(* bearing radians: 0 north, pi/2 east, pi south, 3/2pi west
distance meters *)
let destination start bearing_dg distance = 

  let (lat1,lon1) = start in
  let lat1 = rad_of_deg lat1 in
  let lon1 = rad_of_deg lon1 in
  let bearing = rad_of_deg bearing_dg in

  let lat2 = asin ( ((sin lat1) *. (cos (distance /. earth_radius))) +.
    ((cos lat1) *. (sin (distance /. earth_radius)) *. (cos bearing)) )
  in

  let lon2 = lon1 +. (atan2 ((sin bearing) *. (sin (distance /. earth_radius)) *. (cos lat1))
                        ((cos (distance /. earth_radius)) -. ((sin lat1) *. (sin lat2))))
  in
  (deg_of_rad lat2, deg_of_rad lon2)


let initial_bearing (lat1,lon1) (lat2,lon2) =
      let lat1 = rad_of_deg lat1 in
      let lon1 = rad_of_deg lon1 in
      let lat2 = rad_of_deg lat2 in
      let lon2 = rad_of_deg lon2 in

      let y = (sin (lon2 -. lon1)) *. (cos lat2) in
      let x = (cos lat1) *. (sin lat2) -.
        (sin lat1) *. (cos lat2) *. (cos (lon2 -. lon1)) 
      in
      let bearing_rad = atan2 y x in
      let bearing_deg = deg_of_rad bearing_rad in (* [-180,180] *)
      float_of_int ((int_of_float (bearing_deg +. 360.)) mod 360)

                   

end
and Utm : sig
            type t
            val srid : int
            val make : float -> float -> t
            val distance : t -> t -> float
            val destination : t -> float -> float -> t
            val of_latlon : Wgs.t -> t
            val tuple : t -> float * float
            val string_of : t -> string
          end
  = struct
  type t = float * float

  let srid = srid_merc

  let make x y = (x,y)

  let of_latlon (lat,lon) =
    let lat_rad = rad_of_deg lat in
    let lon_rad = rad_of_deg lon in
    let (x,y) = Proj4.transform_one_tuple pj_latlon pj_merc lon_rad lat_rad in
    make x y


  let tuple c = c

  let string_of c = Printf.sprintf "%f %f" (fst c) (snd c)
     
        
  (*
  @param (x0,y0) point
  @param (c0,c1) parameters of the line y = c0 + c1 x
  @return distance between the point and the line
  https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
   *)
  let perpendicular_distance (x0,y0) (c0,c1) =
    (abs_float ((c1 *. x0) -. y0 +. c0)) /. (sqrt ((c1 ** 2.) +. 1.))


  let project_point_to_line (x0,y0) (c0,c1) = 
    let x = ((c1 *. y0) +. x0 -. (c1 *. c0)) /. ((c1 ** 2.) +. 1.) in
    let y = c0 +. (x *. c1) in
    (x,y)


  (* planar distance between two points - simple Pythagoras *)
  let distance (x1,y1) (x2,y2) = sqrt ((x2 -. x1) ** 2. +. (y2 -. y1) ** 2.)

  (* use only for small distances *)
  (* direction is an angle in degrees: e.g.  0. east, 90. north, 180. west, 270 south *)
  let destination start direction distance = 
    let (x,y) = start in
    let step_x = distance *. (cos (rad_of_deg direction)) in
    let step_y = distance *. (sin (rad_of_deg direction)) in
    let next = (x +. step_x, y +. step_y) in
    next

end


type box = Central of Utm.t * float


(* Mercartor projection *)
module Mercartor = struct
(* 
   Mercator projection 
   https://wiki.openstreetmap.org/wiki/Mercator
   https://en.wikipedia.org/wiki/Mercator_projection
*)

(* representative fraction or principal scale
   rf = radius /. earth_radius
   the resulting map will have  -pi*radius <= x <= pi*radius *)
let rf = 1.
let radius = earth_radius *. rf       (* radius of the projected map *)
(* let scale_factor y = cosh (y /. radius);;  *)

let x_of_lon lon = radius *. (rad_of_deg lon);;
let y_of_lat lat = radius *. log (tan (pi /. 4. +. (rad_of_deg lat) /. 2.));;
let xy_of_latlon (lat,lon) = (x_of_lon lon, y_of_lat lat);;

let lon_of_x x = deg_of_rad (x /. radius);;
let lat_of_y y = deg_of_rad (2. *. (atan (exp (y /. radius))) -. pi /. 2.);;  
let latlon_of_xy (x,y) = (lat_of_y y, lon_of_x x);;

(* distance for xy points in meters.
   Return the ruler distance corrected, it should be (almost) the same value of distance_latlon.
   https://en.wikipedia.org/wiki/Mercator_projection#Formulae_for_distance
*)
(* let distance_xy (x1,y1) (x2,y2) = *)
(*   let d_ruler = distance_ruler_2 (x1,y1) (x2,y2) in *)
(*   let middle_lat = (lat_of_y y1 +. lat_of_y y2) /. 2. in *)
(*   let distance_in_m = d_ruler *. (cos (rad_of_deg middle_lat)) /. rf in *)
(*   distance_in_m *)
end
    

let beijing  = Wgs.make 39.9059093 116.3913489
let beijing_uni = Wgs.make 39.982501 116.326649
let beijing3 = Wgs.make 39.9059093 116.391349
let beijing1 = Wgs.make 39.9066926 116.3900475 (* distance 100 m *)
let beijing2 = Wgs.make 39.906721 116.3912176

let paris1 = Wgs.make 48.8443899 2.3336967   (* distance 100 m *)
let paris2 = Wgs.make 48.8443511 2.33506

let paris    = Wgs.make 48.8567 2.3487
let paris_nw = Wgs.make 48.898807 2.258034
let paris_outer_nw = Wgs.make 48.943291 2.089334
let newyork  = Wgs.make 40.7305991  (-73.9865812)
let rio      = Wgs.make (-22.9112163)  (-43.2093781)


let defense  = Wgs.make 48.8944 2.2470
let champs   = Wgs.make 48.8730 2.3040
let halles   = Wgs.make 48.868382 2.350047
let boulogne = Wgs.make 48.8744 2.2656


(* box stuff *)
(* a box is a represented by two coordinates, the north-west and south-east corners *)

(* let box_xy_of_latlon (nw,se) = *)
(* (lat_min, lat_max, lon_min, lon_max) = *)
(*   let x_min = x_of_lon lon_min in  *)
(*   let x_max = x_of_lon lon_max in  *)
(*   let y_min = y_of_lat lat_min in  *)
(*   let y_max = y_of_lat lat_max in  *)
(*   (x_min,x_max,y_min,y_max) *)


(* let size_of_box (lat_min, lat_max, lon_min, lon_max) = *)
(*   let oriz = distance_latlon (lat_min,lon_min) (lat_min,lon_max) in *)
(*   let vert = distance_latlon (lat_min,lon_min) (lat_max,lon_min) in *)
(*   (oriz,vert) *)

(* type box = coord * coord *)

(* let is_in_box (nw,se) pos =  *)
(*   match (nw,se) with *)
(*   | Wgs(nw_lat,nw_lon),Wgs(se_lat,se_lon) -> *)
(*      let lat,lon = latlon_of pos in  *)
(*      if se_lat <= lat && lat <= nw_lat && *)
(*         nw_lon <= lon && lon <= se_lon *)
(*      then true *)
(*      else false *)
(*   | Utm(nw_x,nw_y),Utm(se_x,se_y) -> *)
(*      let x,y = xy_of pos in  *)
(*      if nw_x <= x && x <= se_x && *)
(*         se_y <= y && y <= nw_y *)
(*      then true *)
(*      else false *)
(*   | _ -> failwith "Wrong arguments" *)

(* let is_in_any_box boxes pos =  *)
(*   List.fold_left (fun tmp box -> (is_in_box box pos) || tmp) false boxes *)


(* let box_radius pts_xy =  *)
(*   let top_x = ref min_float in *)
(*   let top_y = ref min_float in *)
(*   let bot_x = ref max_float in *)
(*   let bot_y = ref max_float in *)
(*   let apts = Array.of_list pts_xy in *)
 
(*   for i = 0 to Array.length apts -1 do *)
(*     let (x,y) = apts.(i) in *)
(*     if !top_x < x then top_x := x; *)
(*     if !top_y < y then top_y := y; *)
(*     if !bot_x > x then bot_x := x; *)
(*     if !bot_y > y then bot_y := y; *)
(*   done; *)
(*   let dx = (distance_ruler_2 (!bot_x,!bot_y) (!top_x,!bot_y)) in *)
(*   let dy = (distance_ruler_2 (!bot_x,!bot_y) (!bot_x,!top_y)) in  *)
(*   (avg [dx;dy]) /. 2. *)
(* ;; *)

(* let p1_ll = (48.84437,2.332964);; *)
(* let p2_ll = (48.844299,2.337513);; *)
(* end *)
