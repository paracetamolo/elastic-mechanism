(* Copyright (C) 2014, Marco Stronati, All Rights Reserved.
   This file is distributed under the terms of the
   GNU General Public License version 3 or later. *)

(**
Contains all configuration parameters.
 *)

module DB = struct
  let conn = "host=localhost user=postgres password=postgres dbname=imposm"
  let db_srid = 4326              (* lat lon coord and distance in degrees *)
end

             
open Geo

module Grid = struct
  let center = Utm.of_latlon Geo.beijing_uni
  let length = 100
  let step = 100.

  let r_city    =  300.
  let r_country = 3000.

  (* let fence_center = Utm.of_latlon (Wgs.make 48.831513 2.323638) (\* south paris *\) *)
  (* let fence_size = 350. *)
  (* let fences = [Central(fence_center,fence_size)] *)
  let fences = []

let to_string () = 
  let spt p = Wgs.string_of (Wgs.of_xy p) in

  Printf.sprintf "
Grid:
center:  %s %s
length:  %i
step:    %.0f m
r_city:  %.0f m
r_cntry: %.0f m
"
"Paris" (spt center) length step r_city r_country 
end

                
module Algo = struct
  let stop_percent = 95.
                       
  let max_distance = (log 6.) *. 2.
                         
  let pp_star = 1.
  let r_star = log 2.
                   
  let square r =
    let a = pp_star /. (r_star ** 2.) in
    (* if 0. <= r && r <= max_distance *)
    (* then *) a *. (r ** 2.)
    (* else a *. (max_distance ** 2.) *)
                      
  let square_inverse n =
    let a = pp_star /. (r_star ** 2.) in
    let n_max = a *. (max_distance ** 2.) in
    if 0. <= n && n <= n_max
    then sqrt (n /. a)
    else infinity

  let linear r =
    let a = pp_star /. r_star in
    if 0. <= r && r <= max_distance
    then a *. r
    else a *. max_distance

  let linear_inverse n =
    let a = pp_star /. r_star in
    let n_max = a *. max_distance in
    if 0. <= n && n <= n_max
    then n /. a
    else infinity

  let requirement = square

  let radius_per_points = square_inverse


let to_string () = 
  Printf.sprintf "
Algo:
stop:    %.0f%%
pp*:     %.0f
r*:      log %.0f
d_max:   log %.0f
"
   stop_percent pp_star (exp r_star) (exp max_distance)

end


module Evaluation = struct
                       
  let strong = Utm.of_latlon Geo.beijing_uni
  let weak   = Utm.of_latlon Geo.beijing_uni
               
  let home = Utm.of_latlon Geo.beijing_uni
  let work = Utm.of_latlon Geo.beijing_uni
                           
  let stat_strong = Grid.center
  let stat_weak = Utm.of_latlon Geo.beijing_uni (* country *)
  let stat_length = Grid.step *. 100.  (* step * number of cells = meters*)

let to_string () =
  let spt p = Wgs.string_of (Wgs.of_xy p) in

  Printf.sprintf "
strong box
center:  %s
weak box
center:  %s
length:  %.0f

strong:  %s
weak:    %s
"
      (spt stat_strong) (spt stat_weak) (stat_length /. Grid.step)
      (spt strong) (spt weak)

end


let to_string () = 
  (Grid.to_string ())^"\n"
  ^(Algo.to_string ())^"\n"
  ^(Evaluation.to_string ())^"\n"
