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
  let center = Utm.of_latlon Geo.paris
  let length = 100
  let step = 100.

  let r_city    =  300.
  let r_country = 3000.

  let fence_center = Utm.of_latlon (Wgs.make 48.831513 2.323638) (* south paris *)
  let fence_size = 350.
  (* let fences = [Central(fence_center,fence_size)] *)
  let fences = []
end

                
module Algo = struct
  let stop_percent = 95.
                       
  let max_distance = log 6.
                         
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
end


module Evaluation = struct
                       
  let strong = Utm.of_latlon Geo.halles
  let weak   = Utm.of_latlon Geo.boulogne
               
  let home = champs
  let work = defense
                           
  let stat_strong = Grid.center
  let stat_weak = Utm.of_latlon (Wgs.make 48.6983 2.1876) (* country *)
  let stat_length = Grid.step *. 100.  (* step * number of cells = meters*)
end


let to_string () = 
  let spt p = Utm.string_of p in

  Printf.sprintf "
Grid:
center:  %s %s
length:  %i
step:    %.0f m
r_city:  %.0f m
r_cntry: %.0f m

Algo:
stop:    %.0f%%
pp*:     %.0f
r*:      log %.0f
d_max:   log %.0f

strong box
center:  %s
weak box
center:  %s
length:  %.0f

strong:  %s
weak:    %s
"
      "Paris" (spt Grid.center) Grid.length Grid.step Grid.r_city Grid.r_country 
      Algo.stop_percent Algo.pp_star (exp Algo.r_star) (exp Algo.max_distance)
      (spt Evaluation.stat_strong) (spt Evaluation.stat_weak) (Evaluation.stat_length /. Grid.step)
      (spt Evaluation.strong) (spt Evaluation.weak)
