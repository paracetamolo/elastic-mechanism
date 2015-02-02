(* Copyright (C) 2014, Marco Stronati, All Rights Reserved.
   This file is distributed under the terms of the
   GNU General Public License version 3 or later. *)

(** 
Evaluate the elastic mechanism.

In this module we build a grid, an elastic mechism, compute statistics and
a comparison with several planar laplace mechanisms. 
*)

open Geo
open Grid
open Metric
open Mechanism


let make_grid () =
  let basename = "merge" in

  let make_from_scratch () =
    let _ = Printf.printf "Making grid from scratch  size %i\n%!" Conf.Grid.length in
    let grid = Grid.make Conf.Grid.center Conf.Grid.step Conf.Grid.length in
    Grid.to_file grid (basename^".dump");
    grid
  in

  let grid = 
    if Util.file_exists (basename^".dump") 
    then 
      let grid_orig = Grid.of_file (basename^".dump") in
      let l_orig = Grid.length grid_orig in
      if l_orig = Conf.Grid.length
      then 
        let _ = Printf.printf "Using cached grid  size %i\n%!" Conf.Grid.length in
        grid_orig
      else if l_orig > Conf.Grid.length
      then
        let _ = Printf.printf "Carving grid  size %i\n%!" Conf.Grid.length in
        let box = Grid.diag_of_box grid_orig (Central (Conf.Grid.center,Conf.Grid.step *. (float Conf.Grid.length))) in
        let grid_carved = Grid.carve grid_orig box in
        grid_carved
      else make_from_scratch ()
    else make_from_scratch ()
  in
    
  Grid.to_geojson grid ("grid-raw.json");
  Printf.printf "Raw grid:\n%s\n\n%!" (Util.string_of_values (Grid.stat grid));
  let grid = Grid.normalize grid in
  Printf.printf "Normalized grid:\n%s\n\n%!" (Util.string_of_values (Grid.stat grid));
  Grid.to_geojson grid ("grid-norm.json");
  grid



let make_mappa grid =
  let name = "mappa" in
 
  if Util.file_exists (name^"-nodes.dump")
  then Elastic.of_files name
  else
    let _ = Printf.printf "creating map\n%!" in
    let fences = List.map (Grid.diag_of_box grid) Conf.Grid.fences in
    (* let fences = [] in *)
    let m = Elastic.make grid Conf.Algo.requirement fences in
    Elastic.to_files m name;

    let d,c = Elastic.stat m in
    
    Printf.printf "Edges      %s\n%!" (Util.string_of_values d);
    Printf.printf "Degrees    %s\n%!" (Util.string_of_values c);

    m




let _ = 

  let argv = Sys.argv in
  if (Array.length argv) = 0 then failwith "No arguments";

  let dir = Util.deslash argv.(1) in
  (try Unix.mkdir dir 0o755; with Unix.Unix_error (Unix.EEXIST,_,_) -> (););
  Sys.chdir dir;
  Printf.printf "Working dir: %s\n" dir;
  Printf.printf "%s\n" (Conf.to_string ());

  let t = Util.get_time () in
  let g = make_grid () in
  Printf.printf "\nTime to make grid: %!";
  let t = Util.print_time t in

  let m = make_mappa g in
  Printf.printf "\nTime to make mappa: %!";
  let t = Util.print_time t in


  (* outputs one raw will the weight of each location *)
  let oc = open_out "location_weights.dat" in
  Grid.iter (Elastic.get_grid m) (
              fun n ->
              Printf.fprintf oc "%e " (Node.weight n);
            );
  Printf.fprintf oc "\n";
  close_out oc;
            

  (* outputs a metrix with distance between each pair of locations *)
  Elastic.to_matrix m ("metric.dat");
  Printf.printf "\nTime for edges to matrix: %!";
  let _ = Util.print_time t in


  (* output a line for each user's prior, removing locations w/o weight *)
  (* expects a directory 'geolife' in the same path *)
  let open Formats in
  let dir = "geolife" in
  let users = Sys.readdir dir in
  let _ = Array.sort compare users in
  let oc = open_out "priors.dump" in
  let counts = Grid.copy g in
  let min_w = let open Util in (Grid.stat g).min in

  let count_track trackname =
    (* Printf.printf " %s\n%!" trackname; *)
    (try
      let track = Formats.track_of_plt trackname in
        List.iter (fun p ->
                   try
                   let n = Grid.get counts (Grid.find counts p.coord) in
                   Node.set_weight n ((Node.weight n) +. 1.)
                 with
                   Not_found -> ()
                ) track
    with _ -> Printf.printf "\nTroubles with track %s\n%!" trackname);
  in
  
  let dump_user user =
    Printf.printf "%s %!" user;
    let _ = Grid.iter counts (fun n -> Node.set_weight n 0.) in
    let tracks = Sys.readdir (dir^"/"^user^"/Trajectory") in
    let tracks = Array.map (fun relat -> dir^"/"^user^"/Trajectory/"^relat) tracks in
    Array.iter (count_track) tracks;
    (* remove from counts locations w/o pois and find max*)
    let sum_w = ref (-1.) in
    Grid.iter counts (fun n ->
                      let w =
                        if Node.weight (Grid.get_id g (Node.id n)) > min_w
                        then Node.weight n
                        else 0.
                      in
                      Node.set_weight n w;
                      sum_w := !sum_w +. w
                     );
    (* normalize to prob distibution and dump *)
    if !sum_w = 0. then Printf.printf "%s is empty\n" user;
    Grid.iter counts (fun n ->
                      let w =
                        if !sum_w > 0.
                        then (Node.weight n) /. !sum_w
                        else 0.
                      in
                      Node.set_weight n w;
                      Printf.fprintf oc "%e " w
                     );
    Printf.fprintf oc "\n";
    Grid.to_geojson counts ("prior-"^user^".json");
  in
  
  Array.iter (dump_user) users;
  close_out oc;

  Printf.printf "\nTime for prior: %!";
  let _ = Util.print_time t in
  ()
