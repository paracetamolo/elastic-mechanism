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
    (* Grid.to_geojson grid (basename^".json"); *)
    grid
  in

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
      Grid.to_file grid_carved (basename^".dump");
      (* Grid.to_geojson grid_carved (basename^"-carved.json"); *)
      grid_carved
    else make_from_scratch ()
  else make_from_scratch ()





let make_mappa grid =
  let name = "mappa" in
 
  if Util.file_exists (name^"-nodes.dump")
  then Elastic.of_files name
  else
    let _ = Printf.printf "Normalizing grid:\n%!" in
    let city_center = Grid.get grid (Grid.find grid Conf.Grid.center) in
    let grid = Grid.normalize grid city_center in
    Printf.printf "%s\n\n%!" (Util.string_of_values (Grid.stat grid));
    (* Grid.to_geojson grid (name^".json"); *)

    Printf.printf "creating map\n%!";
    let fences = List.map (Grid.diag_of_box grid) Conf.Grid.fences in
    (* let fences = [] in *)
    let m = Elastic.make grid Conf.Algo.requirement fences in
    Elastic.to_files m name;
    m




let stat_mappa m box =

  let d,c = Elastic.stat m in

  Printf.printf "Edges      %s\n%!" (Util.string_of_values d);
  Printf.printf "Degrees    %s\n%!" (Util.string_of_values c);
  ()



let mechanism_stat_boxes mec = 
  (* TODO there is a memory problem *)
  let module EI = ElasticInfo in
  let module LI = Laplacian_info in
  
  let g = Mechanism.get_grid mec in
  let m = Mechanism.get_metric mec in
  let strong_box = Grid.diag_of_box g (Central (Conf.Evaluation.stat_strong, Conf.Evaluation.stat_length)) in
  let weak_box   = Grid.diag_of_box g (Central (Conf.Evaluation.stat_weak,   Conf.Evaluation.stat_length)) in
  Grid.to_geojson_box g strong_box "box-strong.json";
  Grid.to_geojson_box g weak_box   "box-weak.json";

  let err_strong = Mechanism.dump_avg_err_box mec strong_box "strong-mappa-err.dat" in
  let err_weak   = Mechanism.dump_avg_err_box mec weak_box   "weak-mappa-err.dat" in
  let open Util in
  Printf.printf "Avg Error Strong box %s\n%!" (Util.string_of_values err_strong);
  Printf.printf "Avg Error Weak   box %s\n%!" (Util.string_of_values err_weak);
  Gc.compact ();

  let dump n file =
    let oc = open_out file in
    Printf.fprintf oc "%+e\n" n;
    close_out oc
  in
  dump err_strong.avg "laplas-err.dat";
  dump err_weak.avg   "laplaw-err.dat";

  Gc.compact ();

  let ls = Laplacian.make_of_avg_err g err_strong.avg in
  let lw = Laplacian.make_of_avg_err g err_weak.avg in

  let r = Conf.Algo.r_star in
  let logr = exp r in
  
    Printf.printf "
Epsilon:
strong  %f
weak    %f
Radius: %f
%!" (Laplacian.eps ls) (Laplacian.eps lw) r;

  let _ = EI.ball_stat_box m  strong_box r (Printf.sprintf "strong-mappa-pp-ln%.0f.dat" logr) in
  let _ = EI.ball_stat_box m  weak_box   r (Printf.sprintf "weak-mappa-pp-ln%.0f.dat"   logr) in

  let _ = LI.ball_stat_box ls strong_box r (Printf.sprintf "strong-laplas-pp-ln%.0f.dat" logr) in
  let _ = LI.ball_stat_box ls weak_box   r (Printf.sprintf "weak-laplas-pp-ln%.0f.dat"   logr) in

  let _ = LI.ball_stat_box lw strong_box r (Printf.sprintf "strong-laplaw-pp-ln%.0f.dat" logr) in
  let _ = LI.ball_stat_box lw weak_box   r (Printf.sprintf "weak-laplaw-pp-ln%.0f.dat"   logr) in

  let _ = EI.geojson_of_pp_box m  strong_box r (Printf.sprintf "strong-mappa-pp-ln%.0f.json" logr) in
  let _ = EI.geojson_of_pp_box m  weak_box   r (Printf.sprintf "weak-mappa-pp-ln%.0f.json"   logr) in

  let _ = LI.geojson_of_pp_box ls strong_box r (Printf.sprintf "strong-laplas-pp-ln%.0f.json" logr) in
  let _ = LI.geojson_of_pp_box ls weak_box   r (Printf.sprintf "weak-laplas-pp-ln%.0f.json"   logr) in

  let _ = LI.geojson_of_pp_box lw strong_box r (Printf.sprintf "strong-laplaw-pp-ln%.0f.json" logr) in
  let _ = LI.geojson_of_pp_box lw weak_box   r (Printf.sprintf "weak-laplaw-pp-ln%.0f.json"   logr) in

  ()

    


let mechanism_stat_points mec = 

  let module EI = ElasticInfo in
  let module LI = Laplacian_info in
  
  let g = Mechanism.get_grid mec in
  let m = Mechanism.get_metric mec in

  Util.plot_function Conf.Algo.requirement "requirement.dat";

  let weak   = Grid.id_of_pos g (Grid.find g Conf.Evaluation.weak) in
  let strong = Grid.id_of_pos g (Grid.find g Conf.Evaluation.strong) in

  let err_weak = Mechanism.avg_error mec weak in
  let err_strong = Mechanism.avg_error mec strong in

  let eu_w = Laplacian.make_of_avg_err g err_weak in
  let eu_s = Laplacian.make_of_avg_err g err_strong in


  let id = weak in
  Printf.printf "Weak: %i\n%!" id;
  Printf.printf "Mappa   avg error in %6i: %f\n%!" id err_weak;
  Printf.printf "Lapla-w avg error in %6i: %f  eps: %f\n%!" id err_weak (Laplacian.eps eu_w);
  Printf.printf "Lapla-s avg error in %6i: %f  eps: %f\n%!" id err_strong (Laplacian.eps eu_s);
  EI.plot m id "mappa-weak.dat";
  LI.plot eu_w id "lapla-weak-weak.dat";
  LI.plot eu_s id "lapla-strong-weak.dat";

  Gc.compact ();

  let id = strong in
  Printf.printf "Strong: %i\n" id;
  Printf.printf "Mappa   avg error in %6i: %f\n%!" id err_strong;
  Printf.printf "Lapla-w avg error in %6i: %f  eps: %f\n%!" id err_weak (Laplacian.eps eu_w);
  Printf.printf "Lapla-s avg error in %6i: %f  eps: %f\n%!" id err_strong (Laplacian.eps eu_s);
  EI.plot m id "mappa-strong.dat";
  LI.plot eu_w id "lapla-weak-strong.dat";
  LI.plot eu_s id "lapla-strong-strong.dat";
  ()
  (* 
mappa-weak.dat
lapla-weak-weak.dat
lapla-strong-weak.dat

mappa-strong.dat
lapla-weak-strong.dat
lapla-strong-strong.dat
 *)

let fences mec = 
  let g = Mechanism.get_grid mec in
  let m = Mechanism.get_metric mec in
  let weak   = Grid.id_of_pos g (Grid.find g Conf.Evaluation.weak) in
  let strong = Grid.id_of_pos g (Grid.find g Conf.Evaluation.strong) in

  let border_points g box = 
    let fence_in,_ = box in
    let fence_out = fence_in-1 in
    fence_in,fence_out
  in
  let fences_couples = List.mapi 
                             (fun i b -> 
                              Printf.printf "Doing fence %i\n\n%!" i;
                              let fi,fo = border_points g b in
                              [(fi,Printf.sprintf "fencein%i" i);
                               (fo,Printf.sprintf "fenceout%i" i)])
                             (Elastic.get_boxes m) 
  in
  let fences = List.flatten fences_couples in

  let json_pdf (id,tag) = 
    Printf.printf "Doing pdf %i\n\n%!" id;
    Formats.geojson_to_file (Mechanism.geojson_of_pdf mec id)
                            (Printf.sprintf "mappa-pdf-%s-%06i.json" tag id);
  in
  List.iter json_pdf ([(weak,"weak"); (strong,"strong");]@fences) ;

  ()


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
  Printf.printf "Raw grid:\n%s\n\n" (Util.string_of_values (Grid.stat g));
  Printf.printf "\nTime to make grid: %!";
  let t = Util.print_time t in

  let m = make_mappa g in
  Printf.printf "\nTime to make mappa: %!";
  let t = Util.print_time t in

  Printf.printf "\nStat box size %i\n" (int_of_float (Conf.Evaluation.stat_length /. Conf.Grid.step));
  let stat_box = Grid.diag_of_box g (Central (Conf.Evaluation.stat_strong, Conf.Evaluation.stat_length)) in
  Grid.to_geojson_box g stat_box "stat-not-normalized.json";
  Grid.to_geojson_box (Elastic.get_grid m) stat_box "box.json";
  Printf.printf "\nTime for geojson stat box: %!";
  let t = Util.print_time t in

  stat_mappa m stat_box;
  Printf.printf "\nTime for stat mappa: %!";
  let t = Util.print_time t in

  let mec = Mechanism.make m in
  
  mechanism_stat_boxes mec;
  Printf.printf "\nTime for mechanism general statistics: %!";
  let _ = Util.print_time t in

  mechanism_stat_points mec;
  Printf.printf "\nTime for mechanism points statistics: %!";
  let t = Util.print_time t in

  fences mec;
  Printf.printf "\nTime for fences: %!";
  let _ = Util.print_time t in

  ()
