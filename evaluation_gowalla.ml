(* Copyright (C) 2014, Marco Stronati, All Rights Reserved.
   This file is distributed under the terms of the
   GNU General Public License version 3 or later. *)

(** 

Collection of functions used to evaluate the elastic mechanism with the Gowalla dataset.
This code was used in conjunction with libqif. 

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
  then
    let _ = Printf.printf "Using cached map\n%!" in
    Elastic.of_files name
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






let _ = 

  let argv = Sys.argv in
  if (Array.length argv) = 0 then failwith "No arguments";

  let dir = Util.deslash argv.(1) in
  (try Unix.mkdir dir 0o755; with Unix.Unix_error (Unix.EEXIST,_,_) -> (););
  Sys.chdir dir;
  Printf.printf "Working dir: %s\n" dir;
  Printf.printf "%s\n" (Conf.to_string ());

  let t = Util.get_time () in
  let g_raw = make_grid () in       (* this is the unnormalized grid *)
  Printf.printf "\nTime to make grid: %!";
  let t = Util.print_time t in

  let l = Grid.length g_raw in
  let nw = Grid.get g_raw (0,0) in
  let se = Grid.get g_raw (l-1,l-1) in

  Printf.printf "nw: %s  se: %s\n" 
                (Geo.Wgs.string_of (Wgs.of_xy (Node.coord nw)))
                (Geo.Wgs.string_of (Wgs.of_xy (Node.coord se)));



  let m = make_mappa g_raw in
  let g = Elastic.get_grid m in (* this is the normalized grid *)
  let mec = Mechanism.make m in
  Printf.printf "\nTime to mechanism mappa: %!";
  let t = Util.print_time t in


  (* (\* Convert gowalla dataset ids to the elastic grid ids. *)
  (*    Outputs a list of couples (gowalla_id, grid_id) *)
  (*  *\) *)
  (* let convert_gowalla () = *)
  (*   let open Batteries in *)
  (*   let filename = "brightkite_to_convert" in *)
  (*   let filelines = File.lines_of filename in *)
  (*   let oc = open_out "brightkite_converted" in *)
  (*   Enum.iteri (fun _ line -> *)
  (*     let _ = Scanf.sscanf line "%s@, %f, %f" *)
  (*       (fun gow_id lat lon -> *)
  (*         let coord = Utm.of_latlon (Wgs.make lat lon) in *)
  (*         try *)
  (*           let id = Grid.id_of_pos g (Grid.find g coord) in *)
  (*           Printf.fprintf oc "%s,%i\n" gow_id id *)
  (*         with Not_found -> ()) in ()) *)
  (*   filelines; *)
  (*   close_out oc; *)
  (* in *)
  (* convert_gowalla (); *)


  (* Outputs one raw with the weight of each location in the grid*)
  let oc = open_out "location_weights.dat" in
  Grid.iter (Elastic.get_grid m) (
              fun n ->
              Printf.fprintf oc "%e " (Node.weight n);
            );
  Printf.fprintf oc "\n";
  close_out oc;



  let city    = Grid.diag_of_box g (Central (Conf.Evaluation.stat_strong, Conf.Evaluation.stat_length)) in
  let _ = Printf.printf "city    box: %i %i\n%!" (fst city) (snd city) in

  (* outputs a matrix with distance between each pair of locations *)
  Elastic.to_matrix_box m city ("metric-city.dat");

  Gc.compact ();

  let country_center = Utm.of_latlon (Wgs.make 48.900669 2.208238) in
  let country = Grid.diag_of_box g (Central (country_center, Conf.Evaluation.stat_length)) in
  let _ = Printf.printf "country box: %i %i\n%!" (fst country) (snd country) in

  (* outputs a matrix with distance between each pair of locations *)
  Elastic.to_matrix_box m country ("metric-country.dat");


  (* Computes the maximum weight of both city and country so to normalize uniformly the geojson *)
  Gc.compact ();
  let tmp = ref (-.1.) in
  Grid.iter_box g city (fun n -> tmp := max !tmp (Node.weight n));
  let max_city = !tmp in

  let tmp = ref (-.1.) in
  Grid.iter_box g country (fun n -> tmp := max !tmp (Node.weight n));
  let max_country = !tmp in

  let max_both = max max_city max_country in
  Grid.to_geojson_box g city    ~max_weight:max_both "city_both.json";
  Grid.to_geojson_box g country ~max_weight:max_both "country_both_light.json";


  (* Dumps the average error of each location in a raw .dat file and geojson, and outputs some global statistics. *)
  Gc.compact ();
  let open Util in
  let avgerr_city = Mechanism.dump_avg_err_box mec city "elastic-avgerr-city" in
  Printf.printf "Avg Error City %s\n%!" (Util.string_of_values avgerr_city);
  Gc.compact ();
  let avgerr_country = Mechanism.dump_avg_err_box mec country "elastic-avgerr-country" in
  Printf.printf "Avg Error Country %s\n%!" (Util.string_of_values avgerr_country);

  (* Same as above but city and country are normalized uniformly *)
  Gc.compact ();
  let max_both = max avgerr_city.max avgerr_country.max in
  let _ = Mechanism.dump_avg_err_box mec city    ~max_error:max_both "elastic-avgerr-city_both" in
  Gc.compact ();
  let _ = Mechanism.dump_avg_err_box mec country ~max_error:max_both "elastic-avgerr-country_both" in



  let _ = t in
  ()
