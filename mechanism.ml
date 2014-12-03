(* Copyright (C) 2014, Marco Stronati, All Rights Reserved.
   This file is distributed under the terms of the
   GNU General Public License version 3 or later. *)

(**
Privacy mechanisms.
 *)

open Geo
open Grid
open Metric
  
let _ = Random.self_init ()

(**
Exponential mechanism built from a metric
 *)
module type Exponential = sig
  type t
  type metric
  val make : metric -> t
  val clean_cache_box : t -> Grid.box_in -> unit
  val clean_cache : t -> unit
  val get_grid : t -> Grid.t
  val get_metric : t -> metric
  val avg_error : t -> int -> float
  val dump_avg_err_box : t -> Grid.box_in -> string -> Util.values
  val dump_avg_err : t -> string -> Util.values
  val geojson_of_pdf : t -> int -> string
  end
                        
(**
Functor to build Exponential mechanism from a metric
 *)
module Make_mechanism (Metric : Metric) : (Exponential with type metric := Metric.t) = struct

  type t = {
    metric : Metric.t;
    (* pdfs : (int * float) list array array; *)
  }

  let make metric =
    (* let l = Grid.length (Metric.get_grid metric) in *)
    {
      metric = metric;
      (* pdfs = Array.make_matrix l l [] *)
    }

  let clean_cache_box mec box =
    let g = Metric.get_grid mec.metric in
    Grid.iter_box g box
                  (fun n ->
                   (* let x,y = Grid.pos_of_id g (Node.id n) in *)
                   (* mec.pdfs.(x).(y) <- [] *)
                   ()
                  )
      
  let clean_cache mec =
    let l = Grid.length (Metric.get_grid mec.metric) in
    clean_cache_box mec (0,l*l-1)

                
  let get_grid mec = Metric.get_grid mec.metric
  let get_metric mec = mec.metric

  (* a pdf is a list of (id,probability) *)
  let compute_pdf mec id =
    (* let x,y = Grid.pos_of_id (Metric.get_grid mec.metric) id in *)
    (* let cached_pdf = mec.pdfs.(x).(y) in *)
    (* if cached_pdf <> [] then cached_pdf else *)
    let distances = Metric.distances_from mec.metric id in
    let distances = List.map (fun (i,d) -> (i,d *. 0.5)) distances in (* TODO clean the 2*)
    let lambda_recip = List.fold_left (fun tmp (_,d) ->
      let v = exp (-.d) in
      (* Printf.printf "tmp %e     d %e      v %e\n" (tmp +. v) d v; *)
      tmp +. v) 0. distances
    in
    (* Printf.printf "Distanze : %i   lambda_recip %f\n" (List.length distances) lambda_recip; *)
    if BatFloat.equal lambda_recip 0. then failwith "uncomputable lambda";
    let res = List.map (fun (n,d) ->
                        let v = ((1. /. lambda_recip) *. (exp (-. d))) in
                        (n,v))
                       distances
    in
    (* mec.pdfs.(x).(y) <- res; *)
    res

  let compute_all_pdfs mec =
    let g = Metric.get_grid mec.metric in
    Grid.iter g (fun n -> let _ = compute_pdf mec (Node.id n) in ())

  let avg_error mec id =
    let g = Metric.get_grid mec.metric in
    let pdf = compute_pdf mec id in
    let err_probs = List.map (fun (id2,p) ->
                              let coo1 = Node.coord (Grid.get_id g id) in
                              let coo2 = Node.coord (Grid.get_id g id2) in
                              (Utm.distance coo1 coo2,p))
                             pdf
    in
    List.fold_left (fun tmp (e,p) -> tmp +. (e *. p)) 0. err_probs

  (* O(2n) *)
  let draw map coord_s =
    let pdf = compute_pdf map coord_s in
    let rnd = Random.float 1. in
    Printf.printf "rnd %f \n"rnd;
    (* this could be done with a binary tree if the distances are a lot *)
    let rec loop sum l =
      match l with
        (n,v)::rest ->
          let cumulative = sum +. v in
          Printf.printf "v %e  cum %e\n" v cumulative;
          if rnd <= cumulative
          then n
          else loop cumulative rest
      | [] -> failwith (Printf.sprintf "draw_laplacian: rnd %f sum %f" rnd sum)
    in
    loop 0. pdf


  let sanitize map coord_s = draw map coord_s

                                  
  let geojson_of_pdf mec pos =
    let g = get_grid mec in
    let pdf = compute_pdf mec pos in
    (* Printf.printf "pdf length : %i\n" (List.length pdf); *)
    let vmax = List.fold_left (fun tmp (n,v) ->
                              if v > tmp then v else tmp)
                             (-1.) pdf
    in
    let nodes_string = List.fold_left (fun tmp (n,v) ->
        (* let _ = Printf.printf "v:%e " v in *)
        (* let prop = Printf.sprintf "\"pdf\" : %f,\n\"opacity\" : %f," v v in *)
        let prop = Printf.sprintf "\"pdf\" : %f,\n\"pdf_n\" : %f, " v (v /. vmax) in
        let radius = Grid.radius g in
        let node = Node.geojson_of ~properties:prop radius (Grid.get_id g n) in
        Printf.sprintf "%s%s,\n" tmp node)
      "" pdf
    in
    (* remove last ,\n *)
    let cleaned = String.sub nodes_string 0 (String.length nodes_string -2) in
    Printf.sprintf
      "{ \"type\": \"FeatureCollection\",
         \"features\": [\
          %s
          ]
       }" cleaned



  let dump_avg_err_box mec box file =
    let g = get_grid mec in
    let l = ref [] in
    let emax = ref (-1.) in
    let oc = open_out (file^".dat") in
    Printf.fprintf oc "#id\tavg_err\n";
    Grid.iter_box g box (fun n ->
                    let id = Node.id n in
                    let err = avg_error mec id in
                    emax := max err !emax;
                    (* let x,y = Grid.pos_of_id g id in *)
                    (* Printf.printf "%i %!" (List.length mec.pdfs.(x).(y)); *)
                    (* mec.pdfs.(x).(y) <- [];                 (\* TODO!!!!!! *\) *)
                    Printf.fprintf oc "%09i %f\n" id err;
                    l := (id, err)::!l);
    let nodes_string = List.fold_left (fun tmp (n,v) ->
        (* let prop = Printf.sprintf "\"pdf\" : %f,\n\"opacity\" : %f," v (v /. !max) in *)
        let prop = Printf.sprintf "\"err\" : %f,\n\"err_n\" : %f, " v (v /. !emax) in
        let radius = Grid.radius g in
        let node = Node.geojson_of ~properties:prop radius (Grid.get_id g n) in
        Printf.sprintf "%s%s,\n" tmp node)
      "" !l
    in
    (* remove last ,\n *)
    let cleaned = String.sub nodes_string 0 (String.length nodes_string -2) in
    let s = Printf.sprintf
      "{ \"type\": \"FeatureCollection\",
         \"features\": [\
          %s
          ]
       }" cleaned
    in
    Formats.geojson_to_file s (file^".json");
    close_out oc;
    Util.stat (snd (List.split !l)) (* TODO this could be sumplified with a nice fold in grid *)
    

  let dump_avg_err mec file = 
    let l = Grid.length (get_grid mec) in
    dump_avg_err_box mec (0,l*l-1) file

end


                                                                     

(**
Planar Laplace mechanism
 *)
module Planar_laplace = struct

  type t = {
    epsilon : float;
    grid : Grid.t;
  }

  let make grid eps =
    {
      epsilon = eps;
      grid = grid;
    }

  let make_of_avg_err g err =
    let eps = Laplacian.radius_of_expected_value_polar err in
    make g eps

  let eps l = l.epsilon

  let get_grid l = l.grid

  let distances_from l c_id = [] (* TODO implement? *)

  let rec pp_of_ball l c_id r =
    Grid.pp_of_ball l.grid c_id (r /. l.epsilon)

end


  
(* module Laplacian = Make_mechanism (Euclidean) *)
module Laplacian = Planar_laplace
(**
Laplacian with info
*)
module Laplacian_info = Metric.Make_info (Planar_laplace)
                     
(**
Elastic Mechanism
*)
module Mechanism = Make_mechanism (Elastic)
