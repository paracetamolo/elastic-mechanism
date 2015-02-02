(* Copyright (C) 2014, Marco Stronati, All Rights Reserved.
   This file is distributed under the terms of the
   GNU General Public License version 3 or later. *)

(**
Distinguishability metrics.
  *)

open Geo
open Grid
       

(**
Basic Metric type
 *)
module type Metric = sig
    type t
    val get_grid : t -> Grid.t
    val distances_from: t -> int -> (int * float) list
    val pp_of_ball : t -> int -> float -> float
end

(**
Informative functions on metrics
 *)
module type Info = sig
    type t
    val plot : t -> int  -> string -> unit
    val geojson_of_pp_box : t -> Grid.box_in -> float -> string -> unit
    val geojson_of_pp : t -> float -> string -> unit
    val ball_stat_box : t -> Grid.box_in -> float -> string -> Util.values
    val ball_stat : t -> float -> string -> Util.values
  end

(**
Functor to build an Informative from a metric
 *)
module Make_info (Metric: Metric) : (Info with type t := Metric.t) = struct
            
  let plot l p_id name =
    let f r =
      Metric.pp_of_ball l p_id r
    in
    Util.plot_function f name


  let geojson_of_pp_box l box r file =
    let grid = Metric.get_grid l in
    let pps = ref [] in
    let max = ref (-1.) in
    Grid.iter_box (Metric.get_grid l) box 
                  (fun n ->
                    let id = Node.id n in
                    let pp = Metric.pp_of_ball l id r in
                    max := if !max >= pp then !max else pp;
                    pps := (id, pp)::!pps);
    let nodes_string = List.fold_left (fun tmp (n,v) ->
        let prop = Printf.sprintf "\"pp\" : %f," (v /. !max) in
        let radius = Grid.radius grid in
        let node = Node.geojson_of ~properties:prop radius (Grid.get_id grid n) in
        Printf.sprintf "%s%s,\n" tmp node)
      "" !pps
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
    Formats.geojson_to_file s file


  let geojson_of_pp lap r file =
    let g = Metric.get_grid lap in
    geojson_of_pp_box lap (Grid.full_box g) r file


  let ball_stat_box lap box r file =

    let l = ref [] in
    let oc = open_out file in
    Printf.fprintf oc "#id       ball-ln%.0f\n" (log r);
    Grid.iter_box (Metric.get_grid lap) box
         (fun n ->
          let v = Metric.pp_of_ball lap (Node.id n) r in
          Printf.fprintf oc "%09i %f\n" (Node.id n) v ;
          l := v::!l;
         );
    close_out oc;
    let d = Util.stat !l in
    d

  let ball_stat lap = 
    let g = Metric.get_grid lap in
    ball_stat_box lap (Grid.full_box g)
end

                       

(**
Elastic metric
  *)
module Elastic : sig
  type t 
  include Metric with type t := t
  val make: Grid.t -> (float -> float) -> Grid.box_in list -> t
  val get_boxes: t -> Grid.box_in list
  val get_id: t -> int -> Node.t
  val iter: t -> (Node.t -> unit) -> unit
  val iter_box: t -> Grid.box_in -> (Node.t -> unit) -> unit
  val to_files: t -> string -> unit
  (** Output a file with a symmetric square matrix containing the distance of each pair of nodes. *)
  val to_matrix: t -> string -> unit
  val of_files: string -> t
  val stat: t -> Util.values * Util.values
  val stat_box: t -> Grid.box_in -> Util.values * Util.values
end
  = struct

    module E = struct
      type t = float
      let compare = Util.my_float_compare
      let default = 0.0
    end
    module S = Graph.Imperative.Graph.ConcreteLabeled(Node)(E)
    module G = Grid

    type t = {
      grid : G.t;
      graph: S.t;
      boxes: Grid.box_in list
  }

  let get_grid m = m.grid



  let ball_cached visited m center_id r =
    let module T = HashedHeap.HashedHeap in

    let to_visit = T.create () in

    let add_neighbors (id,d) =
      let n = Grid.get_id m.grid id in
      S.iter_succ_e (fun e -> 
                     let n2 = S.E.dst e in
                     let d2 = d +. (S.E.label e) in
                     if (not (BatHashtbl.mem visited (Node.id n2))) && (d2 <= r)
                     then T.add to_visit (Node.id n2,d2))
                    m.graph n
    in
    let mark_visited (id,d) = if BatHashtbl.mem visited id then failwith "already visited" else BatHashtbl.replace visited id d in

    if BatHashtbl.length visited = 0
    then T.add to_visit (center_id,0.)
    else BatHashtbl.iter (fun id d -> add_neighbors (id,d)) visited;
      
    if T.is_empty to_visit 
    then BatHashtbl.filter (fun d2 -> d2 <= r) visited
    else
      let rec loop () =
        if T.is_empty to_visit
        then visited
        else 
          let (id,d) = T.pop to_visit in
          if BatHashtbl.mem visited id
          then loop ()
          else
            (add_neighbors (id,d);
             mark_visited (id,d);
             loop ())
      in
      loop ()

             
  let ball m id r = 
    let visited = BatHashtbl.create 10 in
    ball_cached visited m id r


  let distances_from m id =
    let threshold = Conf.Algo.max_distance *. 2. in (* TODO incrase this 2 *)
    let b = ball m id threshold in
    BatList.of_enum (BatHashtbl.enum b)


  let pp_of_ball m center_id r =
    let b = ball m center_id r in
    BatHashtbl.fold (fun id d tmp -> let n = Grid.get_id m.grid id in
                                         (Node.weight n) +. tmp) b 0.


  let get_boxes m = m.boxes

  let radius m =
    Grid.radius m.grid

  let iter m f =
    Grid.iter m.grid f

  let iter_box m box f =
    Grid.iter_box m.grid box f

  let length m =
    Grid.length m.grid

  let get_id m id =
    Grid.get_id m.grid id

  let iter m f = Grid.iter m.grid f

  let add_verteces graph grid =
    G.iter grid (fun n -> S.add_vertex graph n)


  let dump_dot m file =
    let g = m.graph in
    let module Dot = Graph.Graphviz.Dot(
      struct
        include Graph.Imperative.Graph.ConcreteLabeled(Node)(E)
        let edge_attributes (a,e,b) = [`Label (string_of_float e); `Color 4711]
        let default_edge_attributes _ = [`Dir `None]
        let get_subgraph _ = None
        let vertex_attributes v = [`Shape `Box]
        let vertex_name v = (Printf.sprintf "\"%i w:%.0f\"" (Node.id v) (Node.weight v))
        let default_vertex_attributes _ = []
        let graph_attributes _ = []
      end)
    in
    let oc = Pervasives.open_out_bin file in
    Dot.output_graph oc g;
    Pervasives.close_out oc



  (* save to file *)
  (* graph is undirected, edges are saved more that once *)
  let to_file_edges m file =
    let oc = open_out file in
    let dump_edges v1 =
      S.iter_succ_e (fun e ->
                    let v2 = S.E.dst e in
                    Printf.fprintf oc "%05i,%+e,%05i\n" (Node.id v1) (S.E.label e) (Node.id v2);
                   ) m.graph v1
    in
    Grid.iter m.grid dump_edges;
    close_out oc

  let to_matrix m file = 
    let oc = open_out file in

    Grid.iter m.grid 
              (fun n1 ->

               (* Printf.fprintf oc "%05i: %!" (Node.id n1); (\* TODO to remove *\) *)
               let threshold = Conf.Algo.max_distance *. 2. in (* TODO incrase this 2 *)
               let dists = ball m (Node.id n1) threshold in
               Grid.iter m.grid (fun n2 -> 
                                 let id2 = Node.id n2 in
                                 let d =
                                   try Hashtbl.find dists id2 with 
                                     Not_found -> infinity
                                 in
                                 Printf.fprintf oc "%+e " d;
                                );
               Printf.fprintf oc "\n");
    close_out oc
    


  let to_files m name =
    Grid.to_file m.grid (name^"-nodes.dump");
    to_file_edges m (name^"-edges.dump");
    Grid.to_file_boxes m.grid m.boxes (name^"-boxes.dump")


  let of_files name =
    let nodes_file = name^"-nodes.dump" in
    let edges_file = name^"-edges.dump" in
    let boxes_file = name^"-boxes.dump" in

    let grid = Grid.of_file nodes_file in

    let graph = S.create () in
    add_verteces graph grid;

    let ic = Pervasives.open_in edges_file in

    let rec loop () =
      let maybe_s =
        try
          Some (Pervasives.input_line ic)
        with End_of_file -> None
      in
      match maybe_s with
      | Some s ->
          let _ = Scanf.sscanf s "%i,%f,%i"
            (fun id1 weight id2->
              let n1 = Grid.get_id grid id1 in
              let n2 = Grid.get_id grid id2 in
              S.add_edge_e graph (n1,weight,n2))
          in
          loop ()
      | None -> Pervasives.close_in ic
    in
    loop ();
    let boxes = Grid.of_file_boxes grid boxes_file in
    {grid = grid; graph = graph; boxes = boxes}



  let make grid requirement boxes =

    let graph = S.create () in
    add_verteces graph grid;

    let radii = Array.make_matrix (Grid.length grid) (Grid.length grid) 0. in
    let get_radius_of_id id = let (x,y) = Grid.pos_of_id grid id in radii.(x).(y) in
    let set_radius_of_id id r = let (x,y) = Grid.pos_of_id grid id in radii.(x).(y) <- r in

    let iters =
      let empty = Array.make_matrix (Grid.length grid) (Grid.length grid) None in
      Grid.iter grid (fun n ->
                      let id = Node.id n in
                      let x,y = Grid.pos_of_id grid id in
                      empty.(x).(y) <- Some (id,-1));
      empty
    in
    let get_next_of_id c_id = 
      let (x,y) = Grid.pos_of_id grid c_id in
      let old_state = iters.(x).(y) in
      match old_state with
        None -> None
      | Some old_state ->
         let new_state = Grid.next_by_distance grid c_id old_state in
         iters.(x).(y) <- Some new_state;
         Some (fst new_state)
    in
    let clear_next_of_id id = 
      let (x,y) = Grid.pos_of_id grid id in 
      iters.(x).(y) <- None
    in


    (* Connect the interior of the boxes with zero edges, in a E shape. *)
    List.iter (fun (nw,se) ->
               let nw_x,nw_y = Grid.pos_of_id grid nw in
               let se_x,se_y = Grid.pos_of_id grid se in
               for y=nw_y to se_y-1 do
                 let n1 = Grid.get grid (nw_x,y) in
                 let n2 = Grid.get grid (nw_x,y+1) in
                 S.add_edge_e graph (n1,0.,n2);

                 for x=nw_x to se_x-1 do
                   let n1 = Grid.get grid (x  ,y) in
                   let n2 = Grid.get grid (x+1,y) in
                   S.add_edge_e graph (n1,0.,n2);
                   (* this prevents the algo from adding more edges to these nodes *)
                   set_radius_of_id (Node.id n1) infinity;
                   set_radius_of_id (Node.id n2) infinity
                 done
               done;
               for x=nw_x to se_x-1 do
                 let n1 = Grid.get grid (x  ,se_y) in
                 let n2 = Grid.get grid (x+1,se_y) in
                 S.add_edge_e graph (n1,0.,n2);
                 set_radius_of_id (Node.id n1) infinity;
                 set_radius_of_id (Node.id n2) infinity
               done
              ) boxes;


    let m = {grid = grid; graph = graph; boxes = boxes} in


    
    let update_radius n =
      let id = Node.id n in
      let ball_r1 = ball m id (get_radius_of_id id) in

      let pp1 = BatHashtbl.fold (fun id d tmp -> 
                                 let n = Grid.get_id m.grid id in
                                 (Node.weight n) +. tmp) ball_r1 0. 
      in
      let l1 = BatHashtbl.length ball_r1 in
      let new_radius = Conf.Algo.radius_per_points pp1 in
      set_radius_of_id id new_radius;
      let ball_r2_option =
        if new_radius < infinity 
        then Some (ball_cached ball_r1 m id new_radius)
        else (clear_next_of_id id; None)
      in
      let diff = match ball_r2_option with Some ball_r2 -> (BatHashtbl.length ball_r2) -l1 | None -> -1 in
      ball_r2_option,diff
    in


    let exists_shorter visited stop dist =
      let d = 
        try 
          BatHashtbl.find visited (Node.id stop)
        with Not_found -> infinity
      in
      if d < dist then true else false
    in                                   

    let connect_further_node visited n =
      let id = Node.id n in

      let rec loop old cnt =
        (* let t1 = Util.get_time () in *)
        let next = get_next_of_id id in
        (* let t1_diff = int_of_float ((Util.get_time () -. t1) *. 1000.) in *)
        (* Printf.printf "n: %4ims  " t1_diff; *)
        match next with
        | None -> failwith "no more points to add"
        | Some next_pos ->
           let n2 = Grid.get_id m.grid next_pos in
           if Grid.is_in_any_box m.grid m.boxes (Node.id n2)
           then loop n2 (cnt+1)
           else
             let radius = get_radius_of_id id in
             (* let t1 = Util.get_time () in *)
             let shorter = exists_shorter visited n2 radius in
             (* let t1_diff = int_of_float ((Util.get_time () -. t1) *. 1000.) in *)
             (* Printf.printf "s: %4ims  " t1_diff; *)
             if shorter
             then loop n2 (cnt+1)
             else
               (
                 (* let t1 = Util.get_time () in *)
                 S.remove_edge graph n n2; (* remove all edges between n and n2 *)
                 S.add_edge_e graph (n,radius,n2);
                 (* let t1_diff = int_of_float ((Util.get_time () -. t1) *. 1000.) in *)
                 (* Printf.printf "e: %4ims  " t1_diff; *)
                 cnt
               )
      in
      let cnt = loop n 0 in
      cnt
    in


    (* TODO
       this loop contains several calls to Gc.compact to cope with a memory leak that most likely is caused by one of the C library bindings.
       Running it in valgrind gives segmentation fault with MAPERR.
       Solving this requires a lot of time and probably a patch to one of the faulty bindings, so for now this ugly workaround is ok.       
     *)

   (* add edges in passes over the grid *)
    let rec loop i =
      (* let print_over = Util.new_print_over () in *)
      let size = (float (Grid.length m.grid)) ** 2. in
      let skipped = ref 0 in
      let connected = ref 0 in
      let finished = ref true in
      let t_pass = ref (Util.get_time ()) in
      (* let t_max_radius = ref (-1) in *)
      (* let t_max_connect = ref (-1) in *)
      Grid.iter m.grid
        (fun n ->
         (* print_over (Printf.sprintf "pass: %2i  skipped: %5.2f%%  done: %5.2f%%  id:%4i  connected: %4i%!" i ((float !skipped) /. size *. 100.) ((float (Node.id n)) /. size *. 100.) (Node.id n) !connected); *)
         let old_radius = get_radius_of_id (Node.id n) in
         if old_radius = infinity
         then (skipped := !skipped + 1;
               ())   (* this point is done *)
         else
           (* let t4 = Util.get_time () in *)
           let ball_r2_option,diff = update_radius n in
           (* let t_diff_radius = int_of_float ((Util.get_time () -. t4) *. 1000.) in *)
           match ball_r2_option with
           | None -> ()
           | Some ball_r2 ->
              (finished := false;
               connected := !connected + 1;
               (* let t3 = Util.get_time () in *)
               let _ = if (Node.id n) mod 10000 = 0 then (Gc.compact (); Printf.printf "Compaction at node %i\n" (Node.id n)) in
               let (* tries *) _ = connect_further_node ball_r2 n in
               (* let t_diff_connect = int_of_float ((Util.get_time () -. t3) *. 1000.) in *)
               (* if (t_diff_connect > !t_max_connect) || (t_diff_radius > !t_max_radius) *)
               (* then ( *)
               (*   if (t_diff_connect > !t_max_connect) then t_max_connect := t_diff_connect; *)
               (*   if (t_diff_radius > !t_max_radius) then t_max_radius := t_diff_radius; *)
               (*   Printf.printf "%8i: connect_further %5i tries in %6ims    radius diff %5i in %6ims\n%!" (Node.id n) tries !t_max_connect diff !t_max_radius *)
               (* ) *)
               ()
              )
        );
      let s,t2 = Util.string_of_time !t_pass in
      Gc.compact ();
      Printf.printf "Pass: %2i  skipped: %5.2f%%  time: %s\n%!"
                    i ((float !skipped) /. size *. 100.) s;
      t_pass := t2;
      to_files m "temp";

      if (not !finished) && ((float !skipped) /. size *. 100.) <= Conf.Algo.stop_percent
      then loop (i+1)
      else                      (* examine remaining nodes and exit *)
        let max_dist_border = ref (-1) in
        let dist_length = ref 0 in
        let dist_sum = ref 0 in
        let l = Grid.length m.grid in
        let _ = Printf.printf "length %i    stopped at %f\n" l Conf.Algo.stop_percent in
        let dists = ref [] in
        Grid.iter m.grid
                  (fun n ->
                   let radius = get_radius_of_id (Node.id n) in
                   if radius <> infinity
                   then
                     let x,y = Grid.pos_of_id m.grid (Node.id n) in
                     let dist_border = BatList.min [x;y;(l-x);(l-y)] in
                     dists := (float dist_border)::!dists;
                     max_dist_border := max !max_dist_border dist_border;
                     dist_length := !dist_length +1;
                     dist_sum := !dist_sum + dist_border
                  );
        Printf.printf "Border distance: %s\n" 
                      (Util.string_of_values (Util.stat !dists))
    in
    loop 0;
    Printf.printf "\n";
    m



  let stat_box m box =

    (* stats of edges *)
    let l = ref [] in

    S.iter_edges_e
      (fun e ->
       let v = S.E.label e in
       (* at least one vertex needs to be in the box *)
       if (Grid.is_in_box m.grid box (Node.id (S.E.src e))) ||
          (Grid.is_in_box m.grid box (Node.id (S.E.dst e)))
       then l := v::!l)
      m.graph;
    let d = Util.stat !l in

    (* degree of verteces, also dumped to file *)
    let l = ref [] in

    let file = "conn.dat" in
    let oc = open_out file in
    Printf.fprintf oc "#id\tconn\n";
    Grid.iter_box m.grid box
      (fun n ->
       let v = float (S.out_degree m.graph n) in
       Printf.fprintf oc "%09i %f\n" (Node.id n) v ;
       l := v::!l)
      ;
    close_out oc;
    let c = Util.stat !l in

    (d,c)


  let stat m = 
    let l = length m in
    stat_box m (0,l*l-1)

end


(**
Info of an elastic metric
  *)
module ElasticInfo = Make_info (Elastic)



                                      
(**
Euclidean metric scaled with an epsilon
 *)
module Euclidean : sig
  type t 
  include Metric with type t := t
  val make : Grid.t -> float -> t
  val make_of_avg_err : Grid.t -> float -> t
  val eps : t -> float
end
= struct

  type t = {grid : Grid.t ; epsilon : float}
             
  let get_grid l = l.grid
                     
  let distances_from l id = [(0,0.)] (* TODO *)
                              
  let pp_of_ball l center_id r = Grid.pp_of_ball l.grid center_id (r /. l.epsilon)

  let make g e = {grid = g; epsilon = e}

  let make_of_avg_err g err =
    let eps = Laplacian.radius_of_expected_value_polar err in
    make g eps

  let eps l = l.epsilon
end

(**
Info of an euclidean metric
 *)
module EuclideanInfo = Make_info (Euclidean)
