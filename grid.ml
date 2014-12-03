(* Copyright (C) 2014, Marco Stronati, All Rights Reserved.
   This file is distributed under the terms of the
   GNU General Public License version 3 or later. *)

open Geo

(**
Geographic data structures.
 *)

(**
A geographic location, intended as a squared area, with a weight.
*)
module Node : sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val make : int -> Geo.Utm.t -> float -> t
  val coord : t -> Geo.Utm.t
  val weight : t -> float
  val id : t -> int
  val set_id : t -> int -> unit
  val set_weight : t -> float -> unit
  val geojson_of : ?properties:string -> float -> t -> string
end
= struct
  type t = {
    coord : Utm.t;
    mutable id : int;
    mutable weight : float;
  }

  let hash n = Hashtbl.hash n.id
  let equal n1 n2 = n1.id = n2.id
  let compare a b =
    if equal a b then 0
    else compare a b

  let make id coord weight = {
    id = id;
    coord = coord;
    weight = weight;
  }
  let coord n = n.coord
  let weight n = n.weight
  let id n = n.id
  let set_id n id = n.id <- id
  let set_weight n w = n.weight <- w

  let string_of n =
    Printf.sprintf "\"%s\"" (Utm.string_of n.coord)

  let geojson_point_of ?(properties = "") radius n =
    let (lat,lon) = Wgs.tuple (Wgs.of_xy n.coord) in
    Printf.sprintf
      "{\"type\": \"Feature\",
        \"geometry\": {\"type\": \"Point\", \"coordinates\": [%f, %f]},
        \"properties\": {
           %s
          \"id\": %i,
          \"radius\": %f,
          \"weight\": %f
         }
       }" lon lat
      properties
      n.id
      radius
      n.weight

  let geojson_region_of ?(properties = "") radius n =
    let string_of_coo coord =
      let (lat,lon) = Wgs.tuple (Wgs.of_xy coord) in
      Printf.sprintf "[%f, %f]" lon lat (* inversion for geojson *)
    in
    let dist_center_edge = radius *. (sqrt 2.) in
    let p1 = Utm.destination n.coord 45. dist_center_edge in
    let p2 = Utm.destination n.coord 135. dist_center_edge in
    let p3 = Utm.destination n.coord 225. dist_center_edge in
    let p4 = Utm.destination n.coord 315. dist_center_edge in

    Printf.sprintf
      "{\"type\": \"Feature\",
        \"geometry\": {
          \"type\": \"Polygon\",
          \"coordinates\": [[ %s, %s, %s, %s, %s]]
         },
        \"properties\": {
          %s
          \"id\": %i,
          \"coord\": %s,
          \"radius\": %f,
          \"weight\": %f
         }
       }"
      (string_of_coo p1) (string_of_coo p2)
      (string_of_coo p3) (string_of_coo p4)
      (string_of_coo p1)
      properties
      n.id
      (string_of_coo n.coord)
      radius
      n.weight


  let geojson_of = geojson_region_of

end

(**
Locations arranged in a squared grid.
Locations are layed from left to right, top to bottom in a z-like fashion.
e.g. in a grid with 100 points, north-west has id 0, north-east 9, south-west 90, south-east 99.
Locations can be also referenced by position in a matrix style (x,y) with x the row and y the column. 
e.g. north-west is (0,0), south-west (9,0) *)
module Grid : sig
  type t
  type box_in = int * int
  type pos = int * int
  val length: t -> int
  val radius: t -> float
  val iter : t -> (Node.t -> unit) -> unit
  val iter_box : t -> box_in -> (Node.t -> unit) -> unit
  val get: t -> int * int -> Node.t
  val get_id: t -> int -> Node.t
  val pos_of_id : t -> int -> pos
  val id_of_pos : t -> pos -> int
  val make : Geo.Utm.t -> float -> int -> t
  val make_parallel : Geo.Utm.t -> float -> int -> int -> t
  val carve: t -> box_in -> t
  val find: t -> Geo.Utm.t -> pos
  val to_geojson : t -> string -> unit
  val to_geojson_box : t -> box_in -> string -> unit
  val to_file_boxes : t -> box_in list -> string -> unit
  val of_file_boxes : t -> string -> box_in list
  val to_file : t -> string -> unit
  val of_file : string -> t
  val next_by_distance : t -> int -> pos -> pos
  val stat : t -> Util.values
  val diag_of_box: t -> Geo.box -> box_in
  val full_box : t -> box_in
  val is_in_box : t -> box_in -> int -> bool
  val is_in_any_box : t -> box_in list -> int -> bool
  val normalize: t -> t
  val pp_of_ball: t -> int -> float -> float
  val to_numbers_grid: t -> unit
  val of_warped: string -> t
end
= struct
  type t = Node.t array array
  type box_in = int * int
  type pos = int * int

  let length g = Array.length g

  let get g (x,y) = g.(x).(y)
  let set g (x,y) n = g.(x).(y) <- n

  (** Convert ids into positions *)
  let pos_of_id g id =
    let l = length g in
    if id < 0 || id >= (l * l)
    then raise (Invalid_argument (Printf.sprintf "invalid id: %i" id))
    else
      ((id / l), (id mod l))

  (** Convert positions into ids *)
  let id_of_pos g (x,y) =
    if x <0 || x >= length g ||
       y <0 || y >= length g
    then raise (Invalid_argument (Printf.sprintf "invalid pos: (%i,%i)" x y))
    else
    let l = length g in
    (x * l) + y

  (** [get_id g i] returns the node with id [i] in [g] *)
  let get_id g id =
    let p = pos_of_id g id in
    get g p

  (** [set_id g i n] sets the id of node [i] to [n] *)
  let set_id g id n =
    if (Node.id n) <> id then failwith "Id mismatch" else
    let p = pos_of_id g id in
    set g p n

  (* note: this is double the radius, or the diameter of the cirle *)
  let step g =
    Utm.distance
      (Node.coord (get g (0,0)))
      (Node.coord (get g (0,1)))

  (** Return the radius of the grid. That is the horizontal or vertical distance between two adjacent locations or the size of a location cell. *)
  let radius g = step g /. 2.


  (* TODO maybe this center and the center of the iter_box and carve don't coincide *)
  (* if (length g) is odd we get the perfect center otherwise we get the top-left corner of the 2x2 square in the center*)
  let get_center g =
    let x = (length g) / 2 in
    id_of_pos g (x,x)

  (** Apply a function on all the nodes of the grid, in id order. *)
  let iter g f =
    let l = length g in
    for x=0 to l-1 do
      for y=0 to l-1 do
        f g.(x).(y)
      done
    done

  let fold_box g (nw,se) f init =
    let nw_x,nw_y = pos_of_id g nw in
    let se_x,se_y = pos_of_id g se in

    let x_min = nw_x in
    let x_max = se_x in
    let y_min = nw_y in
    let y_max = se_y in

    let tmp = ref init in        
    for x=x_min to x_max do
      for y=y_min to y_max do
        tmp := f !tmp g.(x).(y)
      done
    done;
    tmp

  let fold g f init =
    let l = length g in
    fold_box g (0,l*l-1) f init
    
  (* (\* TODO inefficient but safe *\) *)
  (* (\* TODO this shoudl also raise Not_found, now if the elemtn is not in the grid, the closest is reported anyway!!! *\) *)
  (* let find_slow g coord = *)
  (*   let id = ref (-1) in *)
  (*   let d = ref infinity in *)
  (*   iter g (fun n -> *)
  (*           let d2 = Utm.distance (Node.coord n) coord in *)
  (*           if d2 < !d then (d := d2; id := Node.id n) else ()); *)
  (*   pos_of_id g !id *)

  (* pay attention to difference between xy coordinates and xy position in the grid *)
  (** Find the closest location to the given cartesian coordinates 
      @raise Not_found if the location is outside the grid
   *)
  let find g coord =
    let (x,y) = Utm.tuple coord in
    let r = radius g in
    let rec find_in now_pos =
      let nx,ny = now_pos in
      let now_x,now_y =
        try Utm.tuple (Node.coord (get g now_pos))
        with _ -> raise Not_found
      in
      let dx,dy = abs_float(x-.now_x),abs_float(y-.now_y) in
      let sx,sy = Util.sign_float (x-.now_x),Util.sign_float (y-.now_y) in
      let next_x = if dy > r then nx - sy else nx in
      let next_y = if dx > r then ny + sx else ny in
      let next = (next_x,next_y) in
      (* Printf.printf "%.0f,%.0f r %.0f  now_pos %i,%i now_xy %.0f,%.0f\n" x y r nx ny now_x now_y; *)
      (* Printf.printf "dx %.0f dy%.0f  sx%i sy%i  next %i,%i\n" dx dy sx sy next_x next_y; *)
      if next = now_pos then now_pos else find_in next
    in
    find_in (0,0)



  let to_file_boxes m boxes file =
    let oc = open_out file in
    let dump_box (nw,se) =
      Printf.fprintf oc "%05i,%05i\n" nw se;
    in
    List.iter dump_box boxes;
    close_out oc


  let of_file_boxes m file =
    let boxes = ref [] in
    let ic = Pervasives.open_in file in
    let rec loop () =
      let maybe_s =
        try
          Some (Pervasives.input_line ic)
        with End_of_file -> None
      in
      match maybe_s with
      | Some s ->
         let _ = Scanf.sscanf s "%i,%i"
           (fun nw se ->
            (* Printf.printf "reading node %i at pos %i\n" i (Pervasives.pos_in ic); *)
            boxes := !boxes@((nw,se)::[]))
         in
         loop ()
      | None -> Pervasives.close_in ic
    in
    loop ();
    !boxes
    

  (** Apply a function on all nodes inside a box, in id order. *)
  let iter_box g (nw,se) f = 
    let nw_x,nw_y = pos_of_id g nw in
    let se_x,se_y = pos_of_id g se in

    let x_min = nw_x in
    let x_max = se_x in
    let y_min = nw_y in
    let y_max = se_y in

    for x=x_min to x_max do
      for y=y_min to y_max do
        f g.(x).(y)
      done
    done


  (* let iter_rev g f = *)
  (*   let l = length g in *)
  (*   for x=l-1 downto 0 do *)
  (*     for y=l-1 downto 0 do *)
  (*       f g.(x).(y) *)
  (*     done *)
  (*   done *)

  (* let next_pos g (x,y) = *)
  (*   let l = Array.length g in *)
  (*   if y = l-1 *)
  (*   then *)
  (*     (if x = l-1 *)
  (*     then None *)
  (*     else Some (x+1,0)) *)
  (*   else Some (x,y+1) *)

  (* let rec iter_from f g (i,j) = *)
  (*   f (get g (i,j)); *)
  (*   match next_pos g (i,j) with *)
  (*     None -> () *)
  (*   | Some pos -> iter_from f g pos *)

  let iteri g f =
    let l = length g in
    for x=0 to l-1 do
      for y=0 to l-1 do
        f g.(x).(y) ((x*l) + y)
      done
    done

  let split start1 step length =
    if length mod 2 <> 0 then raise (Invalid_argument "odd length");
    let half = (length / 2) in
    let start2 = Utm.destination start1  90. (step *. (float half)) in
    let start3 = Utm.destination start1 180. (step *. (float half)) in
    let start4 = Utm.destination start3  90. (step *. (float half)) in
    (start1,start2,start3,start4,half)

  let merge l =
    match l with 
    | [g1;g2;g3;g4] ->
       if (length g1) <> (length g2) ||
          (length g2) <> (length g3) ||
          (length g3) <> (length g4)
       then raise (Invalid_argument "incompatible lengths");
       
       let m_left  = Array.append g1 g3 in
       let m_right = Array.append g2 g4 in
       
       let m = Array.mapi (fun i row_l ->
                           Array.append row_l m_right.(i)
                          ) m_left
       in
       iteri m (fun n i -> Node.set_id n i);
       m
    | _ -> raise (Invalid_argument "incompatible lengths")


  let make_nw start step length = 
    let radius = step /. 2. in
    let w0 = Query.get_weight 0 start radius in
    let n0 = Node.make 0 start w0 in
    let m = Array.make_matrix length length n0 in
    for x=0 to length-1 do      (* rows *)
      for y=0 to length-1 do    (* columns *)
        if x=0 && y=0 then () else
          let prev = if y=0 then m.(x-1).(y) else m.(x).(y-1) in
          let coo = if y=0
            then Utm.destination (Node.coord prev) 180. step
            else Utm.destination (Node.coord prev)  90. step
          in
          let id = ((x * length) + y) in
          let w = Query.get_weight id coo radius in
          let n = Node.make id coo w in
          m.(x).(y) <- n
      done
    done;
    m

  (*
     if length is even the center is actually slightly south est of the ideal center
   *)
  (** 
[make center step length] creates a grid from a central location
[center], in cartesian coordinates, where [step] is the distance
between adjacent location and [length] the number of locations in a
side of the grid.
*)
  let make center step length =
    let center_west = Utm.destination center 270. (step *. (float (length /2))) in
    let nw = Utm.destination center_west 0. (step *. (float (length /2))) in
    make_nw nw step length 


  (* Fixed at 4 cores *)
  (** Same as [make] but uses Parmap to parallelize over [n] cores 
   @see <https://github.com/rdicosmo/parmap/> Parmap
   *)
  let make_parallel center step length cores = 
    if length mod 2 <> 0 then raise (Invalid_argument "odd length");

    let half = (length / 2) in
    let center_west = Utm.destination center 270. (step *. (float (length /2))) in
    let start1 = Utm.destination center_west 0. (step *. (float (length /2))) in
    let start2 = Utm.destination start1  90. (step *. (float half)) in
    let start3 = Utm.destination start1 180. (step *. (float half)) in
    let start4 = Utm.destination start3  90. (step *. (float half)) in
    
    Parmap.set_default_ncores cores;
    let grids = Parmap.parmap
                          (fun start -> make_nw start step half)
                          (Parmap.L [start1;start2;start3;start4])
    in
    merge grids


  (* this is expensive, contains a find *)
  let diag_of_box g (Central (coo,size)) =
       let cx,cy = find g coo in
       let steps = int_of_float (ceil (size /. step g)) in
       Printf.printf "c (%i,%i)  size  %f   step %f   steps %i\n" cx cy size (step g) steps;
       let half = steps / 2 in
       let nw_pos = cx-half, cy-half in
       let se_pos = 
         if steps mod 2 = 0
         then (* center is bottom-right of the 2x2 square in the center ?? *)
           cx+half-1, cy+half-1
         else (* center is at the center *)
           cx+half, cy+half
       in
       Printf.printf "c (%i,%i)  size  %f   step %f   steps %i\n" cx cy size (step g) steps;
       Printf.printf "nw (%i,%i)  se (%i,%i)\n" (fst nw_pos) (snd nw_pos) (fst se_pos) (snd se_pos);
       Printf.printf "nw %i       se %i\n" (id_of_pos g nw_pos) (id_of_pos g se_pos);
       (* let dx= fst se_pos - fst nw_pos  in *)
       (* let dy = snd se_pos - snd nw_pos  in *)
       (* if dx <> steps || dy <> steps then failwith (Printf.sprintf "dx %i dy %i\n" dx dy); *)
       (id_of_pos g nw_pos, id_of_pos g se_pos)
         

  let full_box g =
    let l = length g in
    (0,l*l-1)


  (** Create a new grid of a grid of larger size. Each node id is updated. *)
  let carve g (nw,se) =
    let nw_x,nw_y = pos_of_id g nw in
    let se_x,se_y = pos_of_id g se in

    let l_carved = se_x-nw_x+1 in
    let cx,cy = nw_x + l_carved/2,nw_y+l_carved/2 in
    (* Printf.printf "c (%i,%i)  l %i\n" cx cy l_carved; *)

    let new_of_old_pos (x,y) = (x - cx + (l_carved/2),
                                y - cy + (l_carved/2))
    in

    let n0 = Node.make 0 (Utm.of_latlon Geo.rio) 0. in
    let g_carved = Array.make_matrix l_carved l_carved n0 in
    iter_box g (nw,se) 
             (fun n ->
              let (x,y) = pos_of_id g (Node.id n) in
              let (x_new,y_new) = new_of_old_pos (x,y) in
              (* Printf.printf "xy (%i,%i)  xynew (%i,%i)\n" x y x_new y_new; *)
              let id_new = id_of_pos g_carved (x_new,y_new) in
              g_carved.(x_new).(y_new) <- n;
              Node.set_id g_carved.(x_new).(y_new) id_new
             );

    (* let cnt = ref 0 in *)
    (* iter g (fun n ->  *)
    (*         if Node.coord n = Geo.rio then cnt := !cnt +1; *)
    (*        ); *)
    (* if !cnt > 1 then failwith "too many n0"; *)

    g_carved








(* http://stackoverflow.com/questions/16571362/how-to-iterate-over-rings-in-a-raster-grid *)
  let rec next_by_distance_continuous g center_id state = 
    let now_id,ring_idx = state in
    let cx,cy = pos_of_id g center_id in

    let dist dx dy = int_of_float (sqrt (((float dx) ** 2.) +. ((float dy) ** 2.))) in
                          

    let generate_ring d =

      let dx = d in
      let dy = 0 in

      let rec loop tmp =
        let (dx,dy) = List.hd tmp in

        let nx = - (Util.sign dy) in
        let ny =   (Util.sign dx) in

        (* Printf.printf "Got: (%f,%f)     and nx,ny = (%.0f,%f.0)\n" dx dy nx ny; *)

        let dx,dy =
          if nx <> 0 && (dist (dx+nx) dy) = d
          then (dx + nx),dy
          else if ny <> 0 && (dist dx (dy+ny)) = d
          then dx,(dy + ny)
          else (dx + nx),(dy + ny)
        in
        (* Printf.printf "Made: (%f,%f)\n" dx dy; *)
        if dx <> d || dy <> 0
        then loop ((dx,dy)::tmp)
        else (* List.rev *) tmp
      in
      let relative_ring = loop ((dx,dy)::[]) in

      let l = length g in
      List.fold_left (fun tmp (x,y) -> let ax,ay = (cx+x,cy+y) in
                                       if 0 <= ax && ax < l && 0 <= ay && ay < l
                                       then (id_of_pos g (ax,ay))::tmp
                                       else tmp)
                     [] relative_ring
    in

    let rec find_next ring =

      let ids_ring = generate_ring ring in

      let current_ring id =
        let nx,ny = pos_of_id g id in
        dist (cx-nx) (cy-ny)
      in
      List.iter (fun id -> if (current_ring id) <> ring then failwith "buuuu") ids_ring;

      let rec loop l = 
        match l with
          [] -> find_next (ring+1)
        | id::rest -> if id = now_id then 
                        try Some ((List.hd rest),-1)
                        with _ ->
                          (* Printf.printf "reached end, generating new ring\n\n"; *)
                          try Some ((List.hd (generate_ring (ring+1))),-1)
                          with _ -> 
                            (* Printf.printf "reached end of new ring\n\n"; *)
                            None
                      else loop rest
      in
      loop ids_ring
    in
    
    let current_ring =
      let nx,ny = pos_of_id g now_id in
      dist (cx-nx) (cy-ny)
    in
    find_next current_ring







  (* let ring_visited = ref [] *)

  let rec next_by_distance_jumps g center_id state = 
    let now_id,ring_idx = state in
    let cx,cy = pos_of_id g center_id in

    let dist dx dy = int_of_float (sqrt (((float dx) ** 2.) +. ((float dy) ** 2.))) in
                          
    let sign x = if x = 0 then 0 else if x < 0 then -1 else 1 in

    let generate_ring d =

      let dx = d in
      let dy = 0 in

      let ring = BatDynArray.create () in
      BatDynArray.add ring (dx,dy);

      let rec loop () =
        let (dx,dy) = BatDynArray.last ring in

        let nx = - (sign dy) in
        let ny =   (sign dx) in

        (* Printf.printf "Got: (%f,%f)     and nx,ny = (%.0f,%f.0)\n" dx dy nx ny; *)

        let dx,dy =
          if nx <> 0 && (dist (dx+nx) dy) = d
          then (dx + nx),dy
          else if ny <> 0 && (dist dx (dy+ny)) = d
          then dx,(dy + ny)
          else (dx + nx),(dy + ny)
        in
        (* Printf.printf "Made: (%f,%f)\n" dx dy; *)
        if dx <> d || dy <> 0
        then (BatDynArray.add ring (dx,dy); loop ())
        else ()
      in
      loop ();
      ring
    in

(* idx = i*s mod l *)
(*   r = i*s  /  l *)
(* while i < l *)

(* i*s = r*l + idx *)

(* idx_n = (i+1)*s mod l = (i*s + s) mod l = (i*s mod l) + (s mod l) mod l = idx + (s mod l) mod l *)

(* idx=4 => i*3 mod 10 = a,4 -> i=8 *)
(* i = (idx + (l * r)) / s *)

(* idx = root + (i*s) *)
(* idx_n = root + (i+1) * s = root + is + s *)
(*                 if idx_n >= l then  *)
(*                 root+1 *)

(* root = idx_n / s  *)

    let next_idx ring idx = 

      if idx = -1 
      then 
        (* let _ = Printf.printf "Case idx = -1\n" in *)
        Some 0
      else
      let l = BatDynArray.length ring in
      let step = 4 in

      if l <= step
      then 
        (* let _ = Printf.printf "Case l<step\n" in *)
        if idx+1 < l
        then
          (* let _ = Printf.printf "length %2i   idx %2i    next %2i\n" l idx (idx+1) in *)
          Some (idx+1)
        else None
      else
        (* let _ = Printf.printf "Case regular \n" in *)
        (* let _ = Printf.printf "idx %2i   step %2i   l %2i    " idx step l in *)
        let idx_n = 
          if idx + step >= l
          then 
            let root = idx mod step in
            if root = step-1
            then None 
            else Some (root+1)
          else Some (idx + step)
        in
        
        (* (match idx_n with *)
        (*    None -> Printf.printf "next _\n%!"; *)
        (*  | Some next_idx -> *)
        (*     Printf.printf "next %2i\n%!" next_idx); *)
        idx_n
    in

    (* let sprint_dynarray l =  *)
    (*   let core = BatDynArray.fold_left (fun tmp (x,y) -> Printf.sprintf "%s(%i,%i)," tmp x y) "" l in *)
    (*   ("["^core^"]") *)
    (* in *)


    (* let check_ring_visited ring = *)
    (*   let l = BatDynArray.length ring in *)
    (*   let sorted = List.sort compare !ring_visited in *)
    (*   Printf.printf "ring_visited \n %s \n%!" (Util.string_of_int_list !ring_visited); *)
    (*   Printf.printf "sorted    \n %s \n%!" (Util.string_of_int_list sorted); *)
    (*   Printf.printf "ring      \n %s \n%!" (sprint_dynarray ring); *)
    (*   if List.length sorted <> l then failwith (Printf.sprintf "different length  ring_visited %2i  ring %2i   center_if %i    now_id %i\n" (List.length sorted) l center_id now_id); *)
    (*   let rec loop i idxs = *)
    (*     match idxs with *)
    (*       [] -> if i <> l then failwith (Printf.sprintf "ring_visited_end i %i  l %i \n %s \n" i l (Util.string_of_int_list sorted)) *)
    (*       | e::rest -> if i <> e *)
    (*                    then failwith (Printf.sprintf "ring_visited_in i %i  e %i \n %s \n" i e (Util.string_of_int_list sorted)) *)
    (*                    else loop (i+1) rest *)
    (*   in *)
    (*   loop 0 sorted; *)
    (*   ring_visited := [] *)
    (* in *)


    let filter (x,y) =
      let ax,ay = (cx+x,cy+y) in
      id_of_pos g (ax,ay)
    in

    (* find next node in the current ring or generate new rings *)
    let rec find_next ring_n idx =
      let l = length g in

      if ring_n > (BatList.max [(l-cx);(l-cy);cx;cy])  (* TODO check if this test is too loose or strict *)
      then
        failwith "No more rings! Grid exhausted."
      else

      let ring = generate_ring ring_n in

      (* let current_ring nx ny = *)
      (*   dist (cx-nx) (cy-ny) *)
      (* in *)
      (* BatDynArray.iter (fun (x,y) -> try let _ = filter (x,y) in if (current_ring x y) <> ring_n then failwith "ring mismatch" with _ -> ()) ring; *)

      (* find next node in the current ring *)
      let rec loop idx =
        match idx with 
          None -> 
          (* check_ring_visited ring; *)
          find_next (ring_n+1) (-1) (* finished ring *)
        | Some idx ->
           try (filter (BatDynArray.get ring idx),idx)
           with
             Invalid_argument _ ->            
             (* let _ = Printf.printf "failed filter \n%!" in *)
             let idx_n = next_idx ring idx in
             (* (match idx_n with None -> () | Some i -> ring_visited := i::!ring_visited); *)
             loop idx_n (* filter failed, this loop may never end! *)
      in
      let idx_n = next_idx ring idx in
      (* (match idx_n with None -> () | Some i -> ring_visited := i::!ring_visited); *)
      loop idx_n
    in

    let current_ring =
      let nx,ny = pos_of_id g now_id in
      dist (cx-nx) (cy-ny)
    in
    find_next current_ring ring_idx



  (* This can't be tested twice on the same point.
     To test just pick a grid and run:
     let _ = Grid.iter grid (fun n -> let _ = Grid.ball grid (Node.id n) 1000. in ()) in
  *)
  (* let center_old = ref (-1) *)
  (* let check_wrapper g center_id state = *)
  (*   if !center_old <> center_id  *)
  (*   then ( *)
  (*     let _ = Printf.printf "\nchanged id from %i to %i\n" !center_old center_id in *)
  (*     center_old := center_id;  *)
  (*     ring_visited:=[]); *)
  (*   let now_id,ring_idx = state in *)
  (*   let _ = Printf.printf "\ncenter:%i   state (idx:%i, ring_idx:%i)\n" center_id now_id ring_idx in *)
  (*   let res = next_by_distance_jumps g center_id state in *)
  (*   match res with  *)
  (*   | None -> let _ = Printf.printf "\ncenter:%i generated NONE\n" in None *)
  (*   | Some (id,idx) ->  *)
  (*      let _ = Printf.printf "\ncenter:%i generated id:%i   idx:%i\n" center_id id idx in *)
  (*      res *)


  (* let rec next_by_distance_old g center_id state = *)
  (*   let cx,cy = pos_of_id g center_id in *)
  (*   let now_id,outer_ring,neighbors = state in *)

  (*   let new_ring diff = *)
  (*     let norths = List.map (fun y -> ((cx-diff),y)) (Util.sweep (cy-diff)   (cy+diff-1)) in *)
  (*     let easts  = List.map (fun x -> (x,(cy+diff))) (Util.sweep (cx-diff)   (cx+diff-1)) in *)
  (*     let souths = List.map (fun y -> ((cx+diff),y)) (Util.sweep (cy-diff+1) (cy+diff)) in *)
  (*     let wests  = List.map (fun x -> (x,(cy-diff))) (Util.sweep (cx-diff+1) (cx+diff)) in *)
  (*     let neighbors_pos = norths @ wests @ souths @ easts in *)
  (*     let l = length g in *)
  (*     let filtered = List.filter (fun (x,y) -> *)
  (*                                 if 0 <= x && x < l && 0 <= y && y < l *)
  (*                                 then true else false) neighbors_pos *)
  (*     in *)
  (*     List.map (fun pos -> id_of_pos g pos) filtered *)
  (*   in *)

  (*   let add_neighbors neighbors = *)
  (*     let ids = new_ring (outer_ring+1) in *)
  (*     let id_dists = List.map *)
  (*                      (fun id -> *)
  (*                       let coo1 = Node.coord (get_id g center_id) in *)
  (*                       let coo2 = Node.coord (get_id g id) in *)
  (*                       (id, Geo.distance coo1 coo2)) ids *)
  (*     in *)

  (*     match id_dists with *)
  (*     | [] -> ((now_id,outer_ring+1,neighbors),false) *)
  (*     | _ -> *)
  (*        let sorted = List.sort (fun (_,a) (_,b) -> *)
  (*                                let c = my_float_compare a b in *)
  (*                                if c = 0 *)
  (*                                then if Random.bool () then -1 else 1 *)
  (*                                else c *)
  (*                               ) (neighbors@id_dists) in *)
  (*        (\* Printf.printf "\nSorted %!"; *\) *)
  (*        (\* List.iter (fun (x,w) -> let (x,y) = pos_of_id g x in  *\) *)
  (*        (\*               Printf.printf "((%i,%i),%f);%!" x y w) sorted; *\) *)
  (*        (\* Printf.printf "\n%!"; *\) *)
  (*        ((now_id,outer_ring+1,sorted),true) *)
  (*   in *)

  (*   match neighbors with *)
  (*   | [] -> *)
  (*      let (new_state,updated) = add_neighbors [] in *)
  (*      if updated *)
  (*      then next_by_distance_old g center_id new_state *)
  (*      else (None,new_state) *)
  (*   | (x,d)::rest -> *)
  (*      let next_outer_closer = (float (outer_ring+1)) *. (step g) in *)
  (*      if (d > next_outer_closer) *)
  (*      then *)
  (*        let (new_state,updated) = add_neighbors neighbors in *)
  (*        if updated *)
  (*        then (next_by_distance_old g center_id new_state) *)
  (*        else *)
  (*          let _,outer_ring,neighbors = new_state in *)
  (*          (Some x,(x,outer_ring,neighbors)) *)
  (*      else *)
  (*         (Some x,(x,outer_ring,rest)) *)


  (** Starting from a location return the id and distance of the next closest location
 *)
  let next_by_distance = next_by_distance_jumps


  let stat g =
    let l = ref [] in
    iter g (fun n ->
            let w = Node.weight n in
            l := w::!l
           );
    Util.stat !l


  let geojson_box g box =
    let max_weight = ref (-. 1.) in
    iter_box g box (fun n -> max_weight := max !max_weight (Node.weight n));
    let radius = radius g in
    let string = ref "" in
    iter_box g box
             (fun n ->
              let w_norm = Printf.sprintf "\"weight_n\" : %f," ((Node.weight n) /. !max_weight) in
              string := Printf.sprintf "%s%s,\n" !string (Node.geojson_of ~properties:w_norm radius n));
    let nodes_string = !string in
    (* remove last ,\n *)
    let cleaned = String.sub nodes_string 0 (String.length nodes_string -2) in
    Printf.sprintf
      "{ \"type\": \"FeatureCollection\",
         \"features\": [\
          %s
          ]
       }" cleaned


  let to_geojson_box g box file =
    Formats.geojson_to_file (geojson_box g box) file

  let to_geojson g file =
    let l = length g in
    to_geojson_box g (0,l*l-1) file


  (* save to file *)
  let to_numbers_grid g =
   let oc = open_out "weights.dat" in
   let st = stat g in
   
   (* (\* there should be a frame of sea !!!!! with the mean *\) *)
   let l = length g in
   let f = 20 in
    for x=0 to l-1 do
      for y=0 to l-1 do
        if x < f || x > l-f ||
           y < f || y > l-f
        then                    
   let open Util in
          Printf.fprintf oc "%f " st.avg
        else
          Printf.fprintf oc "%f " (Node.weight g.(x).(y))
      done;
      Printf.fprintf oc "\n";
    done;
    close_out oc;

   let oc = open_out "coords.dat" in
  (* there should be a frame of sea !!!!! with the mean *)
    let l = length g in
    for x=0 to l-1 do
      for y=0 to l-1 do
      let (x,y) = Utm.tuple (Node.coord g.(x).(y)) in
        Printf.fprintf oc "%f %f\n" x y;
      done;
    done;
    close_out oc

  (* load from file *)
  let of_warped file =
    let open Geo in

    let count_lines filename =
      let ic = Pervasives.open_in filename in

      let rec loop i =
        let continue =
        try
          let _ = Pervasives.input_line ic in
          true
        with End_of_file -> false
        in
        if continue
        then loop (i+1)
        else i
      in
      let res = loop 0 in
      Pervasives.close_in ic;
      res
    in

    let number_of_lines = float (count_lines file) in

    let ic = Pervasives.open_in file in
    let icw = Pervasives.open_in "warped.dat" in

    Printf.printf "number_of_lines %f\n" number_of_lines;
    let size = int_of_float (sqrt number_of_lines) in
    let dummy_node = Node.make (-1) (Utm.of_latlon Geo.paris) (-1.) in
    let m = Array.make_matrix size size dummy_node in

    let rec loop () =
      let maybe_s =
        try
          Some (Pervasives.input_line ic)
        with End_of_file -> None
      in
      match maybe_s with
      | Some s ->
         let warped_pos = Scanf.sscanf (Pervasives.input_line icw) "%f %f"
                                       (fun x y -> Utm.make x y)
         in
          let _ = Scanf.sscanf s "%i,%f,%f,%f"
            (fun i lat lon weight ->
            Printf.printf "reading node %i at pos %i\n" i (Pervasives.pos_in ic);
              let n = Node.make i warped_pos weight in
              set_id m i n)
          in
          loop ()
      | None -> (Pervasives.close_in ic; Pervasives.close_in icw)
    in
    loop ();
    m




              

  (* save to file *)
  let to_file g file =
    let oc = open_out file in
    let dump n =
      let (lat,lon) = Wgs.tuple (Wgs.of_xy (Node.coord n)) in
      (* Printf.printf "pos %i\n" (pos_out oc); *)
      Printf.fprintf oc "%09i,%+e,%+e,%e\n" (Node.id n) lat lon (Node.weight n);
    in
    iter g dump;
    close_out oc

  (* load from file *)
  let of_file file =
    let open Geo in
    let ic = Pervasives.open_in file in

    let number_of_lines filename =
      let ic = Pervasives.open_in filename in

      let rec loop i =
        let continue =
        try
          let _ = Pervasives.input_line ic in
          true
        with End_of_file -> false
        in
        if continue
        then loop (i+1)
        else i
      in
      let res = loop 0 in
      Pervasives.close_in ic;
      res
    in

    let number_of_lines = float (number_of_lines file) in

    Printf.printf "number_of_lines %f\n" number_of_lines;
    let size = int_of_float (sqrt number_of_lines) in
    let dummy_node = Node.make (-1) (Utm.of_latlon Geo.paris) (-1.) in
    let m = Array.make_matrix size size dummy_node in

    let rec loop () =
      let maybe_s =
        try
          Some (Pervasives.input_line ic)
        with End_of_file -> None
      in
      match maybe_s with
      | Some s ->
          let _ = Scanf.sscanf s "%i,%f,%f,%f"
            (fun i lat lon weight ->
            (* Printf.printf "reading node %i at pos %i\n" i (Pervasives.pos_in ic); *)
              let n = Node.make i (Utm.of_latlon (Wgs.make lat lon)) weight in
              set_id m i n)
          in
          loop ()
      | None -> Pervasives.close_in ic
    in
    loop ();
    m


  let is_in_box g box id =
    let (nw_id,se_id) = box in
    let nw_x,nw_y = pos_of_id g nw_id in
    let se_x,se_y = pos_of_id g se_id in
    let x,y = pos_of_id g id in
    if nw_x <= x && x <= se_x &&
       nw_y <= y && y <= se_y
    then true
    else 
      (* let _ = Printf.printf "is_NOT_in_box: nw_x  %i  se_x %i  nw_y %i  se_y %i   x,y %i,%i\n" nw_x se_x nw_y se_y x y in *)
      false

  let is_in_any_box g boxes id =
    List.fold_left (fun tmp box -> (is_in_box g box id) || tmp) false boxes



  (* let ball g id r =  *)
  (*   let c1 = Node.coord (get_id g id) in *)

  (*   clean_neighbors g id; *)
  (*   let rec loop sum old_id =  *)
  (*     match next_by_distance g id old_id with *)
  (*     | None -> sum *)
  (*     | Some new_id ->  *)
  (*        let n2 = get_id g new_id in *)
  (*        if Geo.distance c1 (Node.coord n2) <= r *)
  (*        then loop (sum +. (Node.weight n2)) new_id *)
  (*        else sum *)
  (*   in *)
  (*   let res = loop 0. id in *)
  (*   clean_neighbors g id; *)
  (*   res *)


  (* in a grid g, apply function f on all nodes within r distance from id, starting with value start.
   f: 'a -> Node.t -> 'a *)
  let fold_within g c_id r f start =
    let c1 = Node.coord (get_id g c_id) in

    let rec loop tmp old_state =
      let state = next_by_distance g c_id old_state in
      let (new_id,ring_idx) = state in
      let n2 = get_id g new_id in
      if Utm.distance c1 (Node.coord n2) <= r
      then loop (f tmp n2) (new_id,ring_idx)
      else tmp
    in
    let res = loop start (c_id,-1) in
    res

  let pp_of_ball g id r = fold_within g id r (fun sum n -> sum +. (Node.weight n)) 0.

  let tiles g id r = fold_within g id r (fun cnt _ -> cnt + 1) 0

  let avg_w g id r =
    let (n,sum) = fold_within g id r (fun (cnt,sum) n -> (cnt+.1., sum +. (pp_of_ball g id Conf.Grid.r_city))) (0.,0.) in
    sum /. n

  (* O(3n) *)
  let cutoff g =
    let w_sum = ref 0. in
    iter g (fun n ->
            let w = Node.weight n in
            w_sum := w +. !w_sum;
           );
    let avg = !w_sum /. (float ((length g) * (length g))) in

    let sd_sum = ref 0. in
    iter g (fun n ->
            let w = Node.weight n in
            let sd = (w -. avg) ** 2. in
            sd_sum := sd +. !sd_sum;
           );
    let sd = sqrt (!sd_sum /. (float ((length g) * (length g)))) in
    let new_max = avg +. (sd *. 2.) in

    iter g (fun n ->
            let w = if Node.weight n >= new_max
                    then new_max
                    else Node.weight n
            in
            Node.set_weight n w
         );
    (g,new_max)

  let normalize g =
    let (g,max_w) = cutoff g in
    let tiles_country = float (tiles g (get_center g) Conf.Grid.r_country) in
    (* Printf.printf "tiles country %.0f\n%!" tiles_country; *)
    let tiles_city = float (tiles g (get_center g) Conf.Grid.r_city) in
    (* Printf.printf "tiles city %.0f\n%!" tiles_city; *)
    let a = 1. /. tiles_country in
    let avg = avg_w g (get_center g) (Conf.Grid.r_city *. 2.) in
    (* Printf.printf "avg pp %f\n%!" avg; *)
    let b = (1. -. (tiles_city /. tiles_country)) /. avg in
    (* Printf.printf "a: %f    b: %f\n%!" a b; *)

    let normalize_weight n =
      (a +. (Node.weight n) *. b)
      (* /. (a +. max_w *. b) *)
    in

    iter g (fun n ->
            Node.set_weight n (normalize_weight n));

    (* let cnt = ref 0 in *)
    (* iter g (fun n -> *)
    (*         if Node.weight n > 1. then cnt:=!cnt+1 else ()); *)
    (* Printf.printf "Nodes over 1: %i\n" !cnt; *)

    g

end
