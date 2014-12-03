(* Copyright (C) 2014, Marco Stronati, Konstantinos Chatzikokolakis, 
   All Rights Reserved.
   This file is distributed under the terms of the
   GNU General Public License version 3 or later. *)

(**
Heap enriched with a hash for fast random access.

When adding an existing element the one with a smaller weight is kept.
 *)

module type HashedHeap = sig
  type t
  val create : unit -> t
  val peek : t -> (int * float)
  val pop : t -> (int * float)
  val add : t -> (int * float) -> unit
  val is_empty : t -> bool
  val get : t -> int -> float
end

module ElasticArray = struct
  include BatDynArray
  let rec set a i el = 
    let l = BatDynArray.length a in
    if i >= l 
    then 
      let _ = BatDynArray.add a None in
      set a i el
    else
      BatDynArray.set a i el

end

module HashedHeap : HashedHeap = struct
  type elt = {
    k : int;
    v : float;
    mutable idx : int;     
  }

  type t = {
    mutable size : int;
    heap : elt option ElasticArray.t;
    hash : (int,elt) BatHashtbl.t
  }

  let print_heap l =
    let core = BatDynArray.fold_left (fun tmp el -> 
                                      let s = 
                                        match el with 
                                        | None -> "_"
                                        | Some elt -> Printf.sprintf "%i" elt.idx 
                                      in
                                      Printf.sprintf "%s,%s" tmp s) "" l
    in
    ("["^core^"]")

  let print_hash l =
    BatHashtbl.fold (fun k _ tmp -> Printf.sprintf "%s,%i" tmp k) l "" 

  let print hh =
        Printf.printf "size:%i\nheap:%s\nhash:%s\n" hh.size (print_heap hh.heap) (print_hash hh.hash)


  let create () =
    {size=0; heap=ElasticArray.create (); hash=BatHashtbl.create 10}

  let heap_get hh i =
    match ElasticArray.get hh.heap i with
      Some e -> e
    | None -> failwith "trying to access null heap element"

  let heap_set hh i el =
    ElasticArray.set hh.heap i el

  (* swap two elements in the heap *)
  let swap hh i j =
    let l = ElasticArray.length hh.heap in
    if i >= l || j >= l then failwith "bad args" else
    let eli = heap_get hh i in
    let elj = heap_get hh j in

    eli.idx <- j;
    elj.idx <- i;
       
    heap_set hh i (Some elj);
    heap_set hh j (Some eli)



  (* internal *)
  (* Assume the heap is correct everywhere except at node i *)
  (* Restore the property everywhere *)
  let rec restore_heap_property hh i =

    let par_i   = (i-1)/2 in
    let left_i  = 2*i + 1 in
    let right_i = 2*i + 2 in
    
    (* find min of two children (if any) *)
    let min_i =
      if right_i < hh.size && (heap_get hh left_i).v > (heap_get hh right_i).v
      then right_i
      else left_i
    in
    let _ = (heap_get hh i).v in
    let _ = (heap_get hh par_i).v in

    if i > 0 && (heap_get hh i).v < (heap_get hh par_i).v
    then (* smaller than parent, go up *)
      (swap hh i par_i;
       restore_heap_property hh par_i)
    else 
      if min_i < hh.size && (heap_get hh i).v > (heap_get hh min_i).v
      then (* bigger than a child, go down *)
	(swap hh i min_i;
	 restore_heap_property hh min_i)
      else ()                 (* everything ok *)



  (* inserts new object *)
  (* if key exists, it inserts only if value is smaller than previous element *)
  (* returns true if inserted *)
  let add hh (k,v) =
    let old = BatHashtbl.find_option hh.hash k in
    let el = 
      match old with
      | Some old -> if v < old.v
                    then {k=k; v=v; idx=old.idx} (* new value is better *)
                    else old                     (* old value is better *)
      | None -> 
         hh.size <- hh.size+1;
         {k=k; v=v; idx=(hh.size-1)}        (* none found *)
    in
    BatHashtbl.replace hh.hash k el;
    heap_set hh el.idx (Some el);
    restore_heap_property hh el.idx


  let is_empty hh = hh.size = 0


  (* returns the smallest object without removing *)
  let peek hh = 
    if is_empty hh 
    then raise (Invalid_argument "empty hashed heap")
    else 
      let res = heap_get hh 0 in
      (res.k,res.v)


  let pop hh = 
    if is_empty hh 
    then raise (Invalid_argument "empty hashed heap");
    hh.size <- hh.size -1;

    if hh.size > 0 
    then ( 
      swap hh 0 hh.size;
      restore_heap_property hh 0);

    let res = heap_get hh hh.size in
    heap_set hh hh.size None;
    res.idx <- -1;
    (res.k,res.v)
    
  let get hh k =
    let res = BatHashtbl.find hh.hash k in
    res.v
end

                                   
(* 
  TESTING 
*)

let test () = 

  let hh = HashedHeap.create () in

  let keyNo = 100 in
  let insertNo = int_of_float 1e6 in

  let mins = Hashtbl.create 50 in


  Printf.printf "starting, %i inserts, %i keys\n" insertNo keyNo;

  let dt = Util.get_time () in

  let empty_ok = ref true in
  if not (HashedHeap.is_empty hh) then empty_ok := false;

  for i = 0 to insertNo do
    let key = i mod keyNo in
    let value = floor (Random.float 1e8) in

    let v_old = 
      match BatHashtbl.find_option mins key with
        Some v -> v
      | None -> infinity
    in
    let v_new = min v_old value in
    Hashtbl.replace mins key v_new;

    HashedHeap.add hh (key,value);
    if HashedHeap.is_empty hh then empty_ok := false
  done;

  Printf.printf "insert finished in %i ms\n" (int_of_float ((Util.get_time () -. dt) *. 1000.));
 
  let hashOk = ref true in
  let isSorted = ref true in
  let popOk = ref true in

  (* check that hash access gets the correct value *)
  Hashtbl.iter (fun k v -> 
                let min = HashedHeap.get hh k in
                if min <> v then 
                  (Printf.printf "hash NOT ok got %f instead of %f\n" min v;
                   hashOk := false;)
               ) mins;
  
  (* check popping *)
  let last = ref 0. in
  let counter = ref 0 in
try
  while true do
    let (k,v) = HashedHeap.pop hh in         
    if v < !last then isSorted := false;
    if v <> (Hashtbl.find mins k) then popOk := false;
    
    last := v;
    counter := !counter +1;
  done
with
  Invalid_argument _ ->

  if !counter <> keyNo then popOk := false;

  empty_ok := HashedHeap.is_empty hh;

Printf.printf
  "tests:
   isSorted: %b
   hashOk:   %b 
   popOk:    %b
   emptyOk:  %b\n"

!isSorted
!hashOk
!popOk
!empty_ok


(* let _ = test () *)
