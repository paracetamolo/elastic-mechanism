(* Copyright (C) 2014, Marco Stronati, All Rights Reserved.
   This file is distributed under the terms of the
   GNU General Public License version 3 or later. *)

(** 
   Utils.
*)

type 'a option = Some of 'a | None

let pi = 4. *. atan 1.                (* 3.14... *)

let sign x = if x = 0 then 0 else if x < 0 then -1 else 1
let sign_float x = if x = 0. then 0 else if x < 0. then -1 else 1

                                                          
let avg xs = 
  match xs with 
    [] -> failwith "avg Empty list"
  | _  -> (List.fold_left (fun sum x -> sum +. x) 0. xs) /. (float (List.length xs))


let variance xs = 
  (List.fold_left (fun sum x -> sum +. abs_float (x -. avg xs)) 0. xs) /. (float (List.length xs))


(* indicize elements of a list with their position, same value as List.nth *)
let indicize l = 
  let rec indicize_in l n =
    match l with 
    | [] -> failwith "indicize: empty list"
    | e::[] -> (n,e)::[]
    | e::es -> (n,e)::(indicize_in es (n+1))
  in
  indicize_in l 0


let rec sublist list n = 
  match list with 
    [] -> []
  | hd::rest -> 
    if n>0 then hd::(sublist rest (n-1)) 
    else if n == 0 then []
    else failwith "Out of bound"


let enumerate n = 
  let rec enumerate_in tmp n =
    if n=0 then 0::tmp
    else enumerate_in (n::tmp) (n-1)
  in
  enumerate_in [] (n-1)


(* lexicographic order *)
let min2 (a1,b1) (a2,b2) = if a1 < a2 then (a1,b1) else if a1 == a2 then (a1,min b1 b2) else (a2,b2)

let compare_fst (a,_) (b,_) = compare a b
let compare_snd (_,a) (_,b) = compare a b
                                      
        

let string_of_int_list l = 
  let core = List.fold_left (fun tmp i -> Printf.sprintf "%s%i," tmp i) "" l in
  ("["^core^"]")
                 

let salt () = 
  let len = 5 in
  let string = (Printf.sprintf "%.3f" (Unix.gettimeofday ())) in
  let after = String.sub string (String.length string -3) 3 in
  let before = String.sub string (String.length string -(len+1)) (len-3) in
  before^after


let deslash string = 
  let n = String.length string in
  if string.[(n -1)] = '/' 
  then (String.sub string 0 (n-1))
  else string


let list_dir dir = Array.to_list (Sys.readdir dir);;


let get_time () = Unix.gettimeofday ()

let string_of_time start = 
  let open Unix in
  let stop =  Unix.gettimeofday () in
  let res = stop -. start in
  let s = if res < 1.
          then Printf.sprintf "%ims" (int_of_float (res *. 1000.))
          else let tm = Unix.gmtime res in
               Printf.sprintf "%ih%im%is" tm.tm_hour tm.tm_min tm.tm_sec
  in
  (s,stop)

let print_time start = 
  let s,t = string_of_time start in
  Printf.printf "%s\n%!" s;
  t

let print_wallclock () = 
  let open Unix in
  let tm = Unix.localtime (Unix.time ()) in
  Printf.printf "%i:%i:%i\n" tm.tm_hour tm.tm_min tm.tm_sec



let new_print_over () = 
  let string_length = ref 0 in
  
  let print_over s = 
    string_length := String.length s;
    while !string_length > 0 do
      string_length := !string_length -1;
      Printf.eprintf "\b"
    done;
    Printf.eprintf "%s%!" s;
  in
  print_over



let file_exists name = 
  let open Unix in
  try 
    Unix.access name [R_OK];
    true
  with _ -> false


(* just plots a function in a certain interval *)

let plot_function f file =
  if file_exists file then () else
    let open Batteries in
    
    let start,step,stop = 0.,0.1,3. in
    let radii1 = List.of_enum ((start,step) --. stop) in
    let start,step,stop = 3.,1.,10. in
    let radii2 = List.of_enum ((start,step) --. stop) in
    let radii = radii1@radii2 in

    let tab = List.map (fun r -> 
                        (r, (f r))) 
                       radii 
    in
    let oc = open_out file in
    Printf.fprintf oc "# start:%f step:%f stop:%f\n" start step stop;
    List.iter (fun (r,pp) ->  
               Printf.fprintf oc "%f %f\n" r pp)
              tab;
    close_out oc



type values = {
  min : float;
  avg : float;
  max : float;
  sum : float;
  perc: float;
  sd  : float
}

let my_float_compare (a:float) (b:float) =
  if BatFloat.approx_equal ?epsilon:None a b
  then 0
  else if a > b then 1 else -1  (* speedup replacing Float.compare with > *)

  let stat_simple l =
    let w_min = ref infinity in
    let w_max = ref (-1.) in
    let w_sum = ref 0. in
    List.iter (fun w ->
            w_min := BatFloat.min w !w_min;
            w_max := BatFloat.max w !w_max;
            w_sum := w +. !w_sum;
           ) l;
    let w = {min = !w_min;
             max = !w_max;
             avg = (!w_sum /. (float (List.length l)));
             sum = !w_sum;
             perc = 0.;
             sd = 0.}
    in
    w

  (* simple stat + standard deviaton + 95th percentile *)
  let stat l =
    let len = List.length l in
    let st = stat_simple l in
    let sd_sum = ref 0. in
    List.iter (fun w ->
            let sd = (w -. st.avg) ** 2. in
            sd_sum := sd +. !sd_sum;
           ) l;
    let sorted = List.sort (fun a b -> - (my_float_compare a b)) l in
    let pos = len * 5 / 100 in
    let perc = List.nth sorted pos in
    let w = {min = st.min;
             max = st.max;
             avg = st.avg;
             sum = st.sum;
             perc = perc;
             sd = sqrt (!sd_sum /. (float len));}
    in
    w

let string_of_values s =
  Printf.sprintf "min %10f  avg %10f  max %10f  perc %10f  sd %10f" s.min s.avg s.max s.perc s.sd

let parmap f l = 
  (* Parmap.set_default_ncores 1; *)
  (* Parmap.parmap f (Parmap.L l) *)
  List.map f l


let repeat f n = 
  let rec repeat_in f n tmp =
    if n = 0
    then tmp
    else repeat_in f (n-1) ((f ())::tmp)
  in
  List.rev (repeat_in f n [])
