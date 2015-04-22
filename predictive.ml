(* Copyright (C) 2014, Marco Stronati, All Rights Reserved.
   This file is distributed under the terms of the
   GNU General Public License version 3 or later. *)

open Geo
open Formats
open Traces

(*           
   PREDICTION
*)


(* super simple prediction *)
let super_simple_prediction obss =
  let (x0,y0) = fst (List.nth obss 0) in
  let (x1,y1) = fst (List.nth obss 1) in
  let x = x0 +. (x0 -. x1) in
  let y = y0 +. (y0 -. y1) in
  (x,y)


(* 
  least square prediction 
 *)

let project_point_to_line (x0,y0) (c0,c1) = 
  let x = ((c1 *. y0) +. x0 -. (c1 *. c0)) /. ((c1 ** 2.) +. 1.) in
  let y = c0 +. (x *. c1) in
  (x,y)

let rotate_pt angle pt = 
  let (x,y) = pt in
  ((x *. (cos (angle))) -. (y *. (sin (angle))),
   (x *. (sin (angle))) +. (y *. (cos (angle))))


let rotate angle pts = List.map (rotate_pt angle) pts

(* @param pts list of points representing a straight line 
   @return angle in radiants between [pi,-pi] of the inclination the line 
*)
let get_inclination pts = 
 
    let (x0,y0) = List.hd pts in
    let (xn,yn) = List.nth pts (List.length pts -1) in

    let direction_x = float (Util.sign_float (x0 -. xn)) in
    let direction_y = float (Util.sign_float (y0 -. yn)) in

    let slope = (y0 -. yn) /. (x0 -. xn) in
    let angle = atan2 ((abs_float slope) *. direction_y) direction_x in
    angle


open Gsl_fun
open Gsl_fit

(* @param obss observables, they must be at least two, last is more recent 
   @return point predicted
   
   we approximate straigth lines
   normally two points are enough to approximate a straight line withp=out noisy, in our case we ask at least 3
*)
let prediction_linear_least_square obss = 
  let all = fst (List.split obss) in
  let hards = fst (List.split (List.filter (fun (_,meta) -> meta.h) obss)) in
  let n = List.length hards in
  match n with
  | 0 -> ((Utm.make 0. 0.),0.)  (* no particular reason *)
  | 1 -> (List.hd all,0.)  (* just return the only observables we have *)
  | _ ->
     let all = (List.map (fun p -> Utm.tuple p) all) in
     let system_angle = get_inclination all in
     let all_rotated = rotate (-.system_angle) all in

    (* STRATEGY without weights *)
    (* let hards_rotated = rotate (-. system_angle) hards in *)
    (* let (hxs,hys) = List.split hards_rotated in *)
    (* let ahx = Array.of_list hxs in *)
    (* let ahy = Array.of_list hys in *)
    (* let coeffs = Gsl_fit.linear ahx ahy in *)
    (* let avg_error = coeffs.sumsq /. float n in *)


    (* STRATEGY with weights *)
    let weights = List.map (fun (_,meta) -> if meta.h
      then 1. /. ((Laplacian.expected_value_polar meta.e) ** 2.)
      else 1. /. (meta.l ** 2.)) obss in
    let (xs,ys) = List.split all_rotated in
    let ahx = Array.of_list xs in
    let ahy = Array.of_list ys in
    let aw = Array.of_list weights in
    let coeffs = Gsl_fit.linear ~weight:aw ahx ahy in
    let avg_error = coeffs.sumsq /. ((float n) *. (List.fold_left (fun tmp w -> tmp +. (w ** 2.)) 0. weights)) in


    let (xp,yp) = project_point_to_line (List.hd all_rotated) (coeffs.c0,coeffs.c1) in

    let step = Util.avg (intra_distances Utm.distance (List.map (fun (x,y) -> Utm.make x y) all_rotated)) in
    let direction_x = float (Util.sign_float (ahx.(0) -. ahx.(1))) in
    let direction_y = float (Util.sign_float (ahy.(0) -. ahy.(1))) in
    
    let angle = atan2 ((abs_float coeffs.c1) *. direction_y) direction_x in
    let x = xp +. ((cos angle) *. step) in
    let y = yp +. ((sin angle) *. step) in

    (* linear_est returns the standard_deviation too *)
    (* if coeffs.sumsq <> coeffs.sumsq then failwith "Prediction: non a number";  *)

    let (new_x,new_y) = rotate_pt system_angle (x,y) in
    let pt = Utm.make new_x new_y in
    (pt, avg_error)



(* 
  LINEAR PREDICTION 
 *)
      
let linear obss =
  match obss with
  | [] -> ((Utm.make 0. 0.),{d = 0; turn=false; pred_e = 0.})  (* no particular reason *)
  | (pt,_)::[] ->  (pt,{d = 1; turn=false; pred_e = 0.})  (* just return the only observables we have *)
  | _ -> 

    (* BEST ERROR STRATEGY *)
    (* let nhards = List.length (List.filter (fun (pt,meta) -> meta.h) obss) in *)
    (* let min_depth = if nhards > 2 then 3 else 2 in (* minimum should be 3 because prediction is linear thus there is no error for two points *) *)
    (* let max_depth = 30 in                      (* parameter *) *)
    (* let depths = Array.to_list (Array.init (max_depth-min_depth) (fun i -> min_depth+i)) in *)
    (* let values = List.combine depths (List.map (fun depth -> let sub_obss = sublist obss depth in *)
    (*                                                          prediction_linear_least_square sub_obss) depths) in *)
    (* let sorted = List.sort (fun (d1,(p1,e1)) (d2,(p2,e2)) -> compare e1 e2) values in *)
    (* let (depth,(pt,err)) = List.hd sorted in *)
    (* let turnpoint = false in *)

    (* TURNING DETECTION STRATEGY *)
    let rec until_last_turn l =
      match l with
        [] -> []
      | (pt,meta)::rest -> if meta.pred.turn then (pt,meta)::[] else (pt,meta)::(until_last_turn rest)
    in
    let sub_obss = until_last_turn obss in
    let (pt,err) = prediction_linear_least_square sub_obss in
    let avg_errs = Util.avg (List.map (fun (p,m) -> m.pred.pred_e) obss) in
    let turnpoint = if err > avg_errs *. 10. then true else false in (* param *)
    let depth = List.length sub_obss in


    (pt, {d = depth; turn = turnpoint; pred_e = err;})



(* 
  PARROT PREDICTION 
 *)

let parrot obss =
  match obss with
  | [] -> ((Utm.make 0. 0.),{d = 0; turn=false; pred_e = 0.})  (* no particular reason *)
  | (pt,_)::[] ->  (pt,{d = 1; turn=false; pred_e = 0.})  (* just return the only observables we have *)
  | _ -> 

    let rec first_hard l = match l with [] -> None | (pt,meta)::rest -> if meta.h then Some pt else first_hard rest in
    let (pt,depth) = 
      match first_hard obss with
        None -> (fst (List.hd obss),0)                 (* just return last easy observable *)
      | Some pt -> (pt,1)
    in
    let err = 0. in
    let turnpoint = false in

    (pt, {d = depth; turn = turnpoint; pred_e = err;}) (* error in this case is useless *)



let prediction = parrot






(* 
  Test function for the selection of a prediction
 *)

(* @param alpha prediction precision 
   @param pred  predicted point
   @param sec   secret point 
   @return true if the prediction is good 
*)
let theta alpha pred sec = if (Utm.distance pred sec) < alpha then true else false

(* differentially private version with laplacian noise *)
let theta_dp epsilon_theta alpha predicted secret =
  (* if alpha = infinity then true else  *)
  (*   if alpha = 0. then false else  *)
      let noise = Laplacian.noise_linear epsilon_theta in
      if (Utm.distance predicted secret) < (alpha +. noise) then true else false







(* 
  Budget Managers 
*)



(* todo move these constants *)
let c_i = (Laplacian.epsilon_of_radius_polar 1.)
let c_l = (Laplacian.epsilon_of_radius_linear 1.)

let skip_to_independent run = 
  match run with
    [] -> ((* print_string "skip_to_ind\n"; *) true)
  | _ -> false


let rec find_last_hard l = 
  match l with 
    [] -> None
  | (pt,meta)::rest -> if meta.h then Some (pt,meta) else (find_last_hard rest) 


let skip_to_prediction time alpha run = 
  let speed = 0.5 in  (* n_first*)                     (* km/h *) (*TODO check *)
  (* let speed = 0.15 in (\* u_first*\)                     (\* km/h *\) *)

  let last_hard = find_last_hard run in

  match last_hard with
    None -> false
  | Some (pt,meta) ->
    let period_calendar = CalendarLib.Calendar.sub time meta.t in
    let period_sec = CalendarLib.Time.Period.length (CalendarLib.Calendar.Period.safe_to_time period_calendar) in

    if period_sec < 0 then (Printf.printf "skip_to_pred wrong time %i sec\n" period_sec; false)
    else
      let distance = speed *. ((float period_sec) /. 60.) *. 1000. in
  
      if distance <= alpha
      then (
      (* Printf.printf "distance %f <= alpha %f\n" distance alpha; *)
        true)
      else false


(* returns a Budget Manager with Fixed Utility/Accuracy *)
let init_bm_fixed_utility epsilon alpha prediction_rate skip_enable = 
  let rho = 0.8 in                      (* noise vs threshold *)
  let eta = 0.5 in                      (* adjusts utility to prior, make it smaller to fit your prior, higher to cover all prior *)

  let n_ind = epsilon *. alpha /. c_i in

  let compute_min_pr eps_step = (c_l /. c_i) *. eta *. (1. +. (1. /. rho)) in
  let min_pr = compute_min_pr (epsilon /. n_ind) in

  if not (prediction_rate >= min_pr && prediction_rate <= 1.) then failwith (Printf.sprintf "Wrong prediction rate. [%f - 1]\n" min_pr)
  else
    fun time run ->
      let stat = statistics_run run in
      let epsilon_so_far = stat.e_so_far in
    
      if (epsilon -. epsilon_so_far < 0.000001)
      then ((* Printf.printf "End of budget\n"; *) (-1.,-1.,-1.))
      else if (skip_to_prediction time alpha run) && skip_enable
      then (0.,0.,alpha)
      else if (skip_to_independent run) && skip_enable
      then (0.,(epsilon /. n_ind),0.)   (* epsilon_{I'} *)
      else (
        (* if not (prediction_rate >= compute_min_pr (epsilon_step)) then Printf.printf "We are not gonna make it...\n"; *)

        (* let prediction_rate = if (stat.n_so_far >= 10) then stat.pr_so_far else prediction_rate in (\* TODO check *\) *)
        
        let epsilon_theta = eta *. (c_l /. alpha) *. (1. +. (1. /. rho)) in
        let epsilon_i = c_i /. alpha in
        let threshold = c_l /. (rho *. epsilon_theta) in
        
        (epsilon_theta, epsilon_i, threshold))


(* returns a Budget Manager with Fixed Rate *)
let init_bm_fixed_rate epsilon n prediction_rate skip_enable =
  let rho = 0.8 in                      (* noise vs threshold *)
  let eta = 0.6 in                      (* utility ind vs utility pred *)
  
  let compute_min_pr eps_step = (c_l /. c_i) *. eta *. (1. +. (1. /. rho)) in
  let min_pr = compute_min_pr (epsilon /. n) in

  let rate = epsilon /. n in
  if not (prediction_rate >= min_pr && prediction_rate <= 1.) then failwith (Printf.sprintf "Wrong prediction rate. [%f - 1]\n" min_pr)
  else
    fun time run ->

      let stat = statistics_run run in
      let n_so_far = float (stat.n_so_far) in
      let epsilon_so_far = stat.e_so_far in
      let delta_e = epsilon -. epsilon_so_far in

      let last_alpha = let hard = find_last_hard run in match hard with Some (pt,meta) -> meta.l | None -> 0. in
    
    if delta_e <= 0. || n_so_far = n
    then ((* Printf.printf "End of budget\n"; *) (-1.,-1.,-1.))
    else if n_so_far = (n-.1.)
    then (
      let epsilon_i = delta_e in
      let epsilon_theta = 0. in
      let threshold = 0. in
      (epsilon_theta, epsilon_i, threshold))
    else if (skip_to_prediction time (last_alpha *. 0.8 ) run) && skip_enable
    then (0.,0.,last_alpha)
    else if (skip_to_independent run) && skip_enable
    then (0.,(epsilon /. n),0.)
    else (
      (* if not (prediction_rate >= compute_min_pr (epsilon_step)) then Printf.printf "We are not gonna make it...\n"; *)
      
      let prediction_rate = if (n_so_far >= 10.) || (n_so_far >= n /. 4.) then stat.pr_so_far else prediction_rate in (* TODO check *)
      if n = n_so_far then failwith "bm n_first: you should have stopped before";

      let epsilon_i = rate /. ((1. -. prediction_rate) +. (c_l /. c_i) *. eta *. (1. +. (1. /. rho))) in
      let epsilon_theta = (c_l /. c_i) *. eta *. (1. +. (1. /. rho)) *. epsilon_i in
      let threshold = c_l /. (rho *. epsilon_theta) in
      (epsilon_theta, epsilon_i, threshold))




(* 
  Predictive mechanism
 *)



(* @param secs (x,y) the head is newest
   @return obss (x,y),(bool,epsilon,epsilon_theta,alpha) head is newest 
*)
let mechanism budget_manager secs =
  let rec mechanism_in idx obss secs = 
    match secs with
    | [] -> obss
    | sec::rest -> 
      let (epsilon_theta, epsilon_i, threshold) = budget_manager sec.time obss in

      (* Printf.printf "%i: epsilon_theta %f epsilon_i %f threshold %f\n" idx epsilon_theta epsilon_i threshold; *)
      if epsilon_i < epsilon_theta then Printf.printf "ei < et!!\n";

      let (predicted,params) = prediction obss in 

      let b = 
        if epsilon_theta = -1. then true                              (* stop condition *)
        else if epsilon_theta = 0. && epsilon_i = 0. then true        (* skip to pred *)
        else if epsilon_theta = 0. && epsilon_i > 0. then false       (* skip to ind *)
        else if epsilon_theta > 0. && epsilon_i > 0. then 
          if epsilon_theta <= epsilon_i then (theta_dp epsilon_theta threshold predicted sec.coord)
          else failwith "Budget manager spends more in et than ei."
        else  failwith "Budget manager returned wrong values."
      in

      let obs = 
        if b
        then 
          (* TODO here idx could come from meta_pre *)
          let meta = {i = idx; t = sec.time; h = false; e = epsilon_i; et = epsilon_theta; l = threshold; pred = params} in
          (predicted,meta)
        else 
          let noisy = Laplacian.noise_polar epsilon_i sec.coord in
          let meta = {i = idx; t = sec.time; h = true; e = epsilon_i; et = epsilon_theta; l = threshold; pred = params} in
          (noisy,meta)
      in
      mechanism_in (idx+1) (obs::obss) rest
  in
  let rsecs = List.rev secs in           (* head is the oldest secret *)
  mechanism_in 0 [] rsecs





(*  
    Indipendent Mechanism
*)

let mechanism_independent_noise epsilon secs = 
  List.map (fun sec -> Laplacian.noise_polar epsilon sec) secs


let mechanism_independent_noise_budget budget secs = 
  let epsilon_step = budget /. float (List.length secs) in
  mechanism_independent_noise epsilon_step secs
