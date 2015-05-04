open Geo
open Formats

let compute_intra_speeds track = 
  let rec intra_speeds_in speeds track = 
      match track with
        [] -> failwith "Empty argument"
      | pt::[] -> speeds
      | pt1::pt2::rest -> 
  
        let period_calendar = CalendarLib.Calendar.sub pt1.time pt2.time in
        let period_sec = CalendarLib.Time.Period.length (CalendarLib.Calendar.Period.safe_to_time period_calendar) in
        let distance = Utm.distance pt1.coord pt2.coord in
        let speed = (distance /. 1000.) /. (float period_sec /. 3600.) in (* km/h *)
        intra_speeds_in (speed::speeds) (pt2::rest)
  in
  let res = intra_speeds_in [] track in
  List.rev res



(* 
  Pre-processing of the traces: sampling with different frequencies
*)


(* Takes the filename of a trace, creates a directory with a sample of the trace with prob_jump. 
  @param small_jump  time interval between two queries, default value
  @param big_jump    time interval between two queries, in case of jump
  @param accuracy    interval in which a sample is accepted wrt to the expected time
  @param prob_jump   probability to perform a big_jump 
*)

let sample small_jump big_jump accuracy prob_jump filename =

  let label_by_speed max_speed track =
    let rec label_by_speed_in labelled track =
      match track with
        [] -> failwith "Empty argument"
      | pt::[] -> labelled
      | pt1::pt2::rest ->
        
        let period_calendar = CalendarLib.Calendar.sub pt1.time pt2.time in
        let period_sec = CalendarLib.Time.Period.length (CalendarLib.Calendar.Period.safe_to_time period_calendar) in
        let distance = Utm.distance pt1.coord pt2.coord in
        let speed = (distance /. 1000.) /. (float (abs period_sec) /. 3600.) in (* km/h *)

        if speed <= max_speed
        then label_by_speed_in ((pt1, true)::labelled) (pt2::rest)
        else label_by_speed_in ((pt1,false)::labelled) (pt2::rest)
    in
    let res = label_by_speed_in [] (List.rev track) in
    res
  in


  let random_jump prob_jump small big = 
    let sigma = 0.2 in
    let gauss () = 1. +. (Gsl_cdf.gaussian_Pinv ~p:(Random.float 1.) ~sigma:sigma) in (* increase sigma for more variance *)
    
    let seconds = 
      if prob_jump >= (Random.float 1.)
      then big   *. (gauss ())
      else small *. (gauss ())
    in
    let step = CalendarLib.Calendar.Period.second (int_of_float seconds) in
    step
  in
  (* accuracy in minutes *)
  let find_closest accuracy timestamp points =
    let rec find_closest_in best_pt timestamp points =
      let accuracy = accuracy * 60 in            (* seconds *)

      let best_gap = CalendarLib.Calendar.sub timestamp (fst best_pt).time in
      let best_gap_sec = abs (CalendarLib.Time.Period.length (CalendarLib.Calendar.Period.safe_to_time best_gap)) in

      match points with
        [] -> 
          if best_gap_sec <= accuracy
          then (Some best_pt,[])                     (* always returns the last point *)
          else (None, [])
      | (pt,label)::rest ->
        let gap = CalendarLib.Calendar.sub timestamp pt.time  in
        let gap_sec = abs (CalendarLib.Time.Period.length (CalendarLib.Calendar.Period.safe_to_time gap)) in
        if gap_sec <= best_gap_sec
        then find_closest_in (pt,label) timestamp rest
        else
          if best_gap_sec <= accuracy
          then (Some best_pt,points)
          else (None, points)
    in
    find_closest_in (List.hd points) timestamp (List.tl points)
  in

  let subsample small_jump big_jump accuracy track =
    let rec subsample_in sampled timestamp rest_track =
      match rest_track with
        [] -> sampled
      | _ ->
        let small = small_jump *. 60. in                     (* param seconds *)
        let big   =   big_jump *. 60. in                     (* param seconds *)
        let point = List.hd sampled in
        let next_timestamp = CalendarLib.Calendar.add timestamp (random_jump prob_jump small big) in
        let closest = find_closest accuracy next_timestamp ((point,true)::rest_track) in

        match closest with
          None,[] -> sampled
        | None,rest_track -> subsample_in sampled next_timestamp rest_track
        | Some(_,false),rest_track -> 
            subsample_in sampled next_timestamp rest_track
        | Some(next_point,true),rest_track ->

          if next_point.idx = point.idx (* we are still *)
          then
            let new_point = {coord = point.coord; idx = point.idx; time = next_timestamp} in
            let next_sampled = new_point::sampled in
            let next_timestamp = (List.hd next_sampled).time in
            subsample_in next_sampled next_timestamp rest_track (* list gets inverted *)
          else
            let next_sampled = next_point::sampled in
            let next_timestamp = (List.hd next_sampled).time in 
            subsample_in next_sampled next_timestamp rest_track (* list gets inverted *)
    in
    let track_rev = List.rev track in
    let start = fst (List.hd track_rev) in
    subsample_in [start] start.time (List.tl track_rev)
  in

  let xml = (Xml.parse_file filename) in
  let track = track_of_gpx xml in       (* head is newest *)
  let max_speed = 15. in                (* param km *)
  let segments = label_by_speed max_speed track in
  let sampled = subsample small_jump big_jump accuracy segments in
  if (List.length sampled) > 5 
  then Some sampled
  else None



(* sample a directory of traces, creating for each prob of jumping a directory *)
let sample_traces small_jump big_jump accuracy src_dir dst_dir =
  let number_of_samples = 1 in     (* param *)
  let prob_jump_list = [0.0;0.1;0.2;0.3;0.4;0.5;0.6;0.7;0.8;0.9;1.0] in

  let input_traces_names = Array.to_list (Sys.readdir src_dir) in
  let n_input_traces = List.length input_traces_names in

  let src_dir = Util.deslash src_dir in
  let dst_dir = Util.deslash dst_dir in
  Util.mkdir dst_dir;

  let sample_prior prob_of_jump =
    let dst_dir_prior = Printf.sprintf "%s/%3.1f" dst_dir prob_of_jump in
    Util.mkdir dst_dir_prior;

    let _ = Util.parmap
      (fun input_trace_name -> 
        List.iter 
          (fun idx -> 
            let sampled_trace = sample small_jump big_jump accuracy prob_of_jump (src_dir^"/"^input_trace_name) in
            let filename = String.sub input_trace_name 0 (String.length input_trace_name -4) in
            match sampled_trace with
              Some trace -> xml_to_file (Printf.sprintf "%s/%s-%03i.gpx" dst_dir_prior filename idx) (gpx_of_track trace);
            | None -> ()) 
          (Util.enumerate number_of_samples))
      input_traces_names;
    in
    ()
  in
  let _ = List.iter sample_prior prob_jump_list in

  Printf.printf "Generated max %i samples.\n" (number_of_samples * n_input_traces * (List.length prob_jump_list))






(* 
  Filters tracks based on various properties so to have a clean dataset to perform the evaluation.
 *)

let filter filename_src filename_dst = 
  let xml = (Xml.parse_file filename_src) in
  let track = track_of_gpx xml in

  let length_filter track = 
    let min_length = 20 in                (* paramter number of points*)
    let length = List.length track in
    let bool_length = length >= min_length in
    bool_length
  in

  let period_filter track =
    let min_period = 20. in         (* param minutes*)
    let newest = (List.hd track).time in
    let oldest = (List.nth track (List.length track -1)).time in
    
    let period_calendar = CalendarLib.Calendar.sub newest oldest in
    let period_sec = CalendarLib.Time.Period.length (CalendarLib.Calendar.Period.safe_to_time period_calendar) in
    let period = (float period_sec) /. 60. in
    let bool_period = period >= min_period in
    bool_period
  in

  let speed_filter track = 
    let intra_speeds = compute_intra_speeds track in
    let query_speed = 15. in            (* param km/h *)
    let ratio = (float (List.length (List.filter (fun speed -> speed < query_speed) intra_speeds))) /. (float (List.length track -1)) in
    let bool_speed = ratio >= 0.1 in
    bool_speed
  in

  let rec apply_filters track filters =
    match filters with
      [] -> true
    | filter::rest -> 
      let passed = filter track in
      if passed then apply_filters track rest
      else false
  in
  let passed = apply_filters track [length_filter;period_filter;speed_filter] in

  (* Printf.printf " %s  length %4.i  Period %4.f  ratio %6.4f  " filename length period ratio; *)
  if passed 
  then xml_to_file filename_dst (gpx_of_track track)
  else ()




(* 
  Computes some statistics on a track
 *)
let trace_stat filename _ = 
  let xml = (Xml.parse_file filename) in
  let track = track_of_gpx xml in

  let length = List.length track in

  let newest = (List.hd track).time in
  let oldest = (List.nth track (List.length track -1)).time in
  
  let period_calendar = CalendarLib.Calendar.sub newest oldest in
  let period_sec = CalendarLib.Time.Period.length (CalendarLib.Calendar.Period.safe_to_time period_calendar) in
  let period = (float period_sec) /. 60. in

  let intra_distances = (intra_distances Utm.distance (List.map (fun pt -> pt.coord) track)) in
  let distance = List.fold_left (+.) 0. intra_distances in
  let speed = (distance /. 1000.)  /. (period /. 60.) in (* km/h *)

  let intra_speeds = compute_intra_speeds track in
  let query_speed = 15. in
  let ratio = (float (List.length (List.filter (fun speed -> speed < query_speed) intra_speeds))) /. (float (length-1)) in
  let max_speed = BatList.max intra_speeds in
  (* List.iter (fun d -> Printf.printf "%f " d) (compute_intra_speeds track); Printf.printf "\n"; *)
  Printf.printf " %s length %5.i  distance %9.1f  Period %4.f  speed %5.1f  maxspeed %5.1f  ratio %5.1f\n" filename length (distance) period speed max_speed ratio














(* 
   post-processing

*)

open Laplacian

let compute_errors secs obss = 
  let distances = BatList.map2 (fun sec (pt,meta) -> Utm.distance sec.coord pt) secs obss in
  distances


type stat_run = {
  n_so_far : int; 
  pr_so_far : float; 
  e_so_far : float; 
  skipped_so_far : float; 
  et_so_far : float; 
  ei_so_far : float;
}
type stat = {
  n : float; 
  pr : float; 
  avg_u : float; 
  avg_u_2 : float; 
  avg_e : float; 
  avg_l : float; 
  bpp : float; 
  skipped : float; 
  et_tot : float; 
  ei_tot : float;
}

let sprint_stat stat =
  (* Printf.sprintf "n:%f pr:%5.1f%% avg_u:%6.4f avg_e:%7.2f %7.2f\n" *)
  Printf.sprintf "%f %5.1f %6.4f %7.2f %7.2f"
    stat.n (stat.pr *. 100.) stat.avg_u stat.avg_e stat.bpp


let compute_percentile value list = 
  let sorted = List.sort compare list in
  let length = float (List.length list) in
  let n = (length *. value /. 100.) +. 0.5 in
  let n_rounded = if (n -. (floor n)) < 0.5 then floor n else ceil n in
  List.nth sorted (int_of_float n_rounded)


let chop_bad_part secs obss = 
  let elaborated_obss = List.filter (fun (pt,meta) -> if meta.et = -1. then false else true) obss in

  let n = List.length elaborated_obss in

  let elaborated_secs = BatList.drop ((List.length secs) - n) secs in
  (elaborated_secs,elaborated_obss)
  

let statistics_run obss =

  let elaborated_obss = List.filter (fun (pt,meta) -> if meta.et = -1. then false else true) obss in

  let n = List.length elaborated_obss in
  let et_tot = List.fold_left (fun tot (_,meta) -> tot +. meta.et) 0. elaborated_obss in
  let ei_tot = List.fold_left (fun tot (_,meta) -> tot +. if meta.h then meta.e else 0.) 0. elaborated_obss in
  let e_tot = et_tot +. ei_tot in

  (* let wrongs = List.length (List.filter (fun (_,meta) -> meta.et >= meta.e) elaborated_obss) in *)
  (* if wrongs > 0 then Printf.printf "Sbagliai: %f%%\n" ((float wrongs) /. (float n)); *)

  let hards = List.fold_left (fun tot (pt,meta) -> if meta.h then tot+1 else tot) 0 elaborated_obss in
  let prediction_rate = (float (n - hards)) /. (float n) in

  let skipped_obss = List.filter (fun (pt,meta) -> if meta.et = 0. && meta.e = 0. then true else false) elaborated_obss in
  let skipped = (float (List.length skipped_obss)) /. (float n) in
  
  {n_so_far = n; pr_so_far = prediction_rate; e_so_far = e_tot; skipped_so_far = skipped; et_so_far = et_tot; ei_so_far = ei_tot}


(* head is the newest *)
let statistics secs obss =

  (* let elaborated_obss = List.filter (fun (pt,meta) -> if meta.et = -1. then false else true) obss in *)

  (* let n = List.length elaborated_obss in *)

  (* let hards = List.fold_left (fun tot (pt,meta) -> if meta.h then tot+1 else tot) 0 elaborated_obss in *)
  (* let prediction_rate = (float (n - hards)) /. (float n) in *)

  (* let skipped_obss = List.filter (fun (pt,meta) -> if meta.et = 0. && meta.e = 0. then true else false) elaborated_obss in *)
  (* let skipped = (float (List.length skipped_obss)) /. (float n) in *)

  (* let e_tot = List.fold_left (fun tot meta -> if meta.h then tot+.meta.e else tot) 0. real_metas in *)
  (* let et_tot = List.fold_left (fun tot meta -> if meta.l = 0. then tot else tot+.meta.et) 0. real_metas in *)

  let stat = statistics_run obss in
  let n = stat.n_so_far in
  let prediction_rate = stat.pr_so_far in
  let skipped = stat.skipped_so_far in

  let elaborated_obss = BatList.drop ((List.length obss) - n) obss in
  let elaborated_secs = BatList.drop ((List.length secs) - n) secs in

  let utilities = List.map (fun (pt,meta) -> 
    if meta.et = 0. && meta.e <> 0. then worst_noise_polar meta.e 
    else max (worst_noise_polar meta.e) ((worst_noise_linear meta.et) +. meta.l)) 
    elaborated_obss 
  in
  if List.length utilities = 0 then Printf.printf "utilities: elab_obss %i  secs %i obss %i \n " (List.length elaborated_obss) (List.length secs) (List.length obss);
  let avg_u = Util.avg utilities in

  let utilities_2 = List.map (fun (pt,meta) -> 
    if meta.et = 0. && meta.e <> 0. then worst_noise_polar meta.e 
    else alpha_of_delta 0.1 stat.pr_so_far (meta.et,meta.e,meta.l))
    elaborated_obss 
  in
  if List.length utilities_2 = 0 then Printf.printf "utilities2: elab_obss %i  secs %i obss %i \n " (List.length elaborated_obss) (List.length secs) (List.length obss);
  let avg_u_2 = Util.avg utilities_2 in

  let errors = compute_errors elaborated_secs elaborated_obss in
  if List.length errors = 0 then Printf.printf "List errors vuota\n";
  let avg_e = Util.avg errors in
  (* let max_e = List.fold_left (fun tmp err -> max tmp err) (-. infinity) errors in *)
  (* let max_e = compute_percentile 90. errors in *)

  let bpp = stat.e_so_far /. (float stat.n_so_far) in (* bpp *)
  let avg_l = Util.avg (List.map (fun (pt,meta) -> meta.l) elaborated_obss) in

  {n = (float n); pr = prediction_rate; avg_u = avg_u; avg_u_2 = avg_u_2; avg_e = avg_e; avg_l=avg_l; bpp = bpp; skipped = skipped; et_tot = stat.et_so_far; ei_tot = stat.ei_so_far}


let average_stat stats =
  let n_stat = float (List.length stats) in
  let zero = {n = 0.; pr = 0.; avg_u = 0.; avg_u_2 = 0.; avg_e = 0.; avg_l = 0.; bpp = 0.; skipped = 0.; et_tot = 0.; ei_tot = 0.} in
  let sum = List.fold_left (fun sum stat ->
    {n = sum.n +. stat.n;
     pr = sum.pr +. stat.pr;
     avg_u = sum.avg_u +. stat.avg_u;
     avg_u_2 = sum.avg_u_2 +. stat.avg_u_2;
     avg_e = sum.avg_e +. stat.avg_e;
     avg_l = sum.avg_l +. stat.avg_l;
     bpp = sum.bpp +. stat.bpp;
     skipped = sum.skipped +. stat.skipped;
     et_tot = sum.et_tot +. stat.et_tot;
     ei_tot = sum.ei_tot +. stat.ei_tot;
    }) zero stats in

  {n = sum.n /. n_stat;
   pr = sum.pr /. n_stat;
   avg_u = sum.avg_u /. n_stat;
   avg_u_2 = sum.avg_u_2 /. n_stat;
   avg_e = sum.avg_e /. n_stat;
   avg_l = sum.avg_l /. n_stat;
   bpp = sum.bpp /. n_stat;
   skipped = sum.skipped /. n_stat;                   (* this is not averaged *)
   et_tot = sum.et_tot /. n_stat;
   ei_tot = sum.ei_tot /. n_stat}


(* @return index of stat in stats closest to avg of stats *)
let representative_stat stats =
  let avg = average_stat stats in
  let distances_stats = List.map (fun stat ->
    {n = abs_float (stat.n -. avg.n);
     pr = abs_float (stat.pr -. avg.pr);
     avg_u = abs_float (stat.avg_u -. avg.avg_u);
     avg_u_2 = abs_float (stat.avg_u_2 -. avg.avg_u_2);
     avg_e = abs_float (stat.avg_e -. avg.avg_e);
     avg_l = abs_float (stat.avg_l -. avg.avg_l);
     bpp = abs_float (stat.bpp -. avg.bpp); (* this is ignored in the sorting *)
     skipped = abs_float (stat.skipped -. avg.skipped); (* ignored *)
     et_tot = abs_float (stat.et_tot -. avg.et_tot);
     ei_tot = abs_float (stat.ei_tot -. avg.ei_tot);
     }) stats in
  let distance_stats = List.map (fun stat ->
    (* stat.n +. stat.pr +. stat.avg_u +. stat.avg_e)  *)
    stat.avg_e)
    distances_stats 
  in
  let indexed = Util.indicize distance_stats in
  let sorted = List.sort (fun (_,e1) (_,e2) -> compare e1 e2) indexed in
  fst (List.hd sorted)








(*
   GENERATION OF RANDOM TRACKS
*)

(*
  @param start list of point to start from
  @param direction to take expressed as angle in degrees
  @param step between each point
  @param length of the resulting segment
  @return straight segment added to the start list
*)
(* let go start direction step length = *)
(*   let rec go_in now length tmp = *)
(*     if length <= 0 then (now::tmp) *)
(*     else *)
(*       let next = destination now direction step in *)
(*       go_in next (length-1) (now::tmp) *)
(*   in *)
(*   go_in (List.hd start) length (List.tl start) *)


  


(* let random_jump variance small big =  *)
(*   let sigma = 0.2 in *)
(*   let gauss () = 1. +. (Gsl_cdf.gaussian_Pinv ~p:(Random.float 1.) ~sigma:sigma) in (\* increase sigma for more variance *\) *)
  
(*   if variance >= (Random.float 1.) *)
(*   then big   *. (gauss ()) *)
(*   else small *. (gauss ()) *)


(* (\* Random walk with gaussian distribution for the angle. *\) *)
(* let random_direction variance =  *)
(*   let gauss () = 1. +. (Gsl_cdf.gaussian_Pinv ~p:(Random.float 1.) ~sigma:variance) in (\* increase sigma for more variance *\) *)
  
(*   float (int_of_float ((gauss ()) *. 360.)) *)


(* let make_random_track jump_prob small big length =  *)
(*   let p1_ll = (48.84437,2.332964) in    (\* paris *\) *)
(*   let start = xy_of_latlon p1_ll in *)

(*   let t1 = Util.iterate (fun () -> ((random_direction 50.),(random_jump jump_prob small big),1)) length in *)

(*   let track_metaless = List.fold_left (fun tmp (direction, step, length) -> go tmp direction step length) (start::[]) t1 in *)
  
(*   let track = BatList.mapi (fun i pt -> {coord = pt; idx=i; time=default_timestamp} ) track_metaless in *)

(*   (\* let filename = Printf.sprintf "tmp/straight-line-%f-%i-%i" jump_prob length (Random.int 999) in *\) *)
(*   (\* (\\* dump gpx *\\) *\) *)
(*   (\* xml_to_file (filename^".gpx") (gpx_of_tracks (track::[])); *\) *)

(*   (\* (\\* dump geojson *\\) *\) *)
(*   (\* geojson_to_file (filename^".json") (geojson_of_simple_track track); *\) *)

(*   (\* List.map (latlon_of_xy) track *\) *)
(*   track *)
