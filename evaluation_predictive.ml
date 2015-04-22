(* parmap is disabled because it crashes the program, maybe the update to ocaml 4.00 did it. *)

(* before running experiments:
   - set number of cores in parmap to the cpus in /proc/cpuinfo
   - sample_traces: 
     - set number_of_samples - the number of samples to generate for each track
     - prob_jump_list - the probability of jumping to use to sample the whole set
   - evaluation_predictive: set number_of_runs to number of runs over the same trace that then are averaged.

   note: 
   - number of resulting tracks is prob_jump_list X number_of_samples, 
   - number of runs of the mechanism is prob_jump_list X number_of_samples X number_of_runs

   Additionally set:
   - rho and eta in the bm_fixed_utility and bm_fixed_rate, just if you know what you are doing
   - speed in skip_to_prediction
   - epsilon, radius_safe in evaluation_predictive
*)

(* compile statically, including gsl library *)
(* ocamlopt -I +gsl gsl.cmxa geo_stripped.ml -noautolink -cclib '-Wl,-Bstatic -lmlgsl -lgsl -lgslcblas -Wl,-Bdynamic -lz' -verbose *)

(* http://stackoverflow.com/questions/2638664/is-there-any-free-ocaml-to-c-translator *)
(* ocamlc -output-obj -o foo.c foo.ml *)
(* gcc -L/usr/lib/ocaml foo.c -lcamlrun -lm -lncurses *)

(* general: head new stuff, tail old stuff *)

(* merkartor projection vs. utm
   todo expected error of linear and polar laplacian, to use in weights of prediction and comparison with independence noise 
   todo prediction can use easy points, because they passed the test and so, removed the expected error of the linear laplacian with respect to alpha they where pretty close to the secret
   todo sanity checks, relations of parameters, this can be put in the meta
 *)


open Geo
open Formats
open Predictive
open Traces

type result = {
  i : int; 
  obs : (Utm.t * meta) list; 
  sampled : point list; 
  stat : Traces.stat
}

let evaluate_predictive pri u n skip_enable directory =
  let radius_safe = 100. in             (* param *)
  let global_budget = 10. in
  let epsilon = (log global_budget) /. radius_safe in (* param *)
  let number_of_runs = 3 in        (* param *)
  let strict = false in             (* only accept traces long enough *)

  if (u = 0. && n = 0.) || (u <> 0. && n <> 0.) then failwith "Wrong parameters.";
  let fixed_utility = if n = 0. then true else false in

  let directory = Util.deslash directory in

  let bm = if fixed_utility 
           then init_bm_fixed_utility epsilon u pri skip_enable 
           else init_bm_fixed_rate epsilon n pri skip_enable in
  let mechanism = mechanism bm in

  let prob_of_jump_list = List.sort compare (Util.list_dir directory) in
  let number_of_samples_per_prior = List.length (Util.list_dir (directory^"/"^(List.hd prob_of_jump_list)^"/")) in


  let average_over_prior prob_of_jump =

    let average_over_track sample_path =
      let sampled_track = track_of_gpx (Xml.parse_file sample_path) in
      let name = Filename.chop_extension sample_path in
      Printf.printf "%s\n" name; flush_all ();

      (* Util.mkdir "tmp/"^(Filename.dirname name); *)
      (* let filename = "tmp/"^name^"-sec" in *)
      (* geojson_to_file (filename^".json") (geojson_of_simple_track sampled_track); *)
      (* xml_to_file (filename^".gpx") (gpx_of_track sampled_track); *)


      let run_mechanism () =
        let obs = mechanism sampled_track in
        let stat = Traces.statistics sampled_track obs in
        if fixed_utility && strict then
          (let (_,meta) = List.hd obs in
          if meta.et = (-. 1.)
          then Some (obs,stat)
          else None)
        else Some (obs,stat)
      in
      
      if (not fixed_utility) && (float (List.length sampled_track) < n) && strict
      then None
      else(
        let option_obs_stats = Util.repeat (run_mechanism) number_of_runs in
        let (some_obs_stats,none_obs_stats) = List.partition (fun x -> match x with Some _ -> true | None -> false) option_obs_stats in
        if (List.length none_obs_stats) > 0
        then None
        else (
          let obs_stats = List.map (fun x -> match x with Some x -> x | _ -> failwith "bad filter") some_obs_stats in
          
          let (obss,stats) = List.split obs_stats in
          
          (* complete version *)
          (* let stat = average_stat stats in *)
          (* let errors = List.flatten (List.map (fun obs -> let (s,o) = chop_bad_part sampled_track obs in compute_errors s o) obss) in *)

          (* short version *)
          let idx = Traces.representative_stat stats in
          let stat = List.nth stats idx in
          let obs = List.nth obss idx in

          (* let filename = "tmp/"^name^"-obs" in *)
          (* geojson_to_file (filename^".json") (geojson_of_rich_track obs); *)
          (* xml_to_file (filename^".gpx") (gpx_of_rich_track obs); *)

          let errors = let (s,o) = Traces.chop_bad_part sampled_track obs in Traces.compute_errors s o in
          Some (stat,errors)))
    in

    let samples_paths = List.map (fun name -> directory^"/"^prob_of_jump^"/"^name) (Util.list_dir (directory^"/"^prob_of_jump)) in
    let option_err_stats = Util.parmap average_over_track samples_paths in
    let (some_err_stats,none_err_stats) = List.partition (fun x -> match x with Some _ -> true | None -> false) option_err_stats in
    let err_stats = List.map (fun x -> match x with Some x -> x | _ -> failwith "bad filter") some_err_stats in
    let none_length = List.length none_err_stats in
    let (stats,errors) = List.split err_stats in
    match stats with
      [] -> None 
    | _ -> 
      let percentile = Traces.compute_percentile 90. (List.flatten errors) in
      let avg_all = Util.avg (List.flatten errors) in
      let stat = Traces.average_stat stats in
      (* Printf.printf "prior %s      avg_e %f     95 percentile %f     length %f     avg_all %f\n"  *)
        (* prob_of_jump stat.avg_e percentile stat.n avg_all; *)
      Printf.printf "%s " prob_of_jump; flush_all ();
      Some (stat,percentile, avg_all, none_length)
  in
  let results = List.map average_over_prior prob_of_jump_list in
  Printf.printf "\n"; flush_all ();

  (* compute info of independent mechanism *)
  let epsiloni_indep = if u = 0. then epsilon /. n else Laplacian.epsilon_of_radius_polar u in
  let n_indep = epsilon /. epsiloni_indep in
  let avg_u_indep = Laplacian.worst_noise_polar epsiloni_indep in
  let avg_e_indep = Laplacian.expected_value_polar epsiloni_indep in
  let bpp_ind = epsilon /. n_indep in
  let ppp_ind = bpp_ind /. epsilon *. 100. in

  (* log statistics *)
  let dir = directory^"-sanitized" in                    (* param *)
  Util.mkdir dir;

  let param_string = (Printf.sprintf "-%.2f-%.0f-%.0f" pri u n)^(if skip_enable then "-skip" else "-noskip") in

  let filename = "all"^param_string in
  let log = open_out (dir^"/"^filename^".log") in

  output_string log (Printf.sprintf "#pri:%.2f u:%.0f n:%.0f %s   %i samples\n" pri u n (if skip_enable then "skip" else "noskip") number_of_samples_per_prior);
  output_string log "#jump  coverage  pr    avg_e  avg_e_ind  avg_n  n_ind     avg_u   avg_u_2   percentile avg_u_ind   ppp   ppp_ind  skipped  test avg_l\n";
  output_string log (List.fold_left2 (fun s prob_of_jump result ->
    match result with 
      Some (stat,percentile,avg_all,none_length) ->
        let coverage = (float (number_of_samples_per_prior - none_length)) /. (float number_of_samples_per_prior) in
        let ppp = stat.bpp /. epsilon *. 100. in
        let test = stat.et_tot /. (stat.et_tot +. stat.ei_tot) *. 100. in
          s^(Printf.sprintf " %s    %3.3f   %3.2f  %7.2f  %7.2f   %5.1f  %5.1f    %8.2f  %8.2f  %8.2f  %8.2f  %5.2f   %5.2f    %5.2f   %5.2f %8.2f\n"
              prob_of_jump coverage (stat.pr *. 100.) avg_all avg_e_indep stat.n  n_indep stat.avg_u stat.avg_u_2 percentile avg_u_indep ppp ppp_ind (stat.skipped *. 100.) test stat.avg_l)
    | None -> s^(Printf.sprintf " %s  \n" prob_of_jump))
                       "" prob_of_jump_list results);
  close_out log;


  let log = open_out (dir^"/"^filename^".plot") in

  output_string log (Printf.sprintf "pri=%.2f; u=%.0f; n=%.0f; number_of_traces=%i; file=\"%s.log\"; set title \"%s\"\n set output \"%s.png\"\n" pri u n number_of_samples_per_prior filename filename filename);
  close_out log








let sample_stat directory = 
  let directory = Util.deslash directory in

  let priors = List.sort compare (Util.list_dir directory) in

  let avg_lengths = Util.parmap (fun prior -> 
    let traces = Util.list_dir (directory^"/"^prior) in
    let lengths = List.map (fun trace ->   
      let track = track_of_gpx (Xml.parse_file (directory^"/"^prior^"/"^trace)) in
      let l = List.length track in
      if l = 0 then failwith "Zero length trace!";
      l)
      traces
    in
    Util.avg (List.map float lengths))
    priors
  in
  List.iter2 (fun prior length -> Printf.printf "%s %f\n" prior length) priors avg_lengths


(* todo: this shoudl be applied to sample_traces too *)
let do_on_a_dir func src_dir dst_dir =
  let input_filenames = Array.to_list (Sys.readdir src_dir) in
  (* let n_input_traces = List.length input_traces_names in *)

  let src_dir = Util.deslash src_dir in
  let dst_dir = Util.deslash dst_dir in
  Util.mkdir dst_dir;

  let _ = Util.parmap
    (fun input_filename -> 
      (* Printf.printf "doing on a dir %s\n" input_filename; *)
      let input_path  = src_dir^"/"^input_filename in
      let output_path = dst_dir^"/"^input_filename in
      func input_path output_path
    )
    input_filenames
  in 
  ()


(* Converts a directory tree populated with plt files to gpx
   Typically run on the Data directory of the geolife dataset
 *)
let rec gpx_of_plt_dir src_dir dst_dir =

  let src_dir = Util.deslash src_dir in
  let dst_dir = Util.deslash dst_dir in
  Util.mkdir dst_dir;

  let filenames = Sys.readdir src_dir in

  Array.iter
    (fun filename ->
     let src = (src_dir^"/"^filename) in
     if Sys.is_directory src
     then gpx_of_plt_dir src (dst_dir^"/"^filename)
     else 
       let dst = dst_dir^"/"^(Filename.chop_extension filename)^".gpx" in
       Formats.gpx_of_plt src dst
    )
    filenames


(* check that 90th percentile and worst_noise_polar coincide *)
let indep () =
  let p1_ll = Wgs.make 48.84437 2.332964 in    (* paris *)
  let point = Utm.of_latlon p1_ll in

  let epsilon = 0.1 in

  let errors = Util.repeat (fun () ->
    let noisy = Laplacian.noise_polar epsilon point in
    Utm.distance noisy point) 
    500000
  in
  let avg = Util.avg errors in
  let max = Laplacian.worst_noise_polar epsilon in
  let perc = compute_percentile 90. errors in
  Printf.printf "avg %f   max %f    perc %f\n" avg max perc;
  ()




let _ =
  let argv = Sys.argv in
  if (Array.length argv) = 0 then failwith "No arguments";
      
  let command = argv.(1) in
  let src_dir = Util.deslash argv.(2) in

  match command with

  | "geolife2gpx" ->             (* src_dir must be Data directory of geolife *)
     let dst_dir = Util.deslash argv.(3) in
     gpx_of_plt_dir src_dir dst_dir

  | "tdrive2gpx" ->             (* TODO *)
     let src_file = argv.(2) in
     let dst_file = argv.(3) in
     gpx_of_tdrive src_file dst_file
  
  | "filter" -> (
      let dst_dir = src_dir^"-filtered" in
      do_on_a_dir Traces.filter src_dir dst_dir;
      do_on_a_dir Traces.trace_stat dst_dir dst_dir;
      ())

  | "sample" -> (
    Printf.printf "sample\n";
    let small = float_of_string argv.(3) in
    let big = float_of_string argv.(4) in
    let accuracy = int_of_string argv.(5) in
    let dst_dir = Printf.sprintf "%s-sampled-%i-%i-%i" src_dir (int_of_float small) (int_of_float big) accuracy in

    sample_traces small big accuracy src_dir dst_dir;
    sample_stat dst_dir;
    ())

  | "run" -> (
      Printf.printf "run\n";
      let pr = float_of_string argv.(3) in
      let u = float_of_string argv.(4) in
      let n = float_of_string argv.(5) in
      let skip = bool_of_string argv.(6) in
 
      evaluate_predictive  pr  u  n  skip  src_dir; 
      ())
  | "gpx2json" ->
      let dst_dir = (Util.deslash src_dir)^"-json" in
      let input_filenames = Array.to_list (Sys.readdir src_dir) in

      Util.mkdir dst_dir;
      
      let _ = Util.parmap
        (fun input_filename -> 
          let input_path  = src_dir^"/"^input_filename in
          let output_filename = (Filename.chop_extension input_filename)^".json" in
          let output_path = dst_dir^"/"^output_filename in
          (* Printf.printf "From %s to %s\n" input_path output_path; *)
          let track = track_of_gpx (Xml.parse_file input_path) in
          geojson_to_file output_path (geojson_of_simple_track track);
        )
        input_filenames
      in
      ()
  | _ -> failwith "Not a valid command"
