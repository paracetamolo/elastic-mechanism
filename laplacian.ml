(* Copyright (C) 2014, Marco Stronati, All Rights Reserved.
   This file is distributed under the terms of the
   GNU General Public License version 3 or later. *)

(**              
Laplacian random noise, linear and planar.       
 *)

open Geo

(* Inverse of a function using binary search *)
let inverse func (min_x,max_x) y =
  let max_iterations = ref 1000 in
  let precision = 0.05 in                 (* 5% *)
  let beg = ref min_x in
  let ending = ref max_x in
  let mid = ref ((!beg +. !ending) /. 2.) in
  let ans = ref (func !mid) in
  
  while (abs_float (!ans-.y) > (precision *. y))  && (!max_iterations > 0) do
    (* Printf.printf "(%f,%f) lookfor %f  found %f\n" !beg !ending y !ans; *)
    if (!ans < y)                       (* inverse monotonicity *)
    then ending := !mid
    else beg := !mid;
    
    max_iterations := !max_iterations - 1;
    mid := (!beg +. !ending) /. 2.;
    ans := func !mid
  done;
  !mid


(* Laplacian cumulative distribution 
   https://en.wikipedia.org/wiki/Laplacian_distribution
*)
let laplacian_cumulative (mu,b) x =
  if x < mu
  then 0.5 *. exp ((x-.mu) /. b)
  else 1. -. 0.5 *. exp (-. (x-.mu) /. b)


(* Laplacian cumulative distribution - epsilon 0-centered version *)
let laplacian_cumulative_dp epsilon x = laplacian_cumulative (0.,(1. /. epsilon)) x

(* Laplacian cumulative distribution 
   https://en.wikipedia.org/wiki/Laplacian_distribution
*)
(* wikipedia version *)
(* let inverse_laplacian_cumulative (mu,b) p = mu -. (b *. sign(p -. 0.5) *. (ln (1. -. (2. *. (abs_float (p -. 0.5))))) ;; *)
(* let inverse_laplacian_cumulative_dp epsilon p = inverse_laplacian_cumulative (0.,(1. /. epsilon)) p;; *)

(* GSL version *)
let inverse_laplacian_cumulative b y = Gsl_cdf.laplace_Pinv y b 
let inverse_laplacian_cumulative_dp epsilon y = inverse_laplacian_cumulative (1. /. epsilon) y


(* Laplacian cumulative distribution function for polar coordinates *)
let polar_laplacian_cumulative epsilon r = 1. -. (1. +. epsilon *. r) *. exp(-. epsilon *. r)

(* Inverse Laplacian cumulative distribution function for polar coordinates *)
(* let inverse_polar_laplacian_cumulative_2 epsilon z = inverse polar_laplacian_cumulative epsilon z;; *) (* very slow, sometimes incorrect*)
let inverse_polar_laplacian_cumulative epsilon z = -. (1. /. epsilon) *. ((Gsl_sf.lambert_Wm1 ((z -. 1.) /. Gsl_math.e)) +. 1.)


(* PRNG with different seeds, comment if you want a predictable random *)
let _ = Random.self_init ()

(* add laplacian noise in one dimension *)
let noise_linear epsilon = 
  let y = Random.float 1. in
  inverse_laplacian_cumulative_dp epsilon y


(* Add laplacian noise to a point in planar coordinates *)
let noise_polar epsilon p =
  let theta = (Random.float 1.) *. 2. *. Util.pi in
  let r = inverse_polar_laplacian_cumulative epsilon (Random.float 1.) in
  let res = Utm.destination p theta r in
  res


(* this would be the average error *)
(* pdf(x) = \frac{1}{2b} \cdot \exp{- \frac{|x|}{b}} 

 E[error] = \int^{+\infty}_{0} x \cdot pdf(x) = 
\frac{1}{2b} \cdot \int^{+\infty}_{0} x \cdot  \exp{-\frac{x}{b}} = [wolfram]
\left[-b exp{-\frac{x}{b}} (b+x) +c\right]^{+\infty}_{0} =
b^2 =
\frac{1}{\epsilon^2}
*)
let expected_value_linear_positive epsilon = 1. /. (epsilon ** 2.)

(* 
E[error] = \int^{+\infty}_{0} r \cdot pdf(r) = 
\epsilon^2 \int^{+\infty}_{0} r^2 e^{- \epsilon r} = \text{[wolfram alpha] = 
\left [- \epsilon^2 \frac{(e^{- \epsilon x} (\epsilon^2 x^2 + 2 \epsilon x + 2))}{\epsilon^3} +c \right]^{+\infty}_{0} = 
   \frac{2}{\epsilon}
 *)
let expected_value_polar epsilon = 2. /. epsilon

(* this is the inverse of the average error, that is what epsilon produces the average error r *)
let radius_of_expected_value_polar r = 2. /. r

(*
  @param e epsilon
  @return radius maximum radius expected from that epsilon

  | epsilon | radius in meters    |
  |---------+---------------------|
  |     10. | 0.38897201698674294 |
  |      5. |  0.7779440339734858 |
  |      1. |   3.889720169867429 |
  |     0.5 | 7.77944033973485816 |
  |     0.1 | 38.8972016986742943 |
  |    0.01 | 388.972016986742915 |
  |   0.001 | 3889.72016986742892 |
*)
let worst_noise_polar e = 
  inverse_polar_laplacian_cumulative e 0.9

(* let worst_noise_polar e = let c_polar = 3.889720169867429 in c_polar /. e;; *)

let epsilon_of_radius_polar r = -. (1. /. r) *. ((Gsl_sf.lambert_Wm1 ((0.9 -. 1.) /. Gsl_math.e)) +. 1.)
(* let epsilon_of_radius_polar r = let c_polar = 3.889720169867429 in c_polar /. r;; *)



(* |  et  |  radius in meters   | *)
(* |------+---------------------| *)
(* |  1.0 |  1.6094379124341005 | *)
(* |  0.1 | 16.0943791243410033 | *)
(* | 0.01 | 160.943791243410061 | *)
let worst_noise_linear e = 
  inverse_laplacian_cumulative_dp e 0.9  

(* let worst_noise_linear e = let c = 1.609437912434100 in c /. e;; *)

let epsilon_of_radius_linear r = let c = 1.609437912434100 in c /. r


let delta_of_alpha alpha pr (epsilon_t,epsilon_i,l) = 
  
  let delta_of_alpha_t alpha_t epsilon_t =
    let alpha_l = alpha_t -. l in
    1. -. (laplacian_cumulative_dp alpha_l epsilon_t)
  in
  
  let delta_of_alpha_i alpha_i epsilon_i =
    1. -. (polar_laplacian_cumulative epsilon_i alpha_i)
  in
  
  let delta_t = delta_of_alpha_t alpha epsilon_t in
  let delta_i = delta_of_alpha_i alpha epsilon_i in
  let delta = delta_i *. (1. -. pr) +. delta_t *. pr in
  delta


let alpha_of_delta delta pr (epsilon_t,epsilon_i,l) = 
  let func alpha = delta_of_alpha alpha pr (epsilon_t,epsilon_i,l) in
  let ans = inverse func (0.,10000.) delta        (* 0 - 10km *)
  in
  ans

