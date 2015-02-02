(**
Geographical data.

This module handles coordinates in WGS84 nad projects them in UTM zone 31N, computes distances and directions and contains some useful points in Paris.
 *)

(**
   Latitude and Longitude in decimal degrees - WGS84
*)
module rec Wgs : sig
    type t
    val srid : int
    val make : float -> float -> t
    val distance : t -> t -> float
    val destination : t -> float -> float -> t
    val of_xy : Utm.t -> t
    val tuple : t -> float * float
    val string_of : t -> string
end
(**  
  Universal Trasversal Mercartor projection for UTM zone 31N (France)
*)
and Utm : sig
    type t
    val srid : int
    val make : float -> float -> t
    val distance : t -> t -> float
    val destination : t -> float -> float -> t
    val of_latlon : Wgs.t -> t
    val tuple : t -> float * float
    val string_of : t -> string
end

type box = Central of Utm.t * float

val paris1 : Wgs.t
val paris2 : Wgs.t

val paris    : Wgs.t
val paris_nw : Wgs.t
val paris_outer_nw : Wgs.t

val defense  : Wgs.t
val champs   : Wgs.t
val halles   : Wgs.t
val boulogne : Wgs.t

val rio : Wgs.t
val beijing_uni : Wgs.t
