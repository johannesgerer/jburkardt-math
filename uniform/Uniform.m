BeginPackage["Uniform`"]

  I4Uniform0i::usage = "I4Uniform0i[seed] returns a pseudorandom I4."
  R4Uniform01::usage = "R4Uniform01[seed] returns a pseudorandom unit R4."
  R8Uniform01::usage = "R8Uniform01[seed] returns a pseudorandom unit R8."

(* This routine implements the recursions    *)
(*                                           *)
(*  I4Uniform0i=16807*seed mod (2**31-1)     *)
(*                                           *)
(*  R4Uniform01= seed / (2**31-1)            *)
(*                                           *)
(*  R8Uniform01= seed / (2**31-1)            *)
(*                                           *)
(*  The integer arithmetic never requires    *)
(*  more than 32 bits, including a sign bit. *)
(*                                           *)
(*  If the initial seed is 12345,            *)
(*  the first three computations are         *)
(*                                           *)
(*        seed   I4Uniform0i                 *)
(*                                           *)
(*       12345   207482415                   *)
(*   207482415  1790989824                   *)
(*  1790989824  2035175616                   *)
(*                                           *)
(*        seed  R4Uniform01                  *)
(*                                           *)
(*   207482415  0.096616                     *)
(*  1790989824  0.833995                     *) 
(*  2035175616  0.947702                     *)
(*                                           *)
(*  Licensing:                               *)
(*                                           *)
(*    This code is distributed under the GNU LGPL license.  *)
(*                                           *)
(*  Modified:                                *)
(*                                           *)
(*    24 September 2006                      *)
(*                                           *)
(*  Author:                                  *)
(*                                           *)
(*    John Burkardt                          *)
(*                                           *)

  Begin["`Private`"]

    I4Uniform0i[seed_Integer]:= (
      k = IntegerPart[ seed / 127773 ];
      seed2=16807*(seed-k*127773)-k*2836;
      seed2 = Mod [ seed2, 2147483647];
      If [ seed2 < 0, seed2 = seed2 + 2147483647];
      seed2 )

    R4Uniform01[seed_Integer] := 
      r4 = N [ seed /(2^31-1), 8 ];

    R8Uniform01[seed_Integer] := 
      r8 = N [ seed /(2^31-1), 16 ];

  End[]

EndPackage[]
