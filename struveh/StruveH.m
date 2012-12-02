BeginPackage["StruveH`"]

StruveH::usage "StruveH[nu,z] evaluates the Struve H function."

Begin["`Private`"]

Attributes[StruveH] = { Listable }

(*  Roman Maeder *)
(*  Programming in Mathematica *)
(* Second Edition, Addison Wesley, 1991, page 258 *)

(* Special values *)

StruveH[r_Rational?Positive, z_] :=
  BesselY[r, z] +
  Sum[ Gamma[ m + 1/2 ] (z/2)^(-2m + r - 1 ) / Gamma[r+1/2 - m], {m, 0, r-1/2}]/Pi /;
    Denominator[r] == 2

StruveH[r_Rational?Negative, z_] :=
  (-1)^(-r-1/2) BesselJ[-r, z ] /; Denominator [r ] == 2

(* Series expansion *)

StruveH/: Series[ StruveH[nu_?NumberQ, z_], { z_, 0, ord_Integer }] :=
  (z/2)^(nu + 1 ) Sum[ (-1)^m (z/2)^(2m) / Gamma[m+3/2] / Gamma[ m + nu + 3/2],
  {m, 0, (ord-nu-1)/2} ] + O[z]^(ord+1)

(* Numerical Evaluation *)

StruveH[_, 0] := 0

StruveH[nu_?NumberQ, z_?NumberQ] :=
  Module[ {s=0, so = -1, prec = Precision[z],
    z2 = -(z/2)^2, k1 = 3/2, k2 = nu+3/2, g1, g2, zf },
    zf = (z/2)^(nu+1); g1 = Gamma[k1]; g2 = Gamma[k2];
  While [ so != s,
    so = s; s+= N[zf/g1/g2, prec ];
    g1 *= k1; g2 *= k2; zf *= z2;
    k1++; k2++
  ];
  s
] /; Precision[z] < Infinity

(* Derivatives *)

Derivative[0, n_Integer?Positive][StruveH][nu_, z_] :=
  D[ (StruveH[nu-1,z] - StruveH[nu+1,z] + (z/2)^nu/Sqrt[Pi]/Gamma[nu+3/2])/2,
  {z, n-1} ]

End[]

Protect[StruveH]
EndPackage[]
