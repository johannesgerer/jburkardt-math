(* ::Package:: *)

BeginPackage["SimplestCalendar`"]

MyQuotient::usage =
"MyQuotient[n,list] is a generalized quotient routine."

MyMod::usage =
"MyMod[n,list] is a generalized modulo routine."

MyDigits1::usage =
"MyDigits1[n,list] does I don't remember."

MyDigits::usage =
"MyDigits[n,list] does I don't remember."

MyDigitsLists::usage =
"MyDigitsLists[n,list] does more stuff."

DigitsToNumberLists::usage =
"DigitsToNumberLists[digits, list] does other stuff."

SimplestCalendar = 
{ {365}, {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }};

Begin["`Private`"]

MyQuotient[n_Integer, list_List ] :=
  Quotient[n, First[list]] + 1 /; Length[list] == 1

MyQuotient[n_Integer, list_List] :=
Block[ { s = First[list], q = 1 },
  While[n > s, q++; s += list[[q]]]; q] /; Length[list] > 1

MyMod[n_Integer, list_List ] :=
  Mod[n, First[list]] /; Length[list] == 1

MyMod[n_Integer, list_List ] :=
  n - Fold[Plus, 0, Take[list, MyQuotient[n,list]-1]] /; Length[list] > 1

MyDigits1[n_,list_] := {n} /; Length[list] == 1

MyDigits1[n_,list_] :=
Prepend[MyDigits1[Mod[n,First[list]],Rest[list]],
Quotient[n,First[list]]] /; VectorQ[list]

MyDigits[n_, b_Integer] := MyDigits1[n,b^Reverse[Range[Log[b,n]]]]

MyDigits[n_,b_List] :=
MyDigits1[n,Reverse[Select[Accumulate[Times,Reverse[b]],# < n&]]] /; VectorQ[b]

MyDigitsLists[n_, {}] := {n}

MyDigitsLists[n_,list_List] :=
Prepend [ MyDigitsLists[MyMod[n, First[list]], Rest[list]],
MyQuotient[n,First[list]]]

DigitsToNumberLists[digits_,list_]:=
(digits[[1]]-1) list[[1,1]]+Last[digits]+
(Plus @@
(Fold[Plus, 0, Take[list[[#]],digits[[#]]-1]]& /@
Range[2,Length[list]]))

End[]

EndPackage[]



