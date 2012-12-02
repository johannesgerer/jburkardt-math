(* ::Package:: *)

BeginPackage["Collatz`"]

Collatz::usage =
  "Collatz[n] gives a list of the iterates in the 3n+1 problem, starting from n.  The conjecture is that this sequence always terminates."

StoppingTime::usage = 
  "StoppingTime[n] finds the total stopping time for the integer n.  This is the length of the Collatz sequence up to the time it reaches the value 1."

FindMaxima::usage =
  "FindMaxima[nmax] reports successive maxima for the total stopping time, starting the search with the integer nmax...and going FOREVER."

Begin["`Private`"]

Collatz[n_Integer] := AppendCollatz[ {}, n ]

AppendCollatz[ sofar_, 1 ] := Flatten[ { sofar, 1 } ]

AppendCollatz[ sofar_, n_Integer ] :=
  AppendCollatz[ {sofar, n}, 3 n + 1 ] /; OddQ[n]

AppendCollatz[ sofar_, n_Integer ] :=
  AppendCollatz[ {sofar, n}, n / 2 ] /; EvenQ[n] && n != 0

StoppingTime[ n_Integer?Positive ] :=
  Module[ { i = 1, m = n }, 
    While [ m != 1,
    m = If [ OddQ[m], 3 m + 1, m / 2 ];
    i++ ];
    i ]

FindMaxima[low_] :=
  Module [ { m = 0, n = 0, i = low, j },
    While [ True,
      j = StoppingTime[i];
      If[ m < j, { m, n } = { j, i };
      Print[ "StoppingTime[", n, "] = ", m ] ];
      i++;
      If[ Mod[i,100] == 0, Print["i = ", i ] ]
    ]
  ]

End[]

EndPackage[]
