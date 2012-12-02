BeginPackage["Filum`"]

FileNameDec::usage =
  "FileNameDec[name] decrements the numeric part of a filename."

FileNameInc::usage =
  "FileNameInc[name] increments the numeric part of a filename."

Begin["`Private`"]

FileNameDec[s_String] := (
  lens = StringLength [ s ];
  change = 0;
  s2 = s;
  Do [ 
    {
    c = StringTake[ s2, {i}],
    code = ToCharacterCode [ c ],
    digit = code[[1]] - 48,
    If [ 0 <= digit && digit <= 9, 
      { 
      change = change + 1,
      digit = digit - 1, 
      If [ digit == -1, digit = 9 ],
      c = FromCharacterCode [ digit + 48 ],
      s2 = StringDrop [ s2, {i} ],
      s2 = StringInsert [ s2, c, {i} ],
      If [ digit != 9, Break[] ]
      }]
    },
    { i, lens, 1, -1} 
  ];
  s2 )

FileNameInc[s_String] := (
  lens = StringLength [ s ];
  change = 0;
  s2 = s;
  Do [ 
    {
    c = StringTake[ s2, {i}],
    code = ToCharacterCode [ c ],
    digit = code[[1]] - 48,
    If [ 0 <= digit && digit <= 9, 
      { 
      change = change + 1,
      digit = digit + 1, 
      If [ digit == 10, digit = 0 ],
      c = FromCharacterCode [ digit + 48 ],
      s2 = StringDrop [ s2, {i} ],
      s2 = StringInsert [ s2, c, {i} ],
      If [ digit != 0, Break[] ]
      }]
    },
    { i, lens, 1, -1} 
  ];
  s2 )

End[]

EndPackage[]
