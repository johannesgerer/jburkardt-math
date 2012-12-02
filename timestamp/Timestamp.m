BeginPackage["Timestamp`"]

Timestamp::usage =
  "Timestamp[] prints the YMDHMS date as a timestamp."

Begin["`Private`"]

Timestamp[] := 
(
  data = Date[];
  
  monthnames = {"January","February","March","April",
      "May","June","July","August","September",
      "October","November","December" };
      
  Print [ data[[4]] , ":", data[[5]], " ", data[[3]], " ", 
  monthnames[[data[[2]] ]] , " ", data[[1]] ]
)

End[]

EndPackage[]

