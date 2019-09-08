(* ::Package:: *)

(* ::Title:: *)
(*NDRoot package*)


(* ::Subsubsection:: *)
(*metadata*)


(* : Title : NDRoot *)
(* : Author : Andrew Miller < amiller@physics.umn.edu > *)
(* : Context : NDRoot` *)
(* : Version : 0.1.0 *)
(* : Date : 2019-09-01 *)
(* : Mathematica Version : 10.2 *)
(* : Copyright : (c) 2019 Andrew Miller *)


(* ::Subsubsection::Closed:: *)
(*declare package*)


Package["NDRoot`"]


(* ::Section:: *)
(*package information*)


(* ::Subsubsection::Closed:: *)
(*general*)


NDRoot`Developer`$Version = "0.1.0 (01 September 2019)";


NDRoot`Developer`$VersionNumber = StringReplace[NDRoot`Developer`$Version, "." ~~ Except["."] .. ~~ EndOfString :> ""];


NDRoot`Developer`$ReleaseNumber = StringSplit[NDRoot`Developer`$Version, {" ", "."}][[3]];


NDRoot`Developer`$CreationDate := DateObject @ Last @ StringSplit[NDRoot`Developer`$Version, {"(", ")"}]


NDRoot`Developer`$PackageDirectory = DirectoryName @ $InputFileName;


(* ::Section:: *)
(*exported symbols*)


(* ::Subsubsection::Closed:: *)
(*NDRoot*)


PackageExport["NDRoot"]


NDRoot::usage = "NDRoot[eqn, {x, \!\(\*SubscriptBox[\(x\), \(0\)]\)}, n] uses NDSolve to find the first n roots of eqn starting at x = \!\(\*SubscriptBox[\(x\), \(0\)]\).
NDRoot[eqn, {x, \!\(\*SubscriptBox[\(x\), \(start\)]\), \!\(\*SubscriptBox[\(x\), \(stop\)]\)}, n] finds up to n roots of eqn in the interval \!\(\*SubscriptBox[\(x\), \(start\)]\) to \!\(\*SubscriptBox[\(x\), \(stop\)]\).
NDRoot[eqn, {x, \!\(\*SubscriptBox[\(x\), \(0\)]\), \!\(\*SubscriptBox[\(x\), \(min\)]\), \!\(\*SubscriptBox[\(x\), \(max\)]\)}, n] starts at \!\(\*SubscriptBox[\(x\), \(0\)]\).
NDRoot[eqn, {x, \!\(\*SubscriptBox[\(x\), \(start\)]\), \!\(\*SubscriptBox[\(x\), \(stop\)]\)}(, All)] finds all roots in the interval.
NDRoot[eqn, {\[Ellipsis]}, {n}] picks out the \!\(\*SuperscriptBox[\(n\), \(th\)]\) root, if possible.";


(* ::Section:: *)
(*utilities*)


(* ::Subsection::Closed:: *)
(*notNumericQ*)


notNumericQ = (! NumericQ @ #) &;


(* ::Section:: *)
(*root-finders*)


(* ::Subsection:: *)
(*numerical differential equation solver*)


(* ::Subsubsection:: *)
(*iNDRoot*)


(* ::Subsubsubsection::Closed:: *)
(*info*)


Options @ iNDRoot = Normal[<|Options @ NDSolve, Direction -> Automatic, "Rationalize" -> Automatic|> // KeySort];


(* ::Subsubsubsection::Closed:: *)
(*specified interval*)


iNDRoot[
  eq_,
  {x_? notNumericQ, xstart_? NumericQ, xstop : (_? NumericQ | -\[Infinity] | \[Infinity])}, 
  Optional[n_Integer /; n >= 1, 0],
  opts : OptionsPattern[]
] := With[
  {rationalize = If[OptionValue @ "Rationalize" /. Automatic -> False, y_Real :> Rationalize[y, 0], Nothing]},
  
  Module[
    {f, roots = {}},
    
    With[
      {equation = eq /. {rationalize, e_Equal :> Subtract @@ e}},
      
      NDSolve[
        {
         f'[x] == 0,
         f[xstart] == 1,
         WhenEvent[equation == 0, AppendTo[roots, x], "IntegrateEvent" -> True],
         If[
           n >= 1,
           WhenEvent[Length @ roots == n, "StopIntegration"],
           Nothing
         ]
        },
        {},
        {x, xstart, xstop} /. {rationalize},
        FilterRules[{opts}, Options @ NDSolve]
      ];
      
      roots
      
    ]
  ]
]


iNDRoot[
  eq_,
  {x_? notNumericQ, x0_? NumericQ, xmin : (_? NumericQ | -\[Infinity]), xmax : (_? NumericQ | \[Infinity])}, 
  Optional[n_Integer /; n >= 1, 0],
  opts : OptionsPattern[]
] := With[
  {rationalize = If[OptionValue @ "Rationalize" /. Automatic -> False, y_Real :> Rationalize[y, 0], Nothing]},
  
  Take[
    SortBy[
      {
       iNDRoot[eq, {x, x0, xmax}, n, opts],
       iNDRoot[eq, {x, x0, xmin}, n, opts]
      } // Flatten,
      Abs[# - x0] &
    ],
    UpTo @ n
  ]

]


(* ::Subsubsubsection::Closed:: *)
(*unspecified interval*)


iNDRoot[
  eq_, 
  {x_? notNumericQ, x0_? NumericQ},
  n_Integer /; n >= 1,
  opts : OptionsPattern[]
] := Switch[
  OptionValue @ Direction /. Automatic -> "TwoSided",
  
  1 | "FromBelow",
  iNDRoot[eq, {x, x0, \[Infinity]}, n, opts],
  
  -1 | "FromAbove",
  iNDRoot[eq, {x, x0, -\[Infinity]}, n, opts],
  
  Reals | "TwoSided",
  iNDRoot[eq, {x, x0, -\[Infinity], \[Infinity]}, n, opts]
  
]


(* ::Subsubsection:: *)
(*NDRoot*)


(* ::Subsubsubsection::Closed:: *)
(*info*)


Options @ NDRoot = Options @ iNDRoot;


SyntaxInformation @ NDRoot = {
  "ArgumentsPattern" -> {_, {_, _, _., _.}, _., OptionsPattern[]},
  "LocalVariables" -> {"Table", {2}},
  "OptionNames" -> Keys @ Options @ NDRoot
};


(* ::Subsubsubsection::Closed:: *)
(*general*)


NDRoot[
  eq_, 
  var : {x_? notNumericQ, _? NumericQ, _? NumericQ},
  opts : OptionsPattern[]
] := With[
  {roots = iNDRoot[eq, var, opts]},
  
  Thread[Thread[x @ Range[Length @ roots]] -> roots]
  
]


NDRoot[
  eq_, 
  var : {_? notNumericQ, _? NumericQ, _? NumericQ},
  All,
  opts : OptionsPattern[]
] := NDRoot[eq, var, opts]


NDRoot[
  eq_, 
  var : {x_? notNumericQ, _? NumericQ, Repeated[(_? NumericQ | -\[Infinity] | \[Infinity]), {0, 2}]}, 
  n_Integer /; n >= 1,
  opts : OptionsPattern[]
] := With[
  {roots = iNDRoot[eq, var, n, opts]},
  
  Thread[Thread[x @ Range[Length @ roots]] -> roots]
  
]


NDRoot[
  eq_, 
  var : {x_? notNumericQ, _? NumericQ, Repeated[(_? NumericQ | -\[Infinity] | \[Infinity]), {0, 2}]}, 
  {n_Integer /; n >= 1}, 
  opts : OptionsPattern[]
] := x[n] /. Association[x[n] -> {}, NDRoot[eq, var, n, opts]]
