(* ::Package:: *)

(* Wolfram language version check *)
If[
  $VersionNumber < 10.2,
  Print["NDRoot requires Wolfram Language 10.2 or later."];
  Abort[]
]

(* unprotect package symbols in case NDRoot is double-loaded *)
Unprotect["NDRoot`*", "NDRoot`Developer`*"];

(* load the package *)
Get["NDRoot`NDRoot`"]

(* protect all package symbols *)
SetAttributes[
  Flatten[
    Names /@ {"NDRoot`*", "NDRoot`Developer`*"}
  ] // Evaluate,
  {Protected, ReadProtected}
]