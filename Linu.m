(*

The BSD 2-Clause License:

Copyright (c) 2015, Sampo Smolander
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*)

BeginPackage["Linu`"]

Linear1::usage =
  "Linear1[eq] shows eq with discretized derivative symbols. Use Linear1Raw if
  you want the output without DisplayForm.";
Linear2::usage =
  "Linear2[eq] discretizes eq and arranges the delta-terms (except delta-t) to
  the left side, other to the right side. Use Linear2Raw if you want the output
  without DisplayForm.";
Linear3::usage =
  "Linear3[eq] is one step further from Linear2, also applying Simplify to both
  sides, to collect terms with same factors together, and dividing by
  \[CapitalDelta]t. Use Linear3Raw if you want the output without DisplayForm.";
Linear1Raw::usage = "Linear1 without DisplayStyle.";
Linear2Raw::usage = "Linear2 without DisplayStyle.";
Linear3Raw::usage = "Linear3 without DisplayStyle.";
Linsys1::usage =
  "Linsys1[eqs] Shows a list of equations each discretized with Linear1. Pretty
  output with MatrixForm and DisplayStyle.";
Linsys2::usage =
  "Linsys2[eqs] Shows a list of equations each discretized with Linear2. Pretty
  output with MatrixForm and DisplayStyle.";
Linsys3::usage =
  "Linsys3[eqs] Shows a list of equations each discretized with Linear3. Pretty
  output with MatrixForm and DisplayStyle.";
Linmat1::usage="Linmat[eqs]"
Linmat2::usage="Linmat[eqs]"


Begin["`Private`"]

dt = "\[CapitalDelta]t";

drule[x_] := x -> "\[CapitalDelta]" <> SymbolName[x[[1]]]

frule[x_] := Module[{f, args},
  f = Head[x];
  args = Cases[x, _];
  x -> f + Plus @@ Map[
    FractionBox["\[PartialD]" <> SymbolName[f],
      "\[PartialD]" <> SymbolName[#]]*("\[CapitalDelta]" <> 
      SymbolName[#]) &, args]
]

Linear1Raw[eq_] := Module[{dterms, fterms, eq2},
  dterms = Cases[eq, f_[x__] /; (f === Derivative[1]), Infinity];
  fterms = Cases[eq, 
    f_[x__] /; (f =!= Plus && f =!= Times && f =!= Derivative[1]), Infinity];
  eq2 = eq /. Join[Map[drule, dterms], Map[frule, fterms]];
  eq2[[1]]/dt == eq2[[2]]
]

Linear2Raw[eq_] := Module[{eq2, eq3, eq4, leftdeltas, rightdeltas, left, right},
  eq2 = Linear1Raw[eq];
  eq3 = Expand[dt eq2[[1]]] == Expand[dt eq2[[2]]];
  leftdeltas =
    Cases[eq3[[1]], x_String /; (x != dt && Characters[x][[1]] ==
    "\[CapitalDelta]"), Infinity] // Union;
  rightdeltas =
    Cases[eq3[[2]], x_String /; (x != dt && Characters[x][[1]] ==
    "\[CapitalDelta]"), Infinity] // Union;
  left = Map[Coefficient[eq3[[1]], #]*# &, leftdeltas];
  right = Map[Coefficient[eq3[[2]], #]*# &, rightdeltas];
  eq4 = eq3[[1]] - (Plus @@ left) - (Plus @@ right) == 
    eq3[[2]] - (Plus @@ left) - (Plus @@ right);
  Chop[Expand[eq4]]
]

Linear3Raw[eq_] := Module[{eq2, left, right, div, rule},
  rule = y_ /; y == 1 -> 1;
  div[x_] := (x/dt // Simplify) /. rule;
  eq2 = Linear2Raw[eq];
  left = eq2[[1]];
  right = eq2[[2]];
  div[left] == div[right] /. {x_ /; x == 1 -> 1, x_ /; x == -1 -> -1}
]

linmatx[eqs_, fraw_] := Module[{var, vars, n, x, lineqs, y, m, lhslist},
  var[eq_] := Cases[eq, f_[x__] /; (f === Derivative[1]), Infinity];
  vars = Map[var[#][[1, 1]] &, eqs]; (* Not checking for duplicates *)
  n = Length[vars];
  x = Map["\[CapitalDelta]" <> SymbolName[#] &, vars];
  lineqs = Map[fraw, eqs];
  y = Map[#[[2]] &, lineqs];
  lhslist = Map[#[[1]] &, lineqs];
  m = Table[Coefficient[lhslist[[i]], x[[j]]], {i, n}, {j, n}];
  MatrixForm[m].MatrixForm[x] == MatrixForm[y] // DisplayForm
]

Linear1[eq_] := Linear1Raw[eq] // DisplayForm
Linear2[eq_] := Linear2Raw[eq] // DisplayForm
Linear3[eq_] := Linear3Raw[eq] // DisplayForm
Linsys1[eqs_] := Map[Linear1Raw, eqs] // MatrixForm // DisplayForm
Linsys2[eqs_] := Map[Linear2Raw, eqs] // MatrixForm // DisplayForm
Linsys3[eqs_] := Map[Linear3Raw, eqs] // MatrixForm // DisplayForm

Linmat1[eqs_] := linmatx[eqs, Linear2Raw]
Linmat2[eqs_] := linmatx[eqs, Linear3Raw]

End[]
EndPackage[]
