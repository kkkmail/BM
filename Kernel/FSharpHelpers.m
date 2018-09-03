generateMatrix[name_, n_, m_] :=
    Table[ToString[name] <> "[" <> ToString[ii - 1] <> ", " <> ToString[jj - 1] <> "]", {ii, 1, n}, {jj, 1, m}];
generateMatrix[name_, n_] := generateMatrix[name, n, n];
generateMatrix[name_] := generateMatrix[name, 3];

generateVector[name_, n_, m_] :=
    Table[ToString[name] <> ToString[ii - 1] <> ".[" <> ToString[jj - 1] <> "]", {ii, 1, n}, {jj, 1, m}];

complexFuncName = "createComplex";

toFloat[s_?StringQ] := If[StringCount[s, "."] == 0, s <> ".0", s];

toFSharpMatrix[m_?MatrixQ] :=
    Module[{s, nn, mm, ii, jj, f, r, i},
      s = "\n[\n";
      nn = Length[m];
      mm = Length[m[[1]]];

      Do[
        (
          If[mm > 1, s = s <> "    [ "];
          Do[
            (
              r = toFloat[ToString[InputForm[Re[m[[ii, jj]]]]]];
              i = toFloat[ToString[InputForm[Im[m[[ii, jj]]]]]];
              f = complexFuncName <> " " <> r <> " " <> i;
              s = s <> f <> If[jj < mm, "; ", ""];
            ), {jj, 1, mm}
          ];
          If[mm > 1 , s = s <> " ]\n"];
        ), {ii, 1, nn}
      ];
      s = StringReplace[ s <> "]\n", "\"" -> ""];
      Return[s];
    ];