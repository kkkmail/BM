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


randomLightInfo[seed_?IntegerQ] :=
    Module[{lmb, ft , bt, el, retVal},
      SeedRandom[seed];
      lmb = RandomInteger[{200, 800}];
      ft = RandomInteger[{0, 85}];
      bt = RandomInteger[{0, 85}];
      el = RandomReal[{-1, 1}];

      retVal = {lmb, ft, bt, el};

      Print["randomLightInfo::retVal", retVal // InputForm];
      Return[retVal];
    ];


randomMedia[seed_?IntegerQ, useIm_?BooleanQ, useMu_?BooleanQ, useRho_?BooleanQ] :=
    Module[{epsReMin, epsReMax, epsImMin, epsImMax, muMin, muMax, rhoMin, rhoMax, rotationEpsRe, rotationEpsIm, rotationMu, rotationRho, eps, mu, rho, retVal},
      SeedRandom[seed];

      epsReMin = 1;
      epsReMax = 5;

      epsImMin = 0;
      epsImMax = 10^-2;

      muMin = 0.9;
      muMax = 1.1;

      rhoMin = -0.1;
      rhoMax = 0.1;

      rotationEpsRe = RotationNew[RandomInteger[{0, 90}] Degree, RandomInteger[{0, 90}] Degree, RandomInteger[{0, 90}] Degree, opts];
      rotationEpsIm = RotationNew[RandomInteger[{0, 90}] Degree, RandomInteger[{0, 90}] Degree, RandomInteger[{0, 90}] Degree, opts];
      rotationMu = RotationNew[RandomInteger[{0, 90}] Degree, RandomInteger[{0, 90}] Degree, RandomInteger[{0, 90}] Degree, opts];
      rotationRho = RotationNew[RandomInteger[{0, 90}] Degree, RandomInteger[{0, 90}] Degree, RandomInteger[{0, 90}] Degree, opts];

      eps = Transform[DiagonalMatrix[{RandomReal[{epsReMin, epsReMax}], RandomReal[{epsReMin, epsReMax}], RandomReal[{epsReMin, epsReMax}]}], rotationEpsRe];
      If[useIm, eps = eps + I * Transform[DiagonalMatrix[{RandomReal[{epsImMin, epsImMax}], RandomReal[{epsImMin, epsImMax}], RandomReal[{epsImMin, epsImMax}]}], rotationEpsIm]];

      mu =
          If[useMu,
            Transform[DiagonalMatrix[{RandomReal[{muMin, muMax}], RandomReal[{muMin, muMax}], RandomReal[{muMin, muMax}]}], rotationMu],
            muMstandard
          ];

      rho =
          If[useRho,
            I * Transform[DiagonalMatrix[{RandomReal[{rhoMin, rhoMax}], RandomReal[{rhoMin, rhoMax}], RandomReal[{rhoMin, rhoMax}]}], rotationRho],
            roMstandard
          ];

      Print["randomMedia::eps = ", eps // toFSharpMatrix];
      Print["randomMedia::mu = ", mu // toFSharpMatrix];
      Print["randomMedia::rho = ", rho // toFSharpMatrix];

      retVal = {eps, mu, rho};
      Print["randomMedia::retVal = ", retVal];

      Return[retVal];
    ];
