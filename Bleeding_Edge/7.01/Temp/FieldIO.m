(* ============================================== *)
(* :Summary: This module defines some commonly used Field IO operations. *)
(* ============================================== *)
(* :Author: Konstantin K. Konstantinov *)
(* :Email: konstantin.k.konstantinov@gmail.com *)
(* :License type: GPL v3 or any later version, see http://www.gnu.org/licenses/ *)
(* :Copyright: K^3, 2001 - 2018 *)
(* :Version: Revision: 6.04.001, Date: 2018/07/02 *)
(* :Mathematica Version: 11.2 *)
(* ============================================== *)
(* This program is free software: you can redistribute it and/or modify it under the terms *)
(* of the GNU General Public License as published by the Free Software Foundation, *)
(* either version 3 of the License, or any later version. This program is distributed in the hope that  *)
(* it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. *)
(* You should have received a copy of the GNU General Public License along with this program. *)
(* If not, see <http://www.gnu.org/licenses/>. *)
(* ============================================== *)
Options[FieldIO] = {FieldIOVersion -> 6.04, FieldIOFormatVersion -> 6.04};
(* ============================================== *)
BMVersionMain = "6.04.001";
BMReleaseDateMain = "2018/07/02";
BMCopyrightStr = "Copyright: K^3, 2001 - 2018.";
BMEmailStr = "konstantin.k.konstantinov@gmail.com";
BMLicenseStr = "License type: GPL v3 or any later version, see http://www.gnu.org/licenses/";
BMDisclaimerStr = "This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program. If not, see http://www.gnu.org/licenses/ .";
BMProgramDescription := "Crystal Plate Reflection and Transmission. ";
(* ============================================== *)
strVersion := "Version " <> ToString[FieldIOVersion /. Options[FieldIO]];
strFormatVersion := "Version " <> ToString[FieldIOFormatVersion /. Options[FieldIO]];
(* ============================================== *)
OutputCopyright[] :=
    Module[{},
      Print[strSeparator];
      Print["BM version: ", BMVersionMain];
      Print["Release date: ", BMReleaseDateMain];
      Print[BMProgramDescription <> "."];
      Print[GetFullVersionInfo[]];
      Print[BMCopyrightStr];
      Print["Email: ", BMEmailStr];
      Print[BMLicenseStr];
      Print[strSeparator];
      Print[BMDisclaimerStr];
      Print[strSeparator];
    ];
(* ============================================== *)
OutputFunc[Calc_, fName_ : "", OverWrite_ : True] :=
    Module[{funcnames, varnames, inpt, outpt, len, width, idxarr, blankarr, OutList, OutListOnly, OutHead, OutFullList, OutFooter, funcnamesOrig, varnamesOrig},
      inpt = Chop[CalcGetInput[Calc]];
      outpt = Chop[CalcGetOutput[Calc]];
      funcnamesOrig = Flatten[{CalcGetFuncNames[Calc]}];
      varnamesOrig = CalcGetVarNames[Calc];

      If[fName != "",
        (
          funcnames = Table[ToEnglish[funcnamesOrig[[iii]]], {iii, 1, Length[funcnamesOrig]}];
          varnames = Table[ToEnglish[varnamesOrig[[iii]]], {iii, 1, Length[varnamesOrig]}];
        ),
        (
          funcnames = funcnamesOrig;
          varnames = varnamesOrig;
        ),
        (
          funcnames = funcnamesOrig;
          varnames = varnamesOrig;
        )
      ];

      (*
      Print["OutputFunc:: funcnamesOrig = ", funcnamesOrig, ", varnamesOrig = ", varnamesOrig];
      Print["OutputFunc:: funcnames = ", funcnames, ", varnames = ", varnames];
      *)

      len = Length[outpt];idxarr = Table[{"", i}, {i, len}];
      blankarr = Table[{""}, {i, len}];
      width = Length[varnames];
      OutList = Join[{Join[{"Input", "No"}, Join[varnames, {"Output"}, funcnames]]}, JoinRight[idxarr, inpt, blankarr, outpt]];
      OutListOnly = If[width > 100, Join[{Join[{"Input", "No"}, Join[{"Output"}, funcnames]]}, JoinRight[idxarr, blankarr, outpt]], {{""}}, {{""}}];
      OutHead = PrepareHeader[Calc];
      OutFooter = PrepareFooter[Calc];

      If[fName === "",
        (
          Print[strSeparator];
          Print[TableForm[OutHead]];
          Print[MatrixForm[N[OutList]]];
          Print[TableForm[OutFooter]]
        ),
        (
          OutFullList = Join[OutHead, N[OutList], OutFooter, N[OutListOnly]];
          Export[fName, OutFullList, "CSV"];
        )
      ];
    ];
(* ============================================== *)
PrepareHeader[Calc_] :=
    Module[{OutFullList, Media, n1, n2, Film, mDescr, flmLen, optBlk, nOut, eps, mu, ro, mHlp1, mHlp2, mHlp3, epsHlp, muHlp, roHlp, roTHlp, roT},
      strFormatHeader = "*** Multilayered Thin Film Output File Format " <> strFormatVersion <> " ***";
      strModellingEngineHeader = "*** Modelling Engine " <> strVersion <> " ***";
      strDescriptionHeader = "* Description *";
      strDescription = CalcDescription[Calc];
      strOpticalPropertiesHeader = "*** Begin Optical Properties Block ***";
      strMediaHeader = "** Media **";strRefractionIndex = "Refraction_Index";
      strThickness = "Thickness";strMediaFooter = "** Media End **";
      strFilmHeader = "** Film **";strFilmThickness = "Film Thickness";strN = "n";
      strK = "k";strEpsilonRE = "Epsilon_RE";strEpsilonIM = "Epsilon_IM";
      strFilmFooter = "** Film End **";
      strOpticalPropertiesFooter = "*** End Optical Properties Block ***";
      strOutputBlockHeader = "*** Begin Output Block ***";
      strOptionsHeader = "*** Begin Options Block ***";
      strOptionsFooter = "*** End Options Block ***";
      strSubstrateHeader = "** Begin Detailed Substrate Block **";
      strSubstrateFooter = "** End Detailed Substrate Block **";
      strSubstrateEpsilon = "Epsilon";
      strSubstrateMu = "Mu";
      strSubstrateRo = "Ro";
      strSubstrateRoT = "RoT";
      strRE = "RE";
      strIM = "IM";
      Media = CalcMedia[Calc];
      n1 = MediaUpperRefractionIndex[Media];
      n2 = MediaLowerRefractionIndex[Media];
      Film = MediaFilm[Media];
      mDescr = MediaDescription[Media];
      flmLen = FilmLength[Film];
      nOut = MediaOutRefractionIndex[Media];
      optBlk = PrepareOptionsBlock[Calc];
      OutFullList = {{strFormatHeader}};
      OutFullList = Join[OutFullList, {{strModellingEngineHeader}}];
      OutFullList = Join[OutFullList, {{strDescriptionHeader}}];
      OutFullList = Join[OutFullList, {{strDescription}}];
      OutFullList = Join[OutFullList, {{""}}];
      OutFullList = Join[OutFullList, {{strOptionsHeader}}];
      OutFullList = Join[OutFullList, optBlk];
      OutFullList = Join[OutFullList, {{strOptionsFooter}}];
      OutFullList = Join[OutFullList, {{""}}];
      OutFullList = Join[OutFullList, {{strOpticalPropertiesHeader}}];
      OutFullList = Join[OutFullList, {{strMediaHeader}}];
      OutFullList = Join[OutFullList, {{strRefractionIndex}}];
      OutFullList = Join[OutFullList, {{"", n1, "", n2, "", nOut}}];
      OutFullList = Join[OutFullList, {{strSubstrateHeader}}];
      OutFullList = Join[OutFullList, {{"", strSubstrateEpsilon <> " " <> strRE, "", "", "", strSubstrateEpsilon <> " " <> strIM, "", "", "", strSubstrateMu <> " " <> strRE, "", "", "", strSubstrateMu <> " " <> strIM, "", "", "", strSubstrateRo <> " " <> strRE, "", "", "", strSubstrateRo <> " " <> strIM, "", "", "", strSubstrateRoT <> " " <> strRE, "", "", "", strSubstrateRoT <> " " <> strIM, "", "", ""}}];
      Clear[mHlp1, mHlp2, mHlp3];
      mHlp1 = {""};
      mHlp2 = mHlp1;
      mHlp3 = mHlp1;

      eps = MediaLowerEpsilon[Media];
      mu = MediaLowerMu[Media];
      ro = MediaLowerRo[Media];
      roT = MediaLowerRo[Media];

      epsHlp = If[Head[eps] === Head[{}], {Re[eps], Im[eps]}, {DiagonalMatrix[{"""" <> ToString[FullDefinition[eps]] <> """", "", ""}], DiagonalMatrix[{"", "", ""}]}];
      muHlp = If[Head[mu] === Head[{}], {Re[mu], Im[mu]}, {DiagonalMatrix[{"""" <> ToString[FullDefinition[mu]] <> """", "", ""}], DiagonalMatrix[{"", "", ""}]}];
      roHlp = If[Head[ro] === Head[{}], {Re[ro], Im[ro]}, {DiagonalMatrix[{"""" <> ToString[FullDefinition[ro]] <> """", "", ""}], DiagonalMatrix[{"", "", ""}]}];
      roTHlp = If[Head[roT] === Head[{}], {Re[roT], Im[roT]}, {DiagonalMatrix[{"""" <> ToString[FullDefinition[roT]] <> """", "", ""}], DiagonalMatrix[{"", "", ""}]}];

      mHlp1 = Join[mHlp1, epsHlp[[1]][[1]], {""}, epsHlp[[2]][[1]], {""}, muHlp[[1]][[1]], {""}, muHlp[[2]][[1]], {""}, roHlp[[1]][[1]], {""}, roHlp[[2]][[1]], {""}, roTHlp[[1]][[1]], {""}, roTHlp[[2]][[1]], {""}];
      mHlp2 = Join[mHlp2, epsHlp[[1]][[2]], {""}, epsHlp[[2]][[2]], {""}, muHlp[[1]][[2]], {""}, muHlp[[2]][[2]], {""}, roHlp[[1]][[2]], {""}, roHlp[[2]][[2]], {""}, roTHlp[[1]][[2]], {""}, roTHlp[[2]][[2]], {""}];
      mHlp3 = Join[mHlp3, epsHlp[[1]][[3]], {""}, epsHlp[[2]][[3]], {""}, muHlp[[1]][[3]], {""}, muHlp[[2]][[3]], {""}, roHlp[[1]][[3]], {""}, roHlp[[2]][[3]], {""}, roTHlp[[1]][[3]], {""}, roTHlp[[2]][[3]], {""}];

      (*
      mHlp1=Join[mHlp1,Re[MediaLowerEpsilon[Media]][[1]],{""},Im[MediaLowerEpsilon[Media]][[1]],{""},Re[MediaLowerMu[Media]][[1]],{""},Im[MediaLowerMu[Media]][[1]],{""},Re[MediaLowerRo[Media]][[1]],{""},Im[MediaLowerRo[Media]][[1]],{""},Re[MediaLowerRoT[Media]][[1]],{""},Im[MediaLowerRoT[Media]][[1]],{""}];
      mHlp2=Join[mHlp2,Re[MediaLowerEpsilon[Media]][[2]],{""},Im[MediaLowerEpsilon[Media]][[2]],{""},Re[MediaLowerMu[Media]][[2]],{""},Im[MediaLowerMu[Media]][[2]],{""},Re[MediaLowerRo[Media]][[2]],{""},Im[MediaLowerRo[Media]][[2]],{""},Re[MediaLowerRoT[Media]][[2]],{""},Im[MediaLowerRoT[Media]][[2]],{""}];
      mHlp3=Join[mHlp3,Re[MediaLowerEpsilon[Media]][[3]],{""},Im[MediaLowerEpsilon[Media]][[3]],{""},Re[MediaLowerMu[Media]][[3]],{""},Im[MediaLowerMu[Media]][[3]],{""},Re[MediaLowerRo[Media]][[3]],{""},Im[MediaLowerRo[Media]][[3]],{""},Re[MediaLowerRoT[Media]][[3]],{""},Im[MediaLowerRoT[Media]][[3]],{""}];
      *)

      OutFullList = Join[OutFullList, {mHlp1, mHlp2, mHlp3}];
      OutFullList = Join[OutFullList, {{strSubstrateFooter}}];
      OutFullList = Join[OutFullList, {{strThickness}}];
      OutFullList = Join[OutFullList, {{"Nothing to ouput so far..."}}];
      OutFullList = Join[OutFullList, {{strMediaFooter}}];
      OutFullList = Join[OutFullList, {{""}}];
      OutFullList = Join[OutFullList, {{strFilmHeader}}];

      (*OutFullList=Join[OutFullList,{{strFilmThickness}}];Clear[mHlp1];
      mHlp1={"",""};*)(*Do[(mHlp1=Join[mHlp1,{FilmLayerThickness[Film[[idx]]]},{"","",""}]),{idx,flmLen}];
OutFullList=Join[OutFullList,{mHlp1,{""}}];*)

      OutFullList = Join[OutFullList, {{strN}}];
      Clear[mHlp1, mHlp2, mHlp3];mHlp1 = {""};mHlp2 = mHlp1;mHlp3 = mHlp1;

      Do[
        (
          eps = FilmLayerEpsilon[Film[[idx]]];
          epsHlp = If[Head[eps] === Head[{}], Re[MatrixPower[eps, 0.5]], DiagonalMatrix[{"N/A", "N/A", "N/A"}]];
          (mHlp1 = Join[mHlp1, epsHlp[[1]], {""}]);
          (mHlp2 = Join[mHlp2, epsHlp[[2]], {""}]);
          (mHlp3 = Join[mHlp3, epsHlp[[3]], {""}])

        (*
        (mHlp1=Join[mHlp1,Re[MatrixPower[FilmLayerEpsilon[Film[[idx]]],0.5][[1]]],{""}]);
        (mHlp2=Join[mHlp2,Re[MatrixPower[FilmLayerEpsilon[Film[[idx]]],0.5][[2]]],{""}]);
        (mHlp3=Join[mHlp3,Re[MatrixPower[FilmLayerEpsilon[Film[[idx]]],0.5][[3]]],{""}])
        *)
        ),
        {idx, flmLen}
      ];

      OutFullList = Join[OutFullList, {mHlp1, mHlp2, mHlp3, {""}}];
      OutFullList = Join[OutFullList, {{strK}}];
      Clear[mHlp1, mHlp2, mHlp3];mHlp1 = {""};mHlp2 = mHlp1;mHlp3 = mHlp1;

      (* TODO - Add film layer mu and ro *)
      Do[
        (
          eps = FilmLayerEpsilon[Film[[idx]]];
          epsHlp = If[Head[eps] === Head[{}], Im[MatrixPower[eps, 0.5]], DiagonalMatrix[{"N/A", "N/A", "N/A"}]];
          (mHlp1 = Join[mHlp1, epsHlp[[1]], {""}]);
          (mHlp2 = Join[mHlp2, epsHlp[[2]], {""}]);
          (mHlp3 = Join[mHlp3, epsHlp[[3]], {""}])
        (*
        (mHlp1=Join[mHlp1,Im[MatrixPower[FilmLayerEpsilon[Film[[idx]]],0.5][[1]]],{""}]);
        (mHlp2=Join[mHlp2,Im[MatrixPower[FilmLayerEpsilon[Film[[idx]]],0.5][[2]]],{""}]);
        (mHlp3=Join[mHlp3,Im[MatrixPower[FilmLayerEpsilon[Film[[idx]]],0.5][[3]]],{""}])
        *)
        ),
        {idx, flmLen}
      ];

      OutFullList = Join[OutFullList, {mHlp1, mHlp2, mHlp3, {""}}];
      OutFullList = Join[OutFullList, {{strEpsilonRE}}];
      Clear[mHlp1, mHlp2, mHlp3];mHlp1 = {""};mHlp2 = mHlp1;mHlp3 = mHlp1;

      Do[
        (
          eps = FilmLayerEpsilon[Film[[idx]]];
          epsHlp = If[Head[eps] === Head[{}], Re[eps], DiagonalMatrix[{"""" <> ToString[FullDefinition[eps]] <> """", "N/A", "N/A"}]];
          (mHlp1 = Join[mHlp1, epsHlp[[1]], {""}]);
          (mHlp2 = Join[mHlp2, epsHlp[[2]], {""}]);
          (mHlp3 = Join[mHlp3, epsHlp[[3]], {""}])

        (*
        (mHlp1=Join[mHlp1,Re[FilmLayerEpsilon[Film[[idx]]][[1]]],{""}]);
        (mHlp2=Join[mHlp2,Re[FilmLayerEpsilon[Film[[idx]]][[2]]],{""}]);
        (mHlp3=Join[mHlp3,Re[FilmLayerEpsilon[Film[[idx]]][[3]]],{""}])
        *)
        ),
        {idx, flmLen}
      ];

      OutFullList = Join[OutFullList, {mHlp1, mHlp2, mHlp3, {""}}];
      OutFullList = Join[OutFullList, {{strEpsilonIM}}];
      Clear[mHlp1, mHlp2, mHlp3];mHlp1 = {""};mHlp2 = mHlp1;
      mHlp3 = mHlp1;

      Do[
        (
          eps = FilmLayerEpsilon[Film[[idx]]];
          epsHlp = If[Head[eps] === Head[{}], Im[eps], DiagonalMatrix[{"N/A", "N/A", "N/A"}]];
          (mHlp1 = Join[mHlp1, epsHlp[[1]], {""}]);
          (mHlp2 = Join[mHlp2, epsHlp[[2]], {""}]);
          (mHlp3 = Join[mHlp3, epsHlp[[3]], {""}])

        (*
        (mHlp1=Join[mHlp1,Im[FilmLayerEpsilon[Film[[idx]]][[1]]],{""}]);
        (mHlp2=Join[mHlp2,Im[FilmLayerEpsilon[Film[[idx]]][[2]]],{""}]);
        (mHlp3=Join[mHlp3,Im[FilmLayerEpsilon[Film[[idx]]][[3]]],{""}])
        *)
        ),
        {idx, flmLen}
      ];

      OutFullList = Join[OutFullList, {mHlp1, mHlp2, mHlp3}];
      OutFullList = Join[OutFullList, {{strFilmFooter}}];
      OutFullList = Join[OutFullList, {{strOpticalPropertiesFooter}}];
      OutFullList = Join[OutFullList, {{""}}];
      OutFullList = Join[OutFullList, {{strOutputBlockHeader}}];
      Return[OutFullList];
    ];
(* ============================================== *)
PrepareFooter[Calc_] :=
    Module[{}, strOutputBlockFooter = "*** End Output Block ***";
    Return[{{strOutputBlockFooter}, {""}}];
    ];
(* ============================================== *)
(*TransmittedAnalyzerAngle\[Rule]45 Degree,TransmittedAnalyzerParallelAmplitude\[Rule]1,TransmittedAnalyzerCrossedAmplitude\[Rule]0,ReflectedAnalyzerAngle\[Rule]45 Degree,ReflectedAnalyzerParallelAmplitude\[Rule]1,ReflectedAnalyzerCrossedAmplitude\[Rule]0,AnalyzerAngleAbsoluteValue\[Rule]True*)
(* ============================================== *)
PrepareOptionsBlock[Calc_] :=
    Module[{retval, OA, opts, optval}, OA = Flatten[{Options[BerremanCommon], Options[BerremanDirect]}];
    opts = CalcOptions[Calc];
    strOptionsShift = "";
    strUseEulerAngles = strOptionsShift <> "Using Euler Angles.";
    strUseMediaAngles = strOptionsShift <> "Using Media Orientation Angles.";
    strCalculateBoundarySolution = strOptionsShift <> "Calculating Boundary Solution.";
    strCalculateDelta = strOptionsShift <> "Calculating Delta.";
    strRotateAll = strOptionsShift <> "Rotating all layers simultaneously.";
    strRotateIndividually = strOptionsShift <> "Rotating all layers individually.";
    strConsecutiveRotation = strOptionsShift <> "Using consecutive rotations.";
    strSumOfRotations = strOptionsShift <> "Using sum of rotations.";
    strSIM = strOptionsShift <> "Solving for layered film between TWO SEMI-INFINITE isotropic media; nOut is IGNORED !!!";
    strUTLL = strOptionsShift <> "Solving for layered film on a THICK SUBSTRATE plate.";
    strAvgType = strOptionsShift <> "    Averaging Type: ";
    strNoOfAveragingPoints = strOptionsShift <> "    No Of Averaging Points: ";
    strAveragingPeriods = strOptionsShift <> "    Averaging Periods: ";
    strTransmittedAnalyzerAngle = "Transmitted Analyzer Angle (Degree)";
    strTransmittedAnalyzerParallelAmplitude = "Transmitted Analyzer Parallel Amplitude";
    strTransmittedAnalyzerCrossedAmplitude = "Transmitted Analyzer Crossed Amplitude";
    strReflectedAnalyzerAngle = "Reflected Analyzer Angle (Degree)";
    strReflectedAnalyzerParallelAmplitude = "Reflected Analyzer Parallel Amplitude";
    strReflectedAnalyzerCrossedAmplitude = "Reflected Analyzer Crossed Amplitude";
    strAnalyzerAngleAbsoluteValue = "Using Analyzer Angle Absolute Value";
    strAbsoluteAzimuth = "Using Absolute Azimuth";
    optval = UseEulerAngles /. opts /. OA;
    retval = {{"", "", If[optval === True, strUseEulerAngles, strUseMediaAngles]}};
    optval = UseThickLastLayer /. opts /. OA;
    If[optval === True, retval = Join[retval, {{"", "", strUTLL}}];
    optval = AveragingType /. opts /. OA;
    retval = Join[retval, {{"", ToString[optval], strAvgType}}];
    optval = NoOfAveragingPoints /. opts /. OA;
    retval = Join[retval, {{"", ToString[optval], strNoOfAveragingPoints}}];
    optval = AveragingPeriods /. opts /. OA;
    retval = Join[retval, {{"", ToString[optval], strAveragingPeriods}}], retval = Join[retval, {{"", "", strSIM}}]];
    optval = CalculateBoundarySolution /. opts /. OA;
    If[optval === True, retval = Join[retval, {{"", "", strCalculateBoundarySolution}}]];
    optval = CalculateDelta /. opts /. OA;
    If[optval === True, retval = Join[retval, {{"", "", strCalculateDelta}}]];
    optval = RotateAll /. opts /. OA;
    If[optval === True, retval = Join[retval, {{"", "", strRotateAll}}], retval = Join[retval, {{"", "", strRotateIndividually}}]];
    optval = ConsecutiveRotation /. opts /. OA;
    If[optval === True, retval = Join[retval, {{"", "", strConsecutiveRotation}}], retval = Join[retval, {{"", "", strSumOfRotations}}]];
    optval = AbsoluteAzimuth /. opts /. OA;
    retval = Join[retval, {{"", ToString[optval], strAbsoluteAzimuth}}];
    optval = UseAnalyzer /. opts /. OA;
    If[optval === True, optval = AnalyzerAngleAbsoluteValue /. opts /. OA;
    retval = Join[retval, {{"", ToString[optval], strAnalyzerAngleAbsoluteValue}}];
    optval = (TransmittedAnalyzerAngle /. opts /. OA) / Degree;
    retval = Join[retval, {{"", ToString[optval], strTransmittedAnalyzerAngle}}];
    optval = TransmittedAnalyzerParallelAmplitude /. opts /. OA;
    retval = Join[retval, {{"", ToString[optval], strTransmittedAnalyzerParallelAmplitude}}];
    optval = TransmittedAnalyzerCrossedAmplitude /. opts /. OA;
    retval = Join[retval, {{"", ToString[optval], strTransmittedAnalyzerCrossedAmplitude}}];
    optval = (ReflectedAnalyzerAngle /. opts /. OA) / Degree;
    retval = Join[retval, {{"", ToString[optval], strReflectedAnalyzerAngle}}];
    optval = ReflectedAnalyzerParallelAmplitude /. opts /. OA;
    retval = Join[retval, {{"", ToString[optval], strReflectedAnalyzerParallelAmplitude}}];
    optval = ReflectedAnalyzerCrossedAmplitude /. opts /. OA;
    retval = Join[retval, {{"", ToString[optval], strReflectedAnalyzerCrossedAmplitude}}];];
    Return[retval];
    ];
(* ============================================== *)
ToEnglish[var_] :=
    Module[{retval, len, ltArr, i, lt, str, prevCRLF},
      str = ToString[var];
      ltArr = Characters[str];
      len = Length[ltArr];
      retval = "";
      prevCRLF = False;

      For[i = 1, i <= len, i++,
        lt = ltArr[[i]];
        If[ ToCharacterCode[lt][[1]] == 10,
          ( lt = "_";prevCRLF = True),
          (If[prevCRLF == True && ToCharacterCode[lt][[1]] == 32, lt = ""]; prevCRLF = False),
          prevCRLF = False
        ];

        (*
        Print["prevCRLF = ", prevCRLF];
        Print["Code for """, lt, """ is ", ToCharacterCode[lt]];
        *)
        retval = retval <> GreekLetterMap[lt];
      ];

      Return[retval];
    ];
(* ============================================== *)
GreekLetterMap[ltVar_] :=
    Module[{retval, lt},
      lt = ToString[ltVar];
      retval = Switch[ToLowerCase[lt],
        "\[Alpha]", "alpha",
        "\[Beta]", "beta",
        "\[Gamma]", "gamma",
        "\[Delta]", "delta",
        "\[Epsilon]", "eps",
        "\[Zeta]", "zeta",
        "\[Eta]", "eta",
        "\[Theta]", "theta",
        "\[Iota]", "iota",
        "\[Kappa]", "kappa",
        "\[Lambda]", "lambda",
        "\[Mu]", "mu",
        "\[Nu]", "nu",
        "\[Xi]", "xi",
        "\[Omicron]" , "omicron",
        "\[Pi]", "pi",
        "\[Rho]", "rho",
        "\[Sigma]", "sigma",
        "\[Tau]", "tau",
        "\[Upsilon]", "upsilon",
        "\[Phi]", "phita",
        "\[CurlyPhi]", "phi",
        "\[Chi]", "chi",
        "\[Psi]", "psi",
        "\[Omega]", "omega",
        _, lt
      ];
      retval = If[ToUpperCase[lt] == lt, ToUpperCase[retval], retval, retval];
      (* retval=If[retval\[Equal] lt,retval,retval <> "_",retval]; *)
      Return[retval];
    ];
(* ============================================== *)
GetRotationInfo[Calc_] :=
    Module[{retVal, opts, useEulerAng},
      opts = CalcOptions[Calc];
      useEulerAng = UseEulerAngles /. opts /. Options[BerremanCommon];
      retVal = If[useEulerAng == True, "Using Euler angles for rotations.", "Using Optiva angles for rotation: " <> FromCharacterCode[10] <> "Rotation 1: Fi (angle between crystal axis and deposition direction) - rotation around z; " <> FromCharacterCode[10] <> "Rotation 2: Psi (angle between crystal axis and substrate plane.) - rotation around y (in the opposite direction!!!); " <> FromCharacterCode[10] <> "Rotation 3: Alpha (rotation of crystal around its axis.) - rotation around x.", ""];
      Return[retVal];
    ];
(* ============================================== *)
GetFullVersionInfo[] :=
    Module[{retVal},
      retVal = "BerremanInitVersion = " <> ToString[(BerremanInitVersion /. Options[BerremanInit])];
      retVal = retVal <> FromCharacterCode[10] <> "BerremanCommonVersion = " <> ToString[(BerremanCommonVersion /. Options[BerremanCommon])];
      retVal = retVal <> FromCharacterCode[10] <> "BerremanDirectVersion = " <> ToString[(BerremanDirectVersion /. Options[BerremanDirect])];
      retVal = retVal <> FromCharacterCode[10] <> "BerremanInverseVersion = " <> ToString[(BerremanInverseVersion /. Options[BerremanInverse])];
      retVal = retVal <> FromCharacterCode[10] <> "FieldAlgebraVersion = " <> ToString[(FieldAlgebraVersion /. Options[FieldAlgebra])];
      retVal = retVal <> FromCharacterCode[10] <> "FieldIOVersion = " <> ToString[(FieldIOVersion /. Options[FieldIO])];
      retVal = retVal <> FromCharacterCode[10] <> "FieldIOFormatVersion = " <> ToString[(FieldIOFormatVersion /. Options[FieldIO])];

      Return[retVal];
    ];
(* ============================================== *)
OutputThickLastLayerInfo[] :=
    Module[{},
      Print[strSeparator];
      Print["\:0414\:043b\:044f \:0440\:0430\:0441\:0447\:0435\:0442\:043e\:0432 c \:0443\:0447\:0435\:0442\:043e\:043c \:043c\:043d\:043e\:0433\:043e\:043a\:0440\:0430\:0442\:043d\:044b\:0445 \:043e\:0442\:0440\:0430\:0436\:0435\:043d\:0438\:0439 \:043d\:0443\:0436\:043d\:043e \:043f\:043e\:0441\:0442\:0430\:0432\:0438\:0442\:044c \:0437\:043d\:0430\:0447\:0435\:043d\:0438\:0435 \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:0430 UseThickLastLayer -> False."];
      Print[strSeparator];
      Print["\:0414\:043b\:044f \:0440\:0430\:0441\:0447\:0435\:0442\:043e\:0432 \:0441 \:0442\:043e\:043b\:0441\:0442\:043e\:0439 \:043f\:043b\:0430\:0441\:0442\:0438\:043d\:043a\:043e\:0439 (\:0442.\:0435. \:0431\:0435\:0437 \:0443\:0447\:0435\:0442\:0430 \:043c\:043d\:043e\:0433\:043e\:043a\:0440\:0430\:0442\:043d\:044b\:0445 \:043e\:0442\:0440\:0430\:0436\:0435\:043d\:0438\:0439) \:043d\:0443\:0436\:043d\:043e \:043f\:043e\:0441\:0442\:0430\:0432\:0438\:0442\:044c \:0437\:043d\:0430\:0447\:0435\:043d\:0438\:0435 \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:0430 UseThickLastLayer -> True."];
      Print["\:041f\:0440\:0438 \:044d\:0442\:043e\:043c \:0437\:043d\:0430\:0447\:0435\:043d\:0438\:0435 \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:0430 NoOfAveragingPoints \:0431\:0443\:0434\:0435\:0442 \:043e\:043f\:0440\:0435\:0434\:0435\:043b\:044f\:0442\:044c \:0441\:043a\:043e\:043b\:044c\:043a\:043e \:043d\:0435\:043a\:043e\:0433\:0435\:0440\:0435\:043d\:0442\:043d\:044b\:0445 \:043e\:0442\:0440\:0430\:0436\:0435\:043d\:0438\:0439 \:0443\:0447\:0438\:0442\:044b\:0432\:0430\:0442\:044c. \:041d\:0435\:043a\:043e\:0433\:0435\:0440\:0435\:043d\:0442\:043d\:044b\:0435 \:043e\:0442\:0440\:0430\:0436\:0435\:043d\:0438\:044f \:0431\:0443\:0434\:0443\:0442 \:0441\:043a\:043b\:0430\:0434\:044b\:0432\:0430\:0442\:044c\:0441\:044f \:043f\:043e \:0438\:043d\:0442\:0435\:043d\:0441\:0438\:0432\:043d\:043e\:0441\:0442\:0438."];
      Print[strSeparator];
    ];
(* ============================================== *)
OutputThickLastLayerInfo[Calc_] :=
    Module[{opts, uTLL},
      opts = CalcOptions[Calc];
      uTLL = UseThickLastLayer /. opts /. Options[BerremanDirect];
    ];
(* ============================================== *)
OutputIncidentRayInfo[ray_?IncidentRayQ] :=
    Module[{light, output, names},
      light = IncidentRayGetLight[ray];
      output = {IncidentLightLambda[light], IncidentLightFita[light], IncidentLightBeta[light], Join[IncidentLightEllipticity[light], {""}]};
      Print["Parameters: ", output // MatrixForm];
      Print[strSeparatorSmall];

      (*
      lambda={600,600,1,"\[Lambda]",nm};
      Print["\:0414\:043b\:0438\:043d\:0430 \:0432\:043e\:043b\:043d\:044b \:043f\:0430\:0434\:0430\:044e\:0449\:0435\:0433\:043e \:0441\:0432\:0435\:0442\:0430: ",lambda[[4]]," = ",N[lambda]];

      fita={0,85,5,"\[Phi]",Degree};
      Print["\:0423\:0433\:043e\:043b \:043f\:0430\:0434\:0435\:043d\:0438\:044f: ",fita[[4]]," = ",fita];

      beta={0,0,45,"\[Beta]",Degree};
      Print["\:0423\:0433\:043e\:043b \:0432\:0440\:0430\:0449\:0435\:043d\:0438\:044f \:043f\:043b\:043e\:0441\:043a\:043e\:0441\:0442\:0438 \:043f\:043e\:043b\:044f\:0440\:0438\:0437\:0430\:0446\:0438\:0438: ",beta[[4]]," = ",beta];

      gamma={0,1,1,"\[Gamma]",Degree};
      Print["\:0423\:0433\:043e\:043b \:0432\:0440\:0430\:0449\:0435\:043d\:0438\:044f \:043e\:0431\:0440\:0430\:0437\:0446\:0430: ",gamma[[4]]," = ",gamma];

      ellipt={0,0,0.5,"e"};
      Print["\:042d\:043b\:043b\:0438\:043f\:0442\:0438\:0447\:043d\:043e\:0441\:0442\:044c \:043f\:0430\:0434\:0430\:044e\:0449\:0435\:0433\:043e \:0441\:0432\:0435\:0442\:0430: ",ellipt[[4]]," = ",N[ellipt]];
      *)
    ];
(* ============================================== *)
OutputIncidentRayInfo[ray___] :=
    Module[{},
      Print["OutputIncidentRayInfo::Invalid IncidentRay: ", ray];
    ];
(* ============================================== *)
OutputLayeredSystem[layeredSystem_?LayeredSystemQ] :=
    Module[{},
      Print["Layered System = ", layeredSystem];
      Print[strSeparator];
    ];
(* ============================================== *)
OutputLayeredSystem[layeredSystem___] :=
    Module[{},
      Print["OutputLayeredSystem::Invalid Layered System: ", layeredSystem];
    ];
(* ============================================== *)
