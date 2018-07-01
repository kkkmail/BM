(* ============================================== *)
(* :Summary: This module extends basic Berreman Matrix transformations. *)
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
BDAVGTYPESUMOFTWOPOINTS = 0;
BDAVGTYPEUSEAVGPERIODS = 1;
BDAVGTYPEUSEAVGPERIODSEXACTLAMBDA = 2;
BDAVGTYPESERIES = 100;
BDAVGTYPESTD = BDAVGTYPESERIES;

BDIMAGESIZE = 640;

BDPLTTEXTOPTS =
    {
      FontFamily -> "Courier",
      FontSize -> 18,
      FontWeight -> "Bold"
    };
(* ============================================== *)
Options[BerremanDirect] =
    {
      BerremanDirectVersion -> 6.04,
      RotateAll -> True,
      ConsecutiveRotation -> True,
      PrintTimeEstimate -> False,
      PrintCalculationProgress -> False,
      PrintCalculationDetails -> False,
      UseThickLastLayer -> False,
      NoOfAveragingPoints -> 5,
      AveragingPeriods -> 1,
      AveragingType -> BDAVGTYPESERIES,
      Amplitude -> 1,
      TransmittedAnalyzerAngle -> 0,
      TransmittedAnalyzerParallelAmplitude -> 1,
      TransmittedAnalyzerCrossedAmplitude -> 0,
      ReflectedAnalyzerAngle -> 0,
      ReflectedAnalyzerParallelAmplitude -> 1,
      ReflectedAnalyzerCrossedAmplitude -> 0,
      AnalyzerAngleAbsoluteValue -> False,
      UseAnalyzer -> False,
      UseMakeFileName -> False,
      EigenValueIndex -> 1,
      UsePlotLabel3D -> True,
      UseChop -> True,
      ChopTolerance -> 10^-7,
      ChopPrecision -> 6,
      PrintFunctionDescription -> True,
      IgnoreZeroIFullBase -> True,
      ZeroIFullBaseReplaceValue -> 1,
      Plot3DType -> Plot3D,
      BDPerformCalculations -> True,
      BDPlotFigures -> True,
      PlotOptions3D -> {PlotPoints -> 25, PlotRange -> All},
      PlotOptions2D -> {PlotPoints -> 25, PlotRange -> All, MaxRecursion -> 3}
    };

(*AveragingType-Wavelength averaging:0-sum of two points/2,1-averaging by NoOfAveragingPoints& using AveragingPeriods,2-same as 1,but lambda is exactly as supplied*)
(* ============================================== *)
ListPoints[lst_, 1] :=
    Module[{len},
      len = Length[lst];
      Return[lst[[len]]];
    ];

ListPoints[lst_, i_Integer] :=
    Module[{len},
      len = Length[lst];
      Return[lst[[len - i + 1]] * ListPoints[lst, i - 1]];
    ];
(* ============================================== *)
ListPointsFull[lst_] :=
    Module[{retval, len},
      len = Length[lst];
      retval = Table[ListPoints[lst, len - i + 1], {i, len}];
      Return[retval];
    ];
(* ============================================== *)
GetQuotient[lst_, idx_Integer, 0] := 1;
GetMod[lst_, idx_Integer, 0] := 1;
GetQuotient[lst_, idx_Integer, 1] := Quotient[idx, lst[[1]]];
GetMod[lst_, idx_Integer, 1] := Mod[idx, lst[[1]]];
(* ============================================== *)
GetMod[lst_, idx_Integer, i_Integer] := Mod[GetMod[lst, idx, i - 1], lst[[i]]];
GetQuotient[lst_, idx_Integer, i_Integer] :=
    If[i <= Length[lst], Quotient[GetMod[lst, idx, i - 1], lst[[i]]], GetMod[lst, idx, i - 1], 1];
(* ============================================== *)
GetIndexList[lst_, idx_Integer] :=
    Module[{retval, lsthlp, len},
      lsthlp = ListPointsFull[lst];len = Length[lsthlp];
      retval = Table[1 + GetQuotient[lsthlp, idx - 1, i + 1], {i, len}];
      Return[retval];
    ];
(* ============================================== *)
GetNumberOfPoints[v_] :=
    Module[{retval},
      retval = Round[If[Length[v] >= 3, 1 + If[v[[3]] != 0, ((v[[2]] - v[[1]]) / v[[3]]), 0, 0], 1, 1]];
      Return[retval];
    ];
(* ============================================== *)
GetValue[v_, i_, UseMultiplier_ : True] :=
    Module[{retval},
      retval = If[Length[v] >= 3, v[[1]] + (i - 1) * v[[3]], If[Length[v] >= 1, v[[1]], 0, 0], 0] * If[UseMultiplier === True, If[Length[v] >= 5, v[[5]], 1, 1], 1];
      Return[retval];
    ];
(* ============================================== *)
GetValueNumberOfPointsList[vlst_] :=
    Module[{retval, len},
      len = Length[vlst];
      retval = Table[GetNumberOfPoints[vlst[[i]]], {i, len}];
      Return[retval];
    ];
(* ============================================== *)
GetValueList[vlst_, idx_, UseMultiplier_ : True] :=
    Module[{retval, len, IdxLst, varlst},
      varlst = GetValueNumberOfPointsList[vlst];
      IdxLst = GetIndexList[varlst, idx];
      len = Length[IdxLst];
      retval = Table[GetValue[vlst[[i]], IdxLst[[i]], UseMultiplier], {i, len}];

      (* Print["GetValueList::retval[", idx, "] = ", retval]; *)

      Return[retval];
    ];
(* ============================================== *)
(* Functions to return min / max values of the variable *)
GetFirstValue[var_?VectorQ] := var[[1]];
GetLastValue[var_?VectorQ] := var[[2]];
(* ============================================== *)
(* TODO - Check and replace Media[[4,...]] *)
TransformMedia[Media_, varlist_, opts___] :=
    Module[{rotnew, rotall, MediaTrf, FilmTrf, FilmHlp, fi, theta, psi, flmLen, gamm, rotgamm, consRot, n1, n2, nOut, Descr, h2, Film, len, rotn, lmbd, eps, mu, ro, eps1, mu1, ro1, eps2, mu2, ro2, epsVal, muVal, roVal, eps1Val, mu1Val, ro1Val, eps2Val, mu2Val, ro2Val, flm, flmLayer, pdi, pdil, n2Val, eVal2},
      rotall = RotateAll /. opts /. Options[BerremanDirect];
      consRot = ConsecutiveRotation /. opts /. Options[BerremanDirect];
      gamm = VarListGetGamma[varlist];
      fi = VarListGetCommonFi[varlist];
      theta = VarListGetCommonTheta[varlist];
      psi = VarListGeCommonPsi[varlist];
      flmLen = FilmLength[MediaFilm[Media]];
      lmbd = VarListGetLambda[varlist];
      n1 = MediaUpperRefractionIndex[Media];
      n2 = MediaLowerRefractionIndex[Media];
      nOut = MediaOutRefractionIndex[Media];
      Descr = MediaDescription[Media];
      h2 = MediaSubstrateThickness[Media];

      pdi = PrintCommonDebugInfo /. opts /. Options[BerremanCommon];
      pdil = PrintCommonDebugInfoLevel /. opts /. Options[BerremanCommon];

      If[pdi == True,
        Print["   "];
        Print["TransformMedia::varlist = ", varlist];
        Print["TransformMedia::n1 = ", n1];
        Print["TransformMedia::n2 = ", n2];
        Print["TransformMedia::nOut = ", nOut];
      ];

      (*Print["n1: ",n1];*)

      FilmHlp = FilmNew[];

      Do[
        flm = MediaFilm[Media];
        flmLayer = flm[[i]];
        eps = FilmLayerEpsilon[flmLayer];
        mu = FilmLayerMu[flmLayer];
        ro = FilmLayerRo[flmLayer];
        epsVal = If[Head[eps] === Head[{}], eps, Apply[eps, {lmbd}]];
        muVal = If[Head[mu] === Head[{}], mu, Apply[mu, {lmbd}]];
        roVal = If[Head[ro] === Head[{}], ro, Apply[ro, {lmbd}]];
        FilmAddLayer[FilmHlp, FilmLayerNew[FilmLayerThickness[flmLayer], epsVal, muVal, roVal]],
        {i, flmLen}
      ];

      eps2 = MediaLowerEpsilon[Media];
      mu2 = MediaLowerMu[Media];
      ro2 = MediaLowerRo[Media];
      eps2Val = If[Head[eps2] === Head[{}], eps2, Apply[eps2, {lmbd}]];
      mu2Val = If[Head[mu2] === Head[{}], mu2, Apply[mu2, {lmbd}]];
      ro2Val = If[Head[ro2] === Head[{}], ro2, Apply[ro2, {lmbd}]];

      eps1 = MediaUpperEpsilon[Media];
      mu1 = MediaUpperMu[Media];
      ro1 = MediaUpperRo[Media];
      eps1Val = If[Head[eps1] === Head[{}], eps1, Apply[eps1, {lmbd}]];
      mu1Val = If[Head[mu1] === Head[{}], mu1, Apply[mu1, {lmbd}]];
      ro1Val = If[Head[ro1] === Head[{}], ro1, Apply[ro1, {lmbd}]];

      eVal2 = Eigenvalues[eps2Val . mu2Val];
      n2Val = Sqrt[eVal2[[1]]];

      If[pdi == True,
        Print["   "];
        Print["TransformMedia::eps1 = ", eps1 // MatrixForm];
        Print["TransformMedia::mu1 = ", mu1 // MatrixForm];
        Print["TransformMedia::ro1 = ", ro1 // MatrixForm];
        Print["   "];
        Print["TransformMedia::eps1Val = ", eps1Val // MatrixForm];
        Print["TransformMedia::mu1Val = ", mu1Val // MatrixForm];
        Print["TransformMedia::ro1Val = ", ro1Val // MatrixForm];
        Print["   "];
        Print["TransformMedia::eps2 = ", eps2 // MatrixForm];
        Print["TransformMedia::mu2 = ", mu2 // MatrixForm];
        Print["TransformMedia::ro2 = ", ro2 // MatrixForm];
        Print["   "];
        Print["TransformMedia::eps2Val = ", eps2Val // MatrixForm];
        Print["TransformMedia::mu2Val = ", mu2Val // MatrixForm];
        Print["TransformMedia::ro2Val = ", ro2Val // MatrixForm];
        Print["TransformMedia::eVal2 = ", eVal2 // MatrixForm];
        Print["TransformMedia::n2Val = ", n2Val];
      ];

      MediaTrf = MediaNew[n1, n2Val, gamm, FilmHlp, Descr, nOut, h2, eps2Val, mu2Val, ro2Val, eps1Val, mu1Val, ro1Val];
      (* Print["TransformMedia::MediaTrf = ",MediaTrf]; *)

      If[consRot === True,
      (*Print["Consecutive Rotations."];*)
        If[rotall === True,
        (*Print["Rotating all layers equally..."];*)
        (*Print["MediaTrf[[3]] before rotation: ",MediaTrf[[3]]];*)
          MediaTrf[[4]] = FilmTransformAll[MediaFilm[MediaTrf], RotationNew[fi, theta, psi, opts]];,
        (*Print["Rotating every layer individually..."];*)
          Do[
            MediaTrf[[4, i]] = FilmLayerTransform[MediaTrf[[4, i]], RotationNew[VarListGetFi[varlist, i], VarListGetTheta[varlist, i], VarListGetPsi[varlist, i]], opts],
            {i, flmLen}
          ];
        ];

        If[Chop[gamm] =!= 0, (*Print["Rotating for Gamma = ",gamm];*)
          MediaTrf[[4]] = FilmTransformAll[MediaFilm[MediaTrf], RotationNew[gamm, 0, 0, opts], False];
        ];,
      (*Print["Sum Rotations."];*)
        If[rotall === True, (*Print["Rotating all layers equally..."];*)
          MediaTrf[[4]] = FilmTransformAll[MediaFilm[MediaTrf], RotationNew[fi + gamm, theta, psi, opts]];,
        (*Print["Rotating every layer individually..."];*)
          Do[
            MediaTrf[[4, i]] = FilmLayerTransform[MediaTrf[[4, i]], RotationNew[VarListGetFi[varlist, i], VarListGetTheta[varlist, i], VarListGetPsi[varlist, i]], opts], {i, flmLen}
          ];
        ];
      ];

      (*Rotating substrate for angle Gamma-not the best code, but at least easier to understand and check *)(*If[Chop[gamm]=!=0,*)(*In 3.03.024 fixed rotation of substrate as it can be anisotropic*)

      FilmTrf = MediaFilm[MediaTrf];
      (*rotn=RotationNew[gamm,0,0,opts];*)

      rotn = RotationNew[fi + gamm, theta, psi, opts];
      MediaTrf = MediaNew[n1, n2Val, gamm, FilmTrf, Descr, nOut, h2, Transform[eps2Val, rotn], Transform[mu2Val, rotn], Transform[ro2Val, rotn], Transform[eps1Val, rotn], Transform[mu1Val, rotn], Transform[ro1Val, rotn]];

      Do[
        MediaTrf[[4, i, 1]] = VarListGetThickness[varlist, i],
        {i, flmLen}
      ];
      (*Print["MediaTrf[[3]] BEFORE RETURN: ",MediaTrf[[3]]];*)

      If[pdi == True,
        Print["   "];
        Print["TransformMedia::MediaTrf = ", MediaTrf];
        Print["   "];
      ];
      Return[MediaTrf];
    ];
(* ============================================== *)
GetSol[Calc_, idx_] :=
    Module[{sol, Media, inclght, opts, MediaTrf, VarList, values, Ampl, pdi, pdil},
    (* Print["GetSol::Starting..."]; *)
      Media = Calc[[1]][[1]];
      VarList = Calc[[1]][[2]];
      opts = Calc[[2]];
      values = GetValueList[VarList, idx];

      Ampl = Amplitude /. opts /. Options[BerremanDirect];
      MediaTrf = TransformMedia[Media, values, opts];
      inclght = IncidentLightNew[values[[1]], values[[2]], values[[3]], MediaUpperRefractionIndex[Media], Ampl, values[[5]]];

      pdi = PrintCommonDebugInfo /. opts /. Options[BerremanCommon];
      pdil = PrintCommonDebugInfoLevel /. opts /. Options[BerremanCommon];

      If[pdi == True,
        Print["   "];
        Print["GetSol::VarList = ", VarList];
        Print["GetSol::values = ", N[values], ", idx = ", idx];
        Print["GetSolAvg::inclght = ", N[inclght]];
        Print["GetSolAvg::MediaTrf = ", N[MediaTrf]];
        Print["   "];
      ];

      sol = Chop[SolutionNew[MediaTrf, inclght, opts]];
      Return[sol];
    ];
(* ============================================== *)
GetSolAvg[Calc_, idx_] :=
    Module[{sol, Media, inclght, opts, MediaTrf, VarList, values, Ampl, elpct, pdi, pdil},
    (* Print["GetSolAvg::Starting..."]; *)
      Media = Calc[[1]][[1]];
      VarList = Calc[[1]][[2]];
      opts = Calc[[2]];
      values = GetValueList[VarList, idx];
      Ampl = Amplitude /. opts /. Options[BerremanDirect];
      MediaTrf = TransformMedia[Media, values, opts];

      pdi = PrintCommonDebugInfo /. opts /. Options[BerremanCommon];
      pdil = PrintCommonDebugInfoLevel /. opts /. Options[BerremanCommon];

      inclght = IncidentLightNew[values[[1]], values[[2]], values[[3]], MediaUpperRefractionIndex[Media], Ampl, values[[5]]];

      If[pdi == True,
        Print["   "];
        Print["GetSolAvg::VarList = ", VarList];
        Print["GetSolAvg::values = ", N[values], ", idx = ", idx];
        Print["GetSolAvg::inclght = ", N[inclght]];
        Print["GetSolAvg::MediaTrf = ", N[MediaTrf]];
        Print["   "];
      ];

      sol = Chop[SolutionAverageNew[MediaTrf, inclght, opts]];
      Return[sol];
    ];
(* ============================================== *)
(*Ok we expect VarList to be in the form:{lambda,fita,beta,gamma,fi,theta,psi,fi1,theta1,psi1,h1,fi2,theta2,psi2,h2,...},where each element has a form{startval,endval,step,name,multiplier} and the first sublist is general settings and each subsequent is for each layer.*)
GetInptLen[Calc_] :=
    Module[{TotalNumberOfPoints, Var, VarLen},
      Var = Calc[[1]][[2]];
      VarLen = Length[Var];
      TotalNumberOfPoints = Product[GetNumberOfPoints[Var[[VarIterator]]], {VarIterator, 1, VarLen}];
      Return[TotalNumberOfPoints];
    ];
(* ============================================== *)
GetSolArr[Calc_, idx_, len_] :=
    Module[{retval, sol},
      sol = GetSol[Calc, idx];
      retval = Table[{sol}, {len}];
      Return[retval];
    ];

(* ============================================== *)
GetSolArrAvg[Calc_, idx_, len_] :=
    Module[{retval, sol},
      sol = GetSolAvg[Calc, idx];
      retval = Table[{sol}, {len}];
      Return[retval];
    ];
(* ============================================== *)
GetFuncList[Calc_] :=
    Module[{retval},
      retval = Calc[[1]][[3]];
      Return[retval];
    ];
(* ============================================== *)
GetFuncArr[Calc_, idx_] :=
    Module[{solArr, len, FuncList, retval, utll, solArrAvg, avgtyp, IsAverageArr, lstTbl, fnelhlp1, fnelhlp2, ueFuncListHlp1, solArrHlp1, solArrHlp2, opts, pdi, pdil},

      opts = CalcOptions[Calc];

      pdi = PrintCommonDebugInfo /. opts /. Options[BerremanCommon];
      pdil = PrintCommonDebugInfoLevel /. opts /. Options[BerremanCommon];

      If[pdi == True,
        Print["   "];
        Print["GetFuncArr:: idx = ", idx];
      ];

      FuncList = Flatten[{GetFuncList[Calc]}];

      len = Length[FuncList];
      IsAverageArr = Table[True, {len}];
      utll = UseThickLastLayer /. opts /. Options[BerremanDirect];
      avgtyp = AveragingType /. opts /. Options[BerremanDirect];
      lstTbl = Table[List, {i, len}];
      fnelhlp1 = MapThread[Apply, {lstTbl, FuncList}];
      fnelhlp2 = MapThread[Head, {FuncList}];
      ueFuncListHlp1 = Table[If[Head[fnelhlp1[[i]]] === Head[{}], fnelhlp2[[i]], fnelhlp1[[i]]], {i, len}];
      solArrHlp1 = Table[If[Head[fnelhlp1[[i]]] === Head[{}], fnelhlp1[[i]], {}], {i, len}];

      If[pdi == True,
        Print["GetFuncArr::FuncList = ", FuncList];
        Print["GetFuncArr::IsAverageArr = ", IsAverageArr];
        Print["GetFuncArr::lstTbl = ", lstTbl];
        Print["GetFuncArr::fnelhlp1 = ", fnelhlp1];
        Print["GetFuncArr::fnelhlp2 = ", fnelhlp2];
        Print["GetFuncArr::ueFuncListHlp1 = ", ueFuncListHlp1];
        Print["GetFuncArr::solArrHlp1 = ", solArrHlp1];
      ];

      (*xxx=MapThread[Apply,{ueFuncListHlp1,solArrHlp2}];
    Print["xxx=",xxx];*)

      If[avgtyp === BDAVGTYPESERIES, IsAverageArr = Table[False, {len}]];

      If[utll === True,
        (
          If[pdi == True,
            Print["GetFuncArr::Using thick last layer..."];
          ];

          solArrAvg = GetSolArrAvg[Calc, idx, len];

          If[pdi == True,
            Print["GetFuncArr::solArrAvg = ", solArrAvg];
          ];

          retval = MapThread[AveragingFunc, {FuncList, solArrAvg, IsAverageArr}];
        (* retval=MapThread[AveragingFunc,{ueFuncListHlp1,solArrHlp1,solArrAvg,IsAverageArr}]; *)
        )
        ,
        (
          solArr = GetSolArr[Calc, idx, len];
          solArrHlp2 = Table[Join[solArrHlp1[[i]], solArr[[i]]], {i, len}];

          If[pdi == True,
            Print["GetFuncArr::solArrHlp2 = ", solArrHlp2];
          ];

          (*retval=MapThread[Apply,{FuncList,solArr}];*)

          retval = MapThread[Apply, {ueFuncListHlp1, solArrHlp2}];
        )
      ];

      If[pdi == True,
        Print["GetFuncArr::retval = ", retval];
        Print["GetFuncArr completed."];
      ];

      Return[retval];
    ];
(* ============================================== *)
GetInput[Calc_] :=
    Module[{retval, maxcnt, VarList},
      maxcnt = GetInptLen[Calc];
      VarList = Calc[[1]][[2]];
      retval = Table[GetValueList[VarList, idx, False], {idx, maxcnt}];
      Return[retval];
    ];
(* ============================================== *)
GetOutput[Calc_] :=
    Module[{retval, maxcnt, useParallelTbl, clcOpts, CalcArr, CalcArrOut, idx},
      maxcnt = GetInptLen[Calc];
      clcOpts = CalcOptions[Calc];
      useParallelTbl = UseParallelTable /. Flatten[{clcOpts}] /. Options[BerremanInit];
      If[useParallelTbl == False || maxcnt <= 4,
        (retval = Table[GetFuncArr[Calc, idx], {idx, maxcnt}];),
        (
          Print["Parallel calculations starting ..."];
          Print["GetOutput::maxcnt = ", maxcnt];
          Print["TODO::GetOutput::Parallel calculations may not work properly ..."];


          DistributeDefinitions[Calc, maxcnt];
          retval = ParallelTable[GetFuncArr[Calc, idx], {idx, maxcnt}];

          (*
          CalcArr=ParallelTable[Calc,{idx,maxcnt}];
          retval=ParallelTable[GetFuncArr[CalcArr[[idx]],idx],{idx,maxcnt}];
          *)

          Print["Parallel calculations completed."];
        )
      ];
      Return[retval];
    ];
(* ============================================== *)
defaultCommonAnglesInfo = {{0, 0, 1, "\[CurlyPhi]", Degree}, {0, 0, 1, "\[Theta]", Degree}, {0, 0, 1, "\[Psi]", Degree}};

VarListNew[IncidentLightInfo : {{_, _, _, _, _}, {_, _, _, _, _}, {_, _, _, _, _}, {_, _, _, _, _}}] :=
    VarListNew[IncidentLightInfo, defaultCommonAnglesInfo];
(* ============================================== *)
VarListNew[IncidentLightInfo : {{_, _, _, _, _}, {_, _, _, _, _}, {_, _, _, _, _}, {_, _, _, _, _}}, CommonAnglesInfo : {{_, _, _, _, _}, {_, _, _, _, _}, {_, _, _, _, _}}] :=
    Module[{retval},
      retval = Join[IncidentLightInfo, {{0, 0, 1, "Ellipticity"}}, CommonAnglesInfo];
      Return[retval];
    ];
(* ============================================== *)
VarListNew[IncidentLightInfo : {{_, _, _, _, _}, {_, _, _, _, _}, {_, _, _, _, _}, {_, _, _, _, _}, {_, _, _, _}}] :=
    VarListNew[IncidentLightInfo, defaultCommonAnglesInfo];
(* ============================================== *)
VarListNew[IncidentLightInfo : {{_, _, _, _, _}, {_, _, _, _, _}, {_, _, _, _, _}, {_, _, _, _, _}, {_, _, _, _}}, CommonAnglesInfo : {{_, _, _, _, _}, {_, _, _, _, _}, {_, _, _, _, _}}] :=
    Module[{retval},
      retval = Join[IncidentLightInfo, CommonAnglesInfo];
      Return[retval];
    ];
(* ============================================== *)
VarListLambdaIdx = 1;
VarListFitaIdx = 2;
VarListBetaIdx = 3;
VarListGammaIdx = 4;
VarListEllipticityIdx = 5;
(* ============================================== *)
VarListGetLambda[vl_] := Module[{}, Return[vl[[1]]]];
VarListGetFita[vl_] := Module[{}, Return[vl[[2]]]];
VarListGetBeta[vl_] := Module[{}, Return[vl[[3]]]];
VarListGetGamma[vl_] := Module[{}, Return[vl[[4]]]];
VarListGetEllipticity[vl_] := Module[{}, Return[vl[[5]]]];

VarListGetCommonFi[vl_] := Module[{}, Return[vl[[6]]]];
VarListGetCommonTheta[vl_] := Module[{}, Return[vl[[7]]]];
VarListGeCommonPsi[vl_] := Module[{}, Return[vl[[8]]]];

VarListGetFi[vl_, idx_] := Module[{}, Return[vl[[9 + (idx - 1) * 4]]]];
VarListGetTheta[vl_, idx_] := Module[{}, Return[vl[[10 + (idx - 1) * 4]]]];
VarListGetPsi[vl_, idx_] := Module[{}, Return[vl[[11 + (idx - 1) * 4]]]];
VarListGetThickness[vl_, idx_] := Module[{}, Return[vl[[12 + (idx - 1) * 4]]]];
(* ============================================== *)
Attributes[VarListAddLayer] = {HoldFirst};
VarListAddLayer[VarList_, LayerAngleInfo : {{_, _, _, _, _}, {_, _, _, _, _}, {_, _, _, _, _}, {_, _, _, _, _}}] :=
    Module[{retval},
      VarList = Join[VarList, LayerAngleInfo];
      Return[VarList];
    ];
(* ============================================== *)
calcNewWarnCount = 0;
(*CalcNew returns {{Media,VarList,FuncList,FuncNameList,Description},{OptionsList},{{},{},0}}*)
(*Calc[[1]]={Media,VarList,FuncList,FuncNameList,Description},Calc[[2]]={Options},Calc[[3]]={inpt,outpt,runtime}*)
(*Media=See BerremanCommon for details*)
(*VarList={{lambda,fita,beta,gamma,ellipticity[,fi,theta,psi]}[,{fi1,theta1,psi1,h1},{fi2,theta2,psi2,h2},...]}*)
(*Each element has a form {startval,endval,step,name,multiplier} and the first sublist is general settings and each subsequent is for each layer.*)
(*optrul=Flatten[{ProcessOptionNames[Flatten[{opts,Options[BerremanDirect]}]]}];*)
CalcNew[Media_, VarList_, FuncList_, Description_ : "", opts___] :=
    Module[{retval, optrul, uanz, len, f, optf, ua, FuncNameListHlp, FuncDescList},
      uanz = {UseAnalyzer -> False};
      (* Print["CalcNew::start"]; *)
      len = Length[FuncList];
      FuncNameListHlp = GetFuncNameList[FuncList, opts];
      FuncDescList = GetFuncDescList[FuncList, opts];

      (*
      Print["CalcNew::len = ", len];
      Print["CalcNew::FuncList = ", FuncList];
      Print["CalcNew::FuncNameListHlp = ", FuncNameListHlp];
      Print["CalcNew::FuncDescList = ", FuncDescList];
      *)

      Do[
      (* Print["i = ", i]; *)
        f = FuncList[[i]];
        (* Print["f = ", f]; *)
        optf = GetFunctionOptions[f];
        (* Print["optf = ", optf]; *)
        ua = UseAnalyzer /. optf /. Options[FieldAlgebra] /. Options[BerremanDirect];
        (* Print["ua = ", ua]; *)
        If[ua === True, uanz = {UseAnalyzer -> True}], {i, len}
      ];
      optrul = Flatten[{uanz, opts}];
      retval = {{Media, VarList, FuncList, FuncNameListHlp, Description, FuncDescList}, optrul, {{}, {}, 0, {0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0}}};
      (* Print["CalcNew::retval = ", retval]; *)

      If[calcNewWarnCount = 0,
        (
          calcNewWarnCount = 1;
          Print["CalcNew::TODO 20180505 - Cannot successfully call CalcPrintTimeEstimate if function is supplied instead of eps / mu / rho."];
        )
      ];

      (* CalcPrintTimeEstimate[retval]; *)
      (*Ok We should not do that but we do to take care of nOut for semiinfinite media (in the next version,may be...)*)(*n2-retval[[1,1,2]];*)(*nOut-retval[[1,1,5]];*)
      (* Print["CalcNew::end"]; *)
      Return[retval];
    ];
(* ============================================== *)
Attributes[CalcPerform] = {HoldFirst};
CalcPerform[Calc_] :=
    Module[{retval, outpt, inpt, tstart, ttdiff, fulltstart, fulltend, prtCalcProgr, opts},
    (* Print["CalcPerform::start"]; *)
    (* opts=Calc[[2]]; *)
      opts = CalcOptions[Calc];
      prtCalcProgr = PrintCalculationProgress /. opts /. Options[BerremanDirect];
      tstart = TimeUsed[];fulltstart = Date[];

      If[prtCalcProgr === True,
        Print["Starting calculation for: ", CalcDescription[Calc]];
        Print["Start time: ", DateTimeToString[fulltstart]]
      ];

      outpt = GetOutput[Calc];
      inpt = GetInput[Calc];
      fulltend = Date[];
      ttdiff = TimeUsed[] - tstart;
      Calc[[3]] = {inpt, outpt, ttdiff, fulltstart, fulltend};

      If[prtCalcProgr === True, Print["Calculation completed. End time: ", DateTimeToString[fulltend]];Print[strSeparator];
      ];

      (* Print["CalcPerform::end"]; *)
      Return[Calc];
    ];
(* ============================================== *)
(* CalcPerformParallel[Calc_]:=Module[{retval,outpt,inpt,tstart,ttdiff,fulltstart,fulltend,prtCalcProgr,opts},
retval=Calc;
opts=retval[[2]];
prtCalcProgr=PrintCalculationProgress/.opts/.Options[BerremanDirect];
tstart=TimeUsed[];fulltstart=Date[];
If[prtCalcProgr===True,Print["Starting calculation for: ",CalcDescription[retval]];
Print["Start time: ",DateTimeToString[fulltstart]]];
outpt=GetOutput[retval];inpt=GetInput[retval];fulltend=Date[];
ttdiff=TimeUsed[]-tstart;
retval[[3]]={inpt,outpt,ttdiff,fulltstart,fulltend};
If[prtCalcProgr===True,Print["Calculation completed. End time: ",DateTimeToString[fulltend]];Print[strSeparator];];
Return[retval];
];
*)
(* ============================================== *)
GetFuncNameList[FuncList_, opts___] :=
    Module[{FuncNameList, len, lenFunc, idx, FuncName},
      len = Length[FuncList];
      FuncNameList = Table["?", {len}];
      (*Print["FuncNameList = ",FuncNameList];*)

      Do[
        FuncName = GetFunctionName[FuncList[[idx]]];
        FuncNameList[[idx]] = FuncName
        , {idx, len}
      ];

      (* Print["GetFuncNameList::FuncNameList = ",FuncNameList]; *)

      Return[FuncNameList];
    ];
(* ============================================== *)
GetFuncDescList[FuncList_, opts___] :=
    Module[{FuncDescList, len, lenFunc, idx, FuncDesc},
      len = Length[FuncList];
      FuncDescList = Table["?", {len}];
      (*Print["FuncDescList = ",FuncDescList];*)

      Do[FuncDesc = GetFunctionDesc[FuncList[[idx]]];
      FuncDescList[[idx]] = FuncDesc, {idx, len}
      ];

      (*Print["FuncDescList = ",FuncDescList];*)

      Return[FuncDescList];];
(* ============================================== *)
CalcPlot[Calc_, pltOptsRaw___] :=
    Module[{FuncList, FuncNameList, len, clcOpts, prtfDesc, FuncDescList},
    (*Print["CalcPlot"];*)

      clcOpts = CalcOptions[Calc];
      FuncDescList = CalcFuncDescList[Calc];
      prtfDesc = PrintFunctionDescription /. clcOpts /. Options[BerremanDirect];
      FuncList = CalcFuncList[Calc];
      FuncNameList = CalcFuncNameList[Calc];
      len = Min[Length[FuncList], Length[FuncNameList]];

      Do[
        If[prtfDesc === True, Print[FuncDescList[[i]]]];
        CalcPlotFunc[Calc, FuncList[[i]], FuncNameList[[i]], pltOptsRaw],
        {i, len}
      ];
    ];
(* ============================================== *)
CalcPlotFunc[Calc_, Func_, FuncName_, pltOptsRaw___] :=
    Module[{f, xVar, xStart, xEnd, plotOpts, Media, varLst, vLen, xName, yName, xVarCnt, var, xAssigned, varNew, opts, nnVar, nnVarMax, funcLst, isNumericN, funcNameLst, fStr, execStr, plotOptsStr, s, ii},
      Media = CalcMedia[Calc];
      varLst = CalcVarList[Calc];
      vLen = Length[varLst];
      xAssigned = False;
      varNew = varLst;
      opts = Flatten[{PrintTimeEstimate -> False, PrintCalculationProgress -> False, PrintCalculationDetails -> False, CalcOptions[Calc]}];

      funcLst = Flatten[{Func}];
      funcNameLst = Flatten[{FuncName}];

      xName = "x";

      If[ToString[Head[FuncName]] != "List",
        yName = ToString[InputForm[FuncName]]
        ,
        s = "";
        For[ii = 1, ii <= Length[FuncName], ii++,
          s = s <> ToString[InputForm[FuncName[[ii]]]];
          If[ii != Length[FuncName], s = s <> "," <> "\", \"" <> ","];
        ];
        yName = "DisplayForm[RowBox[{" <> s <> "}]]";
      ];

      xVarCnt = 1;
      xStart = 0;
      xEnd = 1;

      (*
    Print["CalcPlotFunc::yName = ",yName];
    Print["CalcPlotFunc::FuncName = ",FuncName];
    Print["CalcPlotFunc::ToString[InputForm[yName]] = ",ToString[InputForm[yName]]];
    Print["CalcPlotFunc::InputForm[ToBoxes[yName]]] = ",InputForm[ToBoxes[yName]]];
    Print["CalcPlotFunc::varLst = ", varLst];Print["CalcPlotFunc::Options[Func] = ", Options[Func]];
    *)

      Do[var = varLst[[vCnt]];
      If[var[[1]] + var[[3]] <= var[[2]] && xAssigned === False,
        xAssigned = True;
        xVarCnt = vCnt;
        xStart = var[[1]];
        xEnd = var[[2]];
        xName = var[[4]];];
      varNew[[vCnt, 2]] = varNew[[vCnt, 1]];
        ,
        {vCnt, vLen}
      ];

      (*Print["=============="];Print["CalcPlotFunc::xName = ",xName];*)
      nnVarMax = Length[funcLst];

      FuncPlot[xVar_, nnVar_] :=
          Module[{varN, clc, outpt, retval, isNumericX, nn},
            isNumericN = NumericQ[nnVar];

            (*
        Print["CalcPlotFunc::xVar = ", xVar];
        Print["CalcPlotFunc::nnVar = ", nnVar];
        Print["CalcPlotFunc::isNumericN = ", isNumericN];
        *)

            If[isNumericN,
              nn = Max[Min[Round[nnVar], nnVarMax], 1];
              varN = varNew;
              varN[[xVarCnt, 1]] = xVar;
              varN[[xVarCnt, 2]] = xVar;
              clc = CalcNew[Media, varN, {funcLst[[nn]]}, "", opts];
              CalcPerform[clc];
              outpt = CalcGetOutput[clc];
              retval = N[ChopValue[outpt[[1, 1]], opts]];
              ,
              retval = -10;
            ];
            Return[retval];
          ];

      SetAttributes[FuncPlot, NumericFunction];

      plotOptsStr = "{" <> ToString[InputForm[pltOptsRaw]] <> ", FrameLabel -> {" <> ToString[InputForm[xName]] <> ", " <> yName <> "}, PlotRange -> All, LabelStyle -> {FontFamily -> \"Courier\", FontSize -> 18, FontWeight -> \"Bold\"}, ImageSize -> 640, Frame -> True, GridLines -> Automatic, PlotStyle -> {{Thickness[0.005]}, {Thickness[0.005], Dashing[Large]}, {Thickness[0.005], Dashing[Medium]}, {Thickness[0.005], Dashing[Small]}, {Thickness[0.005], Dashing[Tiny]}, {Thickness[0.005], Dashing[{0.09, 0.01}]}, {Thickness[0.005], Dashing[{0.07, 0.03}]}}}";

      fStr = ToString[Table["FuncPlot[xVar," <> ToString[nn] <> "]", {nn, 1, nnVarMax}]];

      execStr = "Print[Plot[" <> fStr <> ",{xVar," <> ToString[xStart] <> "," <> ToString[xEnd] <> "}," <> plotOptsStr <> "]]";

      (*
      Print["CalcPlotFunc::execStr = ",execStr];
      Print["CalcPlotFunc::xStart = ",xStart];
      Print["CalcPlotFunc::xEnd = ",xEnd];
      *)

      ToExpression[execStr];
    ];
(* ============================================== *)
ChopValue[val_, opts___] :=
    Module[{retval, uc, cTol, cPrec},
      uc = UseChop /. opts /. Options[BerremanDirect];
      cTol = ChopTolerance /. opts /. Options[BerremanDirect];
      cPrec = ChopPrecision /. opts /. Options[BerremanDirect];
      retval = If[uc === False, val, Chop[cTol * Round[N[(1 / cTol) * val]], cTol]];
      Return[retval];
    ];
(* ============================================== *)
CalcPlot3D[Calc_, SwapXY_, pltOptsRaw___] :=
    Module[{FuncList, FuncNameList, len, CalcGridArr, pltPoints, pltPointsX, pltPointsY, fCalc, xVar, yVar, xStart, xEnd, yStart, yEnd, plotOpts, Media, varLst, vLen, xName, yName, zName, xVarCnt, yVarCnt, var, xAssigned, yAssigned, varNew, opts, i, j, k, f, usePltLbl, clcOpts, prtfDesc, FuncDescList, useParallelTbl, meshVar, colorFunc, colorFuncScaling, data, plt3Dtype, funcName, fTmp, isNumericX, isNumericY, xVarr, yVarr, MyTable, MyApproximateFunc},
    (* Print["CalcPlot3D"]; *)
      clcOpts = CalcOptions[Calc];

      FuncList = Flatten[{CalcFuncList[Calc]}];
      FuncDescList = Flatten[{CalcFuncDescList[Calc]}];
      FuncNameList = Flatten[{CalcFuncNameList[Calc]}];

      (*
    Print["CalcPlot3D::FuncList = ", FuncList];
    Print["CalcPlot3D::FuncDescList = ", FuncDescList];
    Print["CalcPlot3D::FuncNameList = ", FuncNameList];
    *)

      prtfDesc = PrintFunctionDescription /. clcOpts /. Options[BerremanDirect];
      pltPoints = PlotPoints /. Flatten[{pltOptsRaw}] /. {PlotPoints -> 25};
      pltPointsX = Flatten[{{pltPoints}, {pltPoints}}][[1]];
      pltPointsY = Flatten[{{pltPoints}, {pltPoints}}][[2]];

      meshVar = {pltPointsX - 2, pltPointsY - 2};
      usePltLbl = UsePlotLabel3D /. Flatten[{pltOptsRaw}] /. Options[BerremanDirect] /. {UsePlotLabel3D -> True};
      useParallelTbl = UseParallelTable /. Flatten[{pltOptsRaw}] /. Flatten[{clcOpts}] /. Options[BerremanInit];
      Media = CalcMedia[Calc];

      varLst = CalcVarList[Calc];
      varNew = varLst;
      vLen = Length[varLst];

      xAssigned = False;
      yAssigned = False;

      opts = {PrintTimeEstimate -> False, PrintCalculationProgress -> False, PrintCalculationDetails -> False, CalcOptions[Calc]};

      xName = "x";
      yName = "y";
      zName = "z";
      xVarCnt = 1;
      yVarCnt = 2;
      xStart = 0;
      xEnd = 1;
      yStart = 0;
      yEnd = 1;

      colorFunc = (Blend[{RGBColor[255 / 255, 216 / 255, 176 / 255], RGBColor[79 / 255, 47 / 255, 176 / 255]}, #3]&);
      colorFuncScaling = True;

      (* Print["Starting Do..."]; *)

      Do[
        var = varLst[[vCnt]];
        If[var[[1]] + var[[3]] <= var[[2]],
          If[SwapXY === True,
            If[yAssigned === False,
              yAssigned = True;
              yVarCnt = vCnt;
              yStart = var[[1]];
              yEnd = var[[2]];
              yName = var[[4]];,
              If[xAssigned === False,
                xAssigned = True;
                xVarCnt = vCnt;
                xStart = var[[1]];
                xEnd = var[[2]];
                xName = var[[4]];
              ];
            ];,
            If[xAssigned === False,
              xAssigned = True;
              xVarCnt = vCnt;
              xStart = var[[1]];
              xEnd = var[[2]];
              xName = var[[4]];,
              If[yAssigned === False,
                yAssigned = True;
                yVarCnt = vCnt;
                yStart = var[[1]];
                yEnd = var[[2]];
                yName = var[[4]];
              ];
            ];
          ];
        ];
        varNew[[vCnt, 2]] = varNew[[vCnt, 1]];,
        {vCnt, vLen}
      ];

      (* Print["=============="];Print["xName = ",xName];Print["xAssigned = ", xAssigned, ", yAssigned = ", yAssigned]; *)

      If[xAssigned === True && yAssigned === True,
        len = Min[Length[FuncList], Length[FuncNameList]];

        fCalc[xVar_, yVar_] :=
            Module[{varN, clc, outpt, retval},
              varN = varNew;
              varN[[xVarCnt, 1]] = xVar;
              varN[[xVarCnt, 2]] = xVar;
              varN[[yVarCnt, 1]] = yVar;
              varN[[yVarCnt, 2]] = yVar;
              clc = CalcNew[Media, varN, FuncList, "", opts];
              CalcPerform[clc];
              retval = CalcGetOutput[clc];
              Return[retval];
            ];

        (* Print["fCalc[",xStart,", ",yStart,"] = ",fCalc[xStart,yStart]]; *)

        If[useParallelTbl == False,
          CalcGridArr = Table[fCalc[xStart + (i - 1) * (xEnd - xStart) / (pltPointsX - 1), yStart + (j - 1) * (yEnd - yStart) / (pltPointsY - 1)], {i, 1, pltPointsX}, {j, 1, pltPointsY}],
          ((* Print["Using parallel table ..."]; ,CalcNew,CalcPerform,CalcGetOutput,GetFunctionOptions *)
            DistributeDefinitions[fCalc, xStart, xEnd, pltPointsX, yStart, yEnd, pltPointsY, xVarCnt, yVarCnt, varNew, Media, FuncList, opts];
            CalcGridArr = ParallelTable[fCalc[xStart + (i - 1) * (xEnd - xStart) / (pltPointsX - 1), yStart + (j - 1) * (yEnd - yStart) / (pltPointsY - 1)], {i, 1, pltPointsX}, {j, 1, pltPointsY}];
            WaitAll[CalcGridArr];
          )
        ];

        fTmp = "";
        Do[
          If[prtfDesc === True, Print[FuncDescList[[k]]]];
          f[xVar_, yVar_] := Module[{varN, clc, outpt, retval, iii, jjj},
            isNumericX = NumericQ[xVar];
            isNumericY = NumericQ[yVar];

            (*
    Print["xVar = ",xVar,", yVar = ",yVar];
    Print["isNumericX = ",isNumericX,", isNumericY = ",isNumericY];
    *)

            If[isNumericX && isNumericY,
              iii = Max[Min[1 + Round[(pltPointsX - 1) * (xVar - xStart) / (xEnd - xStart)], pltPointsX], 1];
              jjj = Max[Min[1 + Round[(pltPointsY - 1) * (yVar - yStart) / (yEnd - yStart)], pltPointsY], 1];
              outpt = CalcGridArr[[iii, jjj]];

              (*
    Print["iii = ",iii,", jjj = ",jjj,", k = ",k];
    Print["outpt = ",outpt];
    *)

              retval = N[ChopValue[outpt[[1, k]], clcOpts]];
              ,
              retval = -10;
            ];
            SetAttributes[f, NumericFunction];

            fTmp = fTmp <> ToString[retval] <> ", ";
            (* Print["CalcPlot3D::f::retval = ",retval]; *)
            Return[retval];
          ];

          zName = FuncNameList[[k]];
          funcName = FuncList[[k]];

          MyTable = Flatten[Table[{xVar, yVar, f[xVar, yVar]}, {xVar, xStart, xEnd, (xEnd - xStart) / (pltPointsX - 1)}, {yVar, yStart, yEnd, (yEnd - yStart) / (pltPointsY - 1)}], 1];
          MyApproximateFunc = Interpolation[MyTable];

          (*
    Print["CalcPlot3D::CalcGridArr = ", N[CalcGridArr]];
    Print["CalcPlot3D::xStart = ", xStart];
    Print["CalcPlot3D::xEnd = ", xEnd];
    Print["CalcPlot3D::yStart = ", yStart];
    Print["CalcPlot3D::yEnd = ", yEnd];
    Print["CalcPlot3D::pltPointsX = ", pltPointsX];
    Print["CalcPlot3D::pltPointsY = ", pltPointsY];
    Print["CalcPlot3D::zName = ", zName];
    Print["CalcPlot3D::FuncList = ", FuncList];
    *)

          (* TODO - plt3Dtype does not work *)
          (*
    plt3Dtype=ToString[(Plot3DType /. (Options[funcName] /.clcOpts)) /. Options[BerremanDirect]];
    *)

          plt3Dtype = "Plot3D";

          (* Print["funcName = ", funcName,", plt3Dtype = ",plt3Dtype]; *)
          (* Print["Before Plot"]; *)
          (* Print[Evaluate[plotOpts]]; *)

          If[plt3Dtype == "RevolutionPlot3D",
            If[usePltLbl === True,
              plotOpts = {pltOptsRaw, AxesLabel -> {xName, yName, ""}, PlotLabel -> zName, ImageSize -> BDIMAGESIZE, Mesh -> meshVar, MaxRecursion -> 0 (* ,ColorFunction\[Rule]colorFunc,ColorFunctionScaling \[Rule] colorFuncScaling *)}, plotOpts = {pltOptsRaw, AxesLabel -> {xName, yName, zName}, ImageSize -> BDIMAGESIZE, Mesh -> meshVar, MaxRecursion -> 0 (*,ColorFunction\[Rule]colorFunc,ColorFunctionScaling \[Rule] colorFuncScaling *) }
            ];
            Print[RevolutionPlot3D[N[f[xVar, yVar]], {xVar, xStart, xEnd}, {yVar, yStart, yEnd}, Evaluate[plotOpts]]];
            ,
            If[usePltLbl === True,
              plotOpts = {pltOptsRaw, AxesLabel -> {xName, yName, ""}, PlotLabel -> zName, LabelStyle -> BDPLTTEXTOPTS, ImageSize -> BDIMAGESIZE, Mesh -> meshVar, MaxRecursion -> 0(* ,ColorFunction\[Rule]colorFunc,ColorFunctionScaling \[Rule] colorFuncScaling *)}, plotOpts = {pltOptsRaw, AxesLabel -> {xName, yName, zName}, LabelStyle -> BDPLTTEXTOPTS, ImageSize -> BDIMAGESIZE, Mesh -> meshVar, MaxRecursion -> 0 (*,ColorFunction\[Rule]colorFunc,ColorFunctionScaling \[Rule] colorFuncScaling *)}
            ];

            If[$VersionNumber >= 10, plotOpts = Join[plotOpts, {PlotTheme -> {"Classic", "ClassicLights"}}]];

            (* Print["CalcPlot3D::plotOpts = ", plotOpts]; *)
            Print[Plot3D[MyApproximateFunc[xVar, yVar], {xVar, xStart, xEnd}, {yVar, yStart, yEnd}, Evaluate[plotOpts]]];
          ];

          (*
    Print["CalcGridArr = ", CalcGridArr // MatrixForm];
    Print["CalcPlot3D::f::retval = ",fTmp];
    *)

          Print[strSeparator];
          Print["    "];
          Print["    "];
        (*
    data=Table[ChopValue[CalcGridArr[[iii,jjj]][[1,k]],clcOpts],{iii,1,pltPointsX},{jjj,1,pltPointsY}];
    Print[ListPlot3D[data]];
    *)
          , {k, len}
        ];
        , Print["!!! Second variable not found. Cannot perform Plot3D..."];
      ];
    ];

(* ============================================== *)
CalcPlot3DFunc[Calc_, Func_, FuncName_, SwapXY_, pltOptsRaw___] :=
    Module[{f, xVar, yVar, xStart, xEnd, yStart, yEnd, plotOpts, Media, varLst, vLen, xName, yName, zName, xVarCnt, yVarCnt, var, xAssigned, yAssigned, varNew, opts, usePltLbl},
      Media = CalcMedia[Calc];
      varLst = CalcVarList[Calc];
      vLen = Length[varLst];
      xAssigned = False;yAssigned = False;varNew = varLst;
      opts = {PrintTimeEstimate -> False, PrintCalculationProgress -> False, PrintCalculationDetails -> False, CalcOptions[Calc]};
      xName = "x";yName = "y";zName = FuncName;xVarCnt = 1;yVarCnt = 2;xStart = 0;xEnd = 1;
      yStart = 0;yEnd = 1;
      usePltLbl = UsePlotLabel3D /. Flatten[{pltOptsRaw}] /. Options[BerremanDirect] /. {UsePlotLabel3D -> True};
      (*Print["CalcPlot3DFunc"];Print["varLst = ",varLst];*)

      Do[var = varLst[[vCnt]];
      If[var[[1]] + var[[3]] <= var[[2]], If[SwapXY === True, If[yAssigned === False, yAssigned = True;yVarCnt = vCnt;
      yStart = var[[1]];yEnd = var[[2]];yName = var[[4]];, If[xAssigned === False, xAssigned = True;xVarCnt = vCnt;
      xStart = var[[1]];xEnd = var[[2]];xName = var[[4]];];];, If[xAssigned === False, xAssigned = True;xVarCnt = vCnt;
      xStart = var[[1]];xEnd = var[[2]];xName = var[[4]];, If[yAssigned === False, yAssigned = True;yVarCnt = vCnt;
      yStart = var[[1]];yEnd = var[[2]];yName = var[[4]];];];];];
      varNew[[vCnt, 2]] = varNew[[vCnt, 1]];, {vCnt, vLen}
      ];

      (*Print["=============="];Print["xName = ",xName];*)

      f[xVar_, yVar_] := Module[{varN, clc, outpt, retval},
        varN = varNew;
        varN[[xVarCnt, 1]] = xVar;
        varN[[xVarCnt, 2]] = xVar;varN[[yVarCnt, 1]] = yVar;
        varN[[yVarCnt, 2]] = yVar;clc = CalcNew[Media, varN, {Func}, "", opts];
        CalcPerform[clc];outpt = CalcGetOutput[clc];retval = outpt[[1, 1]];
        Return[retval];
      ];

      If[usePltLbl === True, plotOpts = {pltOptsRaw, AxesLabel -> {xName, yName, ""}, PlotLabel -> zName, PlotRange -> All, LabelStyle -> BDPLTTEXTOPTS, ImageSize -> BDIMAGESIZE}, plotOpts = {pltOptsRaw, AxesLabel -> {xName, yName, zName}, PlotRange -> All, LabelStyle -> BDPLTTEXTOPTS, ImageSize -> BDIMAGESIZE}];

      If[$VersionNumber >= 10, plotOpts = Join[plotOpts, {PlotTheme -> {"Classic", "ClassicLights"}}]];

      Print[Plot3D[f[xVar, yVar], {xVar, xStart, xEnd}, {yVar, yStart, yEnd}, Evaluate[plotOpts]]];
    ];
(* ============================================== *)
(* TODO::CalcFuncPlot does not work *)
CalcFuncPlot[CalcFunc_, xVarLst : {_, _, _}, pltOptsRaw___] :=
    Module[{FuncList, FuncNameList, len, Calc},
    (*Print["CalcFuncPlot"];*)Calc = CalcFunc[xVarLst[[1]]];
    FuncList = CalcFuncList[Calc];
    FuncNameList = CalcFuncNameList[Calc];
    len = Min[Length[FuncList], Length[FuncNameList]];
    Do[CalcFuncPlotFunc[CalcFunc, xVarLst, FuncList[[i]], FuncNameList[[i]], pltOptsRaw], {i, len}];
    ];
(* ============================================== *)
(* TODO::CalcFuncPlotFunc does not work *)
CalcFuncPlotFunc[CalcFunc_, xVarLst : {_, _, _}, Func_, FuncName_, pltOptsRaw___] :=
    Module[{f, Calc, xVar, xStart, xEnd, plotOpts, xName, yName, var},
    (*Ok,CalcFunc must return Calc for each x and y*)

      xStart = xVarLst[[1]];
      xEnd = xVarLst[[2]];
      xName = xVarLst[[3]];
      yName = FuncName;

      (*Print["CalcFuncPlotFunc"];Print["xVarLst = ",xVarLst];*)

      f[xVar_] := Module[{clc, outpt, retval, Calc1, Media, varLst, vLen, varNew, opts}, Calc1 = Apply[CalcFunc, {xVar}];
      Media = CalcMedia[Calc1];varLst = CalcVarList[Calc1];vLen = Length[varLst];
      varNew = varLst;
      opts = {PrintTimeEstimate -> False, PrintCalculationProgress -> False, PrintCalculationDetails -> False, CalcOptions[Calc1]};
      Do[varNew[[vCnt, 2]] = varNew[[vCnt, 1]];, {vCnt, vLen}];
      clc = CalcNew[Media, varNew, {Func}, "", opts];CalcPerform[clc];
      outpt = CalcGetOutput[clc];retval = outpt[[1, 1]];
      Return[retval];
      ];

      (*Print["f[xStart] = ",f[xStart]];*)

      plotOpts = {pltOptsRaw, FrameLabel -> {xName, yName}, PlotRange -> All, LabelStyle -> BDPLTTEXTOPTS, ImageSize -> BDIMAGESIZE, Frame -> True, GridLines -> Automatic};
      Print[Plot[f[xVar], {xVar, xStart, xEnd}, Evaluate[plotOpts]]];
    ];
(* ============================================== *)
(* TODO::CalcFuncPlot3D does not work *)
CalcFuncPlot3D[CalcFunc_, xVarLst : {_, _, _}, yVarLst : {_, _, _}, pltOptsRaw___] :=
    Module[{FuncList, FuncNameList, len, Calc},
    (*Print["CalcFuncPlot3D"];*)

      Calc = CalcFunc[xVarLst[[1]], yVarLst[[1]]];
      FuncList = CalcFuncList[Calc];
      FuncNameList = CalcFuncNameList[Calc];
      len = Min[Length[FuncList], Length[FuncNameList]];
      Do[CalcFuncPlot3DFunc[CalcFunc, xVarLst, yVarLst, FuncList[[i]], FuncNameList[[i]], pltOptsRaw], {i, len}];
    ];
(* ============================================== *)
(* TODO::CalcFuncPlot3DFunc does not work *)
CalcFuncPlot3DFunc[CalcFunc_, xVarLst : {_, _, _}, yVarLst : {_, _, _}, Func_, FuncName_, pltOptsRaw___] :=
    Module[{f, Calc, xVar, yVar, xStart, xEnd, yStart, yEnd, plotOpts, xName, yName, zName, var, usePltLbl},
    (*Ok,CalcFunc must return Calc for each x and y*)

      usePltLbl = UsePlotLabel3D /. Flatten[{pltOptsRaw}] /. Options[BerremanDirect] /. {UsePlotLabel3D -> True};
      xStart = xVarLst[[1]];
      xEnd = xVarLst[[2]];
      yStart = yVarLst[[1]];
      yEnd = yVarLst[[2]];
      xName = xVarLst[[3]];
      yName = yVarLst[[3]];
      zName = FuncName;

      (*Print["CalcFuncPlot3DFunc"];Print["varLst = ",varLst];*)

      f[xVar_, yVar_] :=
          Module[{clc, outpt, retval, Calc1, Media, varLst, vLen, varNew, opts},
            Calc1 = Apply[CalcFunc, {xVar, yVar}];
            Media = CalcMedia[Calc1];varLst = CalcVarList[Calc1];vLen = Length[varLst];
            varNew = varLst;
            opts = {PrintTimeEstimate -> False, PrintCalculationProgress -> False, PrintCalculationDetails -> False, CalcOptions[Calc1]};
            Do[varNew[[vCnt, 2]] = varNew[[vCnt, 1]];, {vCnt, vLen}];
            clc = CalcNew[Media, varNew, {Func}, "", opts];CalcPerform[clc];
            outpt = CalcGetOutput[clc];
            (* TODO - replace outpt[[1,1]] by a function call *)
            retval = outpt[[1, 1]];
            Return[retval];
          ];

      (*Print["f[xStart,yStart] = ",f[xStart,yStart]];*)

      If[usePltLbl === True, plotOpts = {pltOptsRaw, AxesLabel -> {xName, yName, ""}, PlotLabel -> zName, PlotRange -> All, LabelStyle -> BDPLTTEXTOPTS, ImageSize -> BDIMAGESIZE}, plotOpts = {pltOptsRaw, AxesLabel -> {xName, yName, zName}, PlotRange -> All, LabelStyle -> BDPLTTEXTOPTS, ImageSize -> BDIMAGESIZE}];
      Print[Plot3D[f[xVar, yVar], {xVar, xStart, xEnd}, {yVar, yStart, yEnd}, Evaluate[plotOpts]]];
    ];
(* ============================================== *)
CalcGetInput[Calc_] := Module[{}, Return[Calc[[3]][[1]]]];
CalcGetOutput[Calc_] := Module[{}, Return[Calc[[3]][[2]]]];
CalcGetFuncNames[Calc_] := Module[{}, Return[Calc[[1]][[4]]];];
(* ============================================== *)
CalcGetVarNames[Calc_] :=
    Module[{VarList, VarNames, len},
      VarList = Calc[[1]][[2]];len = Length[VarList];
      VarNames = Table[VarList[[i]][[4]], {i, len}];
      Return[VarNames];
    ];
(* ============================================== *)
CalcGetNumberOfResults[Calc_] := Module[{}, Return[Length[Calc[[3]][[2]]]]];
CalcMedia[Calc_] := Module[{}, Return[Calc[[1]][[1]]];];
CalcVarList[Calc_] := Module[{}, Return[Calc[[1, 2]]]];
CalcFuncList[Calc_] := Module[{}, Return[Calc[[1, 3]]]];
CalcFuncNameList[Calc_] := Module[{}, Return[Calc[[1, 4]]]];
CalcDescription[Calc_] := Module[{}, Return[Calc[[1]][[5]]];];
CalcFuncDescList[Calc_] := Module[{}, Return[Calc[[1, 6]]]];
CalcOptions[Calc_] := Module[{}, Return[Calc[[2]]]];
CalcSave[Calc_, fName_ : "", OverWrite_ : True] := Module[{}, Return[OutputFunc[Calc, fName, OverWrite]];];
DateTimeToNumber[cdt : {_, _, _, _, _, _}] := Module[{sttime}, sttime = cdt[[4]] * 3600 + cdt[[5]] * 60 + cdt[[6]];Return[sttime];];
(* ============================================== *)
DateTimeToString[cdt : {_, _, _, _, _, _}] := Module[{sttime},
  sttime = cdt[[4]] * 3600 + cdt[[5]] * 60 + cdt[[6]];
  Return[ConvertToTime[sttime]];
];
(* ============================================== *)
CalcPrintTimeEstimate[Calc_] :=
    Module[{opts, prtts, maxcnt, tstart, tlen, sol, Media, inclght, estTime, cdt, sttime, mult, multpoints, napt, utll, avgtyp},
    (* Print["CalcPrintTimeEstimate::start"]; *)
    (* Print["Calc = ", Calc]; *)
      opts = Calc[[2]];
      (* Print["opts = ", opts]; *)
      prtts = PrintTimeEstimate /. opts /. Options[BerremanDirect];
      (* Print["prtts = ", prtts]; *)
      If[prtts === True,
        napt = 1;
        utll = UseThickLastLayer /. Calc[[2]] /. Options[BerremanDirect];
        If[utll === True, avgtyp = AveragingType /. Calc[[2]] /. Options[BerremanDirect];
        napt = Max[(NoOfAveragingPoints /. opts /. Options[BerremanDirect]), 2];
        If[avgtyp === BDAVGTYPESERIES, napt *= 2];
        ];
        mult = 1.4;
        multpoints = 10^(-5);
        maxcnt = GetInptLen[Calc];
        (* Print["maxcnt = GetInptLen[Calc] = ", maxcnt]; *)
        Media = Calc[[1]][[1]];
        (* Print["Media = ", Media]; *)
        (*Ok we DO NOT care about Incident Light as this is just for estimating total time*)
        inclght = IncidentLightNew[600 / 10^9, 0, 0, 1, 1];
        tstart = TimeUsed[];
        sol = SolutionNew[Media, inclght];
        tlen = TimeUsed[] - tstart;
        estTime = tlen * maxcnt * (mult + multpoints * maxcnt) * napt;
        sttime = DateTimeToNumber[Date[]];

        Print["Start time: ", ConvertToTime[sttime]];
        Print["Total number of points to calculate: ", maxcnt];
        Print["Estimated total calculation time: ", ConvertToTime[estTime]];
        Print["Estimated end time: ", ConvertToTime[Mod[sttime + estTime, 24 * 3600]]];
        Print[strSeparator];];
    (* Print["CalcPrintTimeEstimate::end"]; *)
    ];
(* ============================================== *)
CalcPrintRunTime[Calc_] :=
    Module[{tt, ttsec},
      ttsec = Calc[[3, 3]];
      tt = If[ttsec >= 60, ConvertToTime[ttsec], ttsec, ttsec];
      Print["End time: ", DateTimeToString[Calc[[3, 5]]]];
      Print["Time Used: ", tt];
      Print["Time per point: ", ttsec / CalcGetNumberOfResults[Calc]];
      Print[strSeparator];
    ];
(* ============================================== *)
AnalyzerInfoGetAngle[anInf : {_, _, _}] := Module[{}, Return[anInf[[1]]]];
AnalyzerInfoGetParallelAmplitude[anInf : {_, _, _}] := Module[{}, Return[anInf[[2]]]];
AnalyzerInfoGetCrossedAmplitude[anInf : {_, _, _}] := Module[{}, Return[anInf[[3]]]];
(* ============================================== *)
SolutionGetTransmittedAnalyzerInfo[sol_] :=
    Module[{retval = 0, opts, inclght, beta, analyzerAngle, analyzerParAmpl, analyzerCrossAmpl, anglAbs},
      opts = GetSolOptions[sol];
      inclght = GetSolIncidentLightInfo[sol];
      beta = IncidentLightBeta[inclght];
      analyzerAngle = TransmittedAnalyzerAngle /. opts /. Options[BerremanDirect];
      analyzerParAmpl = TransmittedAnalyzerParallelAmplitude /. opts /. Options[BerremanDirect];
      analyzerCrossAmpl = TransmittedAnalyzerCrossedAmplitude /. opts /. Options[BerremanDirect];
      anglAbs = AnalyzerAngleAbsoluteValue /. opts /. Options[BerremanDirect];
      If[anglAbs =!= True, analyzerAngle += beta];
      retval = {analyzerAngle, analyzerParAmpl, analyzerCrossAmpl};
      Return[retval];
    ];
(* ============================================== *)
SolutionGetReflectedAnalyzerInfo[sol_] :=
    Module[{retval = 0, opts, inclght, beta, analyzerAngle, analyzerParAmpl, analyzerCrossAmpl, anglAbs},
      opts = GetSolOptions[sol];
      inclght = GetSolIncidentLightInfo[sol];
      beta = IncidentLightBeta[inclght];
      analyzerAngle = ReflectedAnalyzerAngle /. opts /. Options[BerremanDirect];
      analyzerParAmpl = ReflectedAnalyzerParallelAmplitude /. opts /. Options[BerremanDirect];
      analyzerCrossAmpl = ReflectedAnalyzerCrossedAmplitude /. opts /. Options[BerremanDirect];
      anglAbs = AnalyzerAngleAbsoluteValue /. opts /. Options[BerremanDirect];
      If[anglAbs =!= True, analyzerAngle += beta];
      retval = {analyzerAngle, analyzerParAmpl, analyzerCrossAmpl};
      Return[retval];
    ];
(* ============================================== *)
CalcCollectionNew[BaseDir_, BaseFileName_ : "", Description_ : ""] :=
    Module[{retval},
      retval = {{ToString[BaseDir], ToString[BaseFileName], ToString[Description]}, {}};
      Return[retval];
    ];
(* ============================================== *)
CalcCollectionLength[coll_] :=
    Module[{},
      Return[Length[coll[[2]]]];
    ];
(* ============================================== *)
GetFullName[coll_, fName_] :=
    Module[{fullName, len},
      len = CalcCollectionLength[coll] + 1;
      fullName = coll[[1]][[1]] <> If[fName === "", coll[[1]][[2]] <> "_" <> ToString[len], ToString[fName]] <> ".CSV";
      Return[fullName];
    ];
(* ============================================== *)
Attributes[CalcCollectionAddCalc] = {HoldFirst};
CalcCollectionAddCalc[coll_, Calc_, fName_ : ""] :=
    Module[{fullName},
      fullName = GetFullName[coll, fName];
      coll[[2]] = Join[coll[[2]], {{Calc, fullName}}];
      Return[coll];
    ];
(* ============================================== *)
CalcCollectionItem[coll_, idx_Integer] :=
    Module[{},
      Return[coll[[2]][[idx]][[1]]]
    ];
(* ============================================== *)
Attributes[CalcCollectionPerform] = {HoldFirst};
CalcCollectionPerform[coll_] :=
    Module[{retval, len, Calc},
    (* Print["CalcCollectionPerform::start"]; *)
      len = CalcCollectionLength[coll];
      Calc = coll[[2, 1, 1]];

      (* Print["len = ", len]; Print["coll = ", coll]; Print["coll = ", coll // MatrixForm]; *)

      Do[
      (* Print["idx = ", idx]; Print["coll[[2]] = ", coll[[2]]]; Print["coll[[2,idx]] = ", coll[[2,idx]]]; Print["coll[[2,idx,1]] = ", coll[[2,idx,1]]]; *)
        Calc = coll[[2, idx, 1]];
        coll[[2, idx, 1]] = CalcPerform[Calc], {idx, len}
      ];

      Return[retval];
    ];
(* ============================================== *)
CalcCollectionSave[coll_, DoPrintOut_ : False] :=
    Module[{retval, len},
      retval = True;
      len = CalcCollectionLength[coll];
      If[DoPrintOut === True, Print[strSeparator];
      Print["Printing Collection..."];
      Do[CalcSave[coll[[2, idx, 1]]], {idx, len}];Print["Done."];, Print[strSeparator];Print["Saving Collection..."];
      Do[CalcSave[coll[[2, idx, 1]], coll[[2, idx, 2]], True], {idx, len}];
      Print["Done."];
      ];
      Return[retval];
    ];
(* ============================================== *)
CalcCollectionPlot[coll_, pltOptsRaw___] :=
    Module[{len, Calc},
      len = CalcCollectionLength[coll];
      Do[CalcPlot[coll[[2, idx, 1]], pltOptsRaw], {idx, len}];
    ];
(* ============================================== *)
CalcCollectionPlot3D[coll_, SwapXY_, pltOptsRaw___] :=
    Module[{len, Calc},
      len = CalcCollectionLength[coll];
      Do[CalcPlot3D[coll[[2, idx, 1]], SwapXY, pltOptsRaw], {idx, len}];
    ];

(* ============================================== *)
CalcCollCombine[coll_, funcNo_, hVarInputNo_ : 0, vVarInputNo_ : 0, OutputHeader_ : False] :=
    Module[{retval, collLen, colRowLen, collOutArr, collRowArr},
    (*coll[[2]]-this is actuall collection of calculations*)(*coll[[2,1,1]]-this is first calculation in collection*)(*coll[[2,1,1,3,2]]-this is output from first calculation in collection*)

      collLen = Length[coll[[2]]];
      colRowLen = Length[coll[[2, 1, 1, 3, 2]]];
      collOutArr = {};
      collRowArr = {};

      Do[collRowArr = {};
      Do[collRowArr = Append[collRowArr, coll[[2, collIter, 1, 3, 2, rowIter, funcNo]]],
        {collIter, collLen}];
      collOutArr = Append[collOutArr, collRowArr],
        {rowIter, colRowLen}
      ];

      Return[collOutArr];
    (*Print[collOutArr//MatrixForm];
    Export["D:\\KKK\\Math\\Calc\\EpsAll.CSV",collOutArr,"CSV"];*)(*Show[SurfaceGraphics[collOutArr,MeshRange\[Rule]{{nStart,nEnd},{thk1[[1]],thk1[[2]]}}],Axes\[Rule]True];*)
    ];
(* ============================================== *)
CalcCollCombinePlot3D[coll_, funcNo_, hVarInputNo_ : 0, vVarInputNo_ : 0, OutputHeader_ : False, pltOptsRaw___] :=
    Module[{collOutArr, xStart = 0, xEnd = 1, yStart = 0, yEnd = 1, xName = "x", yName = "y", zName = "z", plotOpts},
      collOutArr = CalcCollCombine[coll, funcNo, hVarInputNo, vVarInputNo, OutputHeader];
      plotOpts = {pltOptsRaw, MeshRange -> {{xStart, xEnd}, {yStart, yEnd}}, Axes -> True, AxesLabel -> {xName, yName, zName}, PlotRange -> All, LabelStyle -> {FontFamily -> "Courier", FontSize -> 18, FontWeight -> "Bold"}, ImageSize -> BDIMAGESIZE};
      Print[Show[SurfaceGraphics[collOutArr, Evaluate[plotOpts]]]];
    ];
(* ============================================== *)
SolAvgLambdaBoundaries[lambda_, fita_, n1_, n2_, LayerThickness_, opts___] :=
    Module[{retval, stp, lmb, dltlmb, lmbstart, lmbend, len, opt, utll, napt, avgper, avgtyp, psi, NumberOfWavelengths},
    (*UseThickLastLayer\[Rule]False,NoOfAveragingPoints\[Rule]10,AveragingPeriods\[Rule]1,AveragingType\[Rule]0*)

      utll = UseThickLastLayer /. opts /. Options[BerremanDirect];
      napt = Max[(NoOfAveragingPoints /. opts /. Options[BerremanDirect]), 2];
      avgper = Max[(AveragingPeriods /. opts /. Options[BerremanDirect]), 0.5];
      avgtyp = AveragingType /. opts /. Options[BerremanDirect];
      psi = PsiAngle[fita, n1, n2];
      NumberOfWavelengths = Floor[2 * LayerThickness * n2 / lambda / Cos[psi]];
      lmb = 2 * LayerThickness * n2 / NumberOfWavelengths / Cos[psi];
      dltlmb = lmb^2 / (n2 * LayerThickness / Cos[psi] - lmb) / 2;
      (*BDAVGTYPESERIES=100;*)

      Which[avgtyp === BDAVGTYPESUMOFTWOPOINTS, (lmbstart = lmb;lmbend = lmb + dltlmb / 2;
      stp = dltlmb / 2;len = 2;), avgtyp === BDAVGTYPEUSEAVGPERIODS, (lmbstart = lmb - dltlmb * avgper / 2 + dltlmb / 4;
      lmbend = lmb + dltlmb * avgper / 2 + dltlmb / 4;stp = dltlmb / (napt - 1);
      len = napt), avgtyp === BDAVGTYPEUSEAVGPERIODSEXACTLAMBDA, (lmbstart = lambda - dltlmb * avgper / 2;lmbend = lambda + dltlmb * avgper / 2;
      stp = dltlmb / (napt - 1);len = napt), True, (Print["Invalid averaging type specified in SolAvgLambdaBoundaries"];Abort[];)
      ];

      retval = {lmbstart, lmbend, stp, len};
      Return[retval];
    ];
(* ============================================== *)
SolutionAverageNew[Media_, IncidentLight_, opts___] :=
    Module[{retval, lmbbound, lmb, len, lmbstart, stp, optprc, lambda, fita, beta, gamm, n1, n2, LayerThickness, Film, flmLen, inclgth, avgtyp, pdi, pdil},
      optprc = Flatten[{opts}];
      Film = MediaFilm[Media];
      flmLen = FilmLength[Film];
      lambda = IncidentLightLambda[IncidentLight];
      fita = IncidentLightFita[IncidentLight];
      beta = IncidentLightBeta[IncidentLight];
      n1 = MediaUpperRefractionIndex[Media];
      avgtyp = AveragingType /. optprc /. Options[BerremanDirect];

      pdi = PrintCommonDebugInfo /. opts /. Options[BerremanCommon];
      pdil = PrintCommonDebugInfoLevel /. opts /. Options[BerremanCommon];

      If[pdi == True,
        Print["   "];
        Print["SolutionAverageNew::start ================================================="];
        Print["SolutionAverageNew::IncidentLight = ", Chop[N[IncidentLight]]];
        Print["SolutionAverageNew::Media = ", Chop[N[Media]]];
      ];

      Which[avgtyp === BDAVGTYPESERIES,
        retval = SolutionSumNew[Media, IncidentLight, opts],
        True,
        (
          Print["Function FilmLayerEpsilon changed - update the code !!!"];
          Abort[];
          n2 = MatrixPower[FilmLayerEpsilon[FilmItem[Film, flmLen]], 0.5][[1, 1]];
          LayerThickness = FilmLayerThickness[FilmItem[Film, flmLen]];
          lmbbound = SolAvgLambdaBoundaries[lambda, fita, n1, n2, LayerThickness, optprc];len = lmbbound[[4]];lmbstart = lmbbound[[1]];
          stp = lmbbound[[3]];
          retval = Table[(inclgth = IncidentLightNew[lmbstart + (i - 1) * stp, fita, beta, MediaUpperRefractionIndex[Media], -10^10];
          SolutionNew[Media, inclgth, opts]), {i, len}];
        )
      ];

      Return[retval];
    ];
(* ============================================== *)
SSMakeFirstStep[Media_, IncidentLight_, opts___] :=
    Module[{retval, sol, solToAvg, inclNext, lambda, fita, n1, n2, ehZero, fita2, gamm, h2, PPPm, beta, pdi, pdil, utll, transmittedLight},

      utll = UseThickLastLayer /. opts /. Options[BerremanDirect];
      pdi = PrintCommonDebugInfo /. opts /. Options[BerremanCommon];
      pdil = PrintCommonDebugInfoLevel /. opts /. Options[BerremanCommon];

      If[pdi == True,
        Print["   "];
        Print["SSMakeFirstStep::start ================================================="];
        Print["SSMakeFirstStep::IncidentLight = ", Chop[N[IncidentLight]]];
      ];

      lambda = IncidentLightLambda[IncidentLight];
      fita = IncidentLightFita[IncidentLight];
      n1 = MediaUpperRefractionIndex[Media];
      n2 = MediaLowerRefractionIndex[Media];
      fita2 = ArcSin[n1 * Sin[fita] / n2];
      ehZero = {0, 0, 0, 0, 0, 0, True};
      beta = IncidentLightBeta[IncidentLight];

      If[pdi == True && pdil >= PCDILEVELMEDIUM,
        Print["SSMakeFirstStep::IncidentLight = ", IncidentLight];
        Print["SSMakeFirstStep::lambda = ", Chop[N[lambda]]];
        Print["SSMakeFirstStep::fita = ", Chop[N[fita]]];
        Print["SSMakeFirstStep::n1 = ", Chop[N[n1]]];
        Print["SSMakeFirstStep::n2 = ", Chop[N[n2]]];
        Print["SSMakeFirstStep::fita2 = ", Chop[N[fita2]]];
        Print["SSMakeFirstStep::beta = ", Chop[N[beta]]];
      ];

      sol = SolutionNew[Media, IncidentLight, OutputPPPMultiplier -> 1, opts];

      If[!utll,
        (
          solToAvg = SolutionCombine[GetSolIncidentLight[sol], GetSolReflectedLight[sol], ehZero, GetSolPPP[sol], GetSolDelta[sol], Media, IncidentLight, opts, GetSolM1[sol][[1]], GetSolM1[sol][[2]], GetSolCoeff[sol], GetSolFreeTerm[sol], GetSolEGSys1[sol], GetSolEGSys2[sol]];
        ),
        (
          solToAvg = SolutionCombine[GetSolIncidentLight[sol], GetSolReflectedLight[sol], ehZero, GetSolPPP[sol], GetSolDelta[sol], Media, IncidentLight, opts, GetSolM1[sol][[1]], GetSolM1[sol][[2]], GetSolCoeff[sol], GetSolFreeTerm[sol], GetSolEGSys1[sol], GetSolEGSys2[sol], GetSolBeta0Sol[sol], GetSolBeta90Sol[sol]];
        )
      ];

      (* retval={EHIFull,EHRFull,EHTFull,PPPv,delta,Media,IncidentLight,opts,MMM1[[1]],MMM2[[1]],coeffTbl,freeTbl,egs1,egs2}; *)

      (*Print["SSMakeFirstStep:solToAvg = ",solToAvg];*)

      transmittedLight = GetSolTransmittedLight[sol];
      inclNext = IncidentLightNew[lambda, fita2, beta, transmittedLight];
      retval = {solToAvg, inclNext};

      (* PCDILEVELALL; PCDILEVELDETAILED; PCDILEVELMEDIUM; PCDILEVELSHORT; *)
      If[pdi == True && pdil >= PCDILEVELMEDIUM,
        Print["SSMakeFirstStep::transmittedLight = ", Chop[N[transmittedLight]]];
        Print["SSMakeFirstStep::inclNext = ", Chop[N[inclNext]]];
        Print["SSMakeFirstStep::retval = ", Chop[N[retval]]];
      ];

      If[pdi == True,
        Print["SSMakeFirstStep::completed ================================================="];
        Print["   "];
      ];

      Return[retval];
    ];
(* ============================================== *)
SSMakeDownStep[Media_, IncidentLight_, opts___] :=
    Module[{retval, n1, n2, lambda, fita, sol, inclght, MediaBound, solCmb, ehZero, nOut, gamm, ehZeroRefl, h2, PPPm, Film, beta, solToAvg, inclNext, pdi, pdil, utll},
      utll = UseThickLastLayer /. opts /. Options[BerremanDirect];
      pdi = PrintCommonDebugInfo /. opts /. Options[BerremanCommon];
      pdil = PrintCommonDebugInfoLevel /. opts /. Options[BerremanCommon];

      If[pdi == True,
        Print["   "];
        Print["SSMakeDownStep::start ================================================="];
        Print["SSMakeDownStep::IncidentLight = ", Chop[N[IncidentLight]]];
      ];

      lambda = IncidentLightLambda[IncidentLight];
      fita = IncidentLightFita[IncidentLight];
      n1 = MediaUpperRefractionIndex[Media];
      beta = IncidentLightBeta[IncidentLight];
      gamm = MediaGamma[Media];
      nOut = MediaOutRefractionIndex[Media];
      n2 = MediaLowerRefractionIndex[Media];
      ehZeroRefl = {0, 0, 0, 0, 0, 0, False};
      ehZero = {0, 0, 0, 0, 0, 0, True};
      h2 = MediaSubstrateThickness[Media];
      Film = FilmNew[];

      (* TODO - This is what was here prior to version 6.03. That makes no sense. *)
      (* MediaBound = MediaNew[n2, nOut, gamm, Film, "", nOut, h2, EpsilonFromN[Re[nOut]], muMstandard, roMstandard, MediaLowerEpsilon[Media], MediaLowerMu[Media], MediaLowerRo[Media]]; *)
      MediaBound = Media;

      (* PCDILEVELALL; PCDILEVELDETAILED; PCDILEVELMEDIUM; PCDILEVELSHORT; *)
      If[pdi == True && pdil >= PCDILEVELMEDIUM,
        Print["SSMakeDownStep::MediaBound = ", MediaBound];
      ];

      (* Print["SSMakeDownStep::MediaBound = ", MediaBound]; *)

      sol = SolutionNew[MediaBound, IncidentLight, OutputPPPMultiplier -> -1, opts];
      inclght = GetSolTransmittedLight[sol];

      (* PCDILEVELALL; PCDILEVELDETAILED; PCDILEVELMEDIUM; PCDILEVELSHORT; *)
      If[pdi == True && pdil >= PCDILEVELMEDIUM,
        Print["SSMakeDownStep::inclght = ", Chop[N[inclght]]];
      ];

      If[!utll,
        (
          solToAvg = SolutionCombine[ehZero, ehZeroRefl, GetSolTransmittedLight[sol], GetSolPPP[sol], GetSolDelta[sol], MediaBound, IncidentLight, opts, GetSolM1[sol][[1]], GetSolM1[sol][[2]], GetSolCoeff[sol], GetSolFreeTerm[sol], GetSolEGSys1[sol], GetSolEGSys2[sol]];
        ),
        (
          solToAvg = SolutionCombine[ehZero, ehZeroRefl, GetSolTransmittedLight[sol], GetSolPPP[sol], GetSolDelta[sol], MediaBound, IncidentLight, opts, GetSolM1[sol][[1]], GetSolM1[sol][[2]], GetSolCoeff[sol], GetSolFreeTerm[sol], GetSolEGSys1[sol], GetSolEGSys2[sol], GetSolBeta0Sol[sol], GetSolBeta90Sol[sol]];
        )
      ];

      inclNext = IncidentLightNew[lambda, fita, beta, GetSolReflectedLight[sol]];

      retval = {solToAvg, inclNext};

      (* PCDILEVELALL; PCDILEVELDETAILED; PCDILEVELMEDIUM; PCDILEVELSHORT; *)
      If[pdi == True && pdil >= PCDILEVELMEDIUM,
        Print["SSMakeDownStep::inclNext = ", Chop[N[inclNext]]];
        Print["SSMakeDownStep::retval = ", Chop[N[retval]]];
      ];

      (*Print["SSMakeDownStep:solToAvg = ",solToAvg];*)(*Print["SSMakeDownStep:inclNext = ",inclNext];*)

      If[pdi == True,
        Print["SSMakeDownStep::completed ================================================="];
        Print["   "];
      ];

      Return[retval];
    ];
(* ============================================== *)
SSMakeUpStep[Media_, IncidentLight_, opts___] :=
    Module[{retval, inclFlp, MediaFlp, solFlp, ehZero, ehReflFlp, ehTrFlp, ehRefl, ehTr, lambda, fita, gamm, beta, solToAvg, inclNext, pdi, pdil, utll},
      utll = UseThickLastLayer /. opts /. Options[BerremanDirect];
      pdi = PrintCommonDebugInfo /. opts /. Options[BerremanCommon];
      pdil = PrintCommonDebugInfoLevel /. opts /. Options[BerremanCommon];

      If[pdi == True,
        Print["   "];
        Print["SSMakeUpStep::start ================================================="];
        Print["SSMakeUpStep::IncidentLight = ", Chop[N[IncidentLight]]];
      ];

      lambda = IncidentLightLambda[IncidentLight];
      fita = IncidentLightFita[IncidentLight];
      ehZero = {0, 0, 0, 0, 0, 0, True};
      beta = IncidentLightBeta[IncidentLight];
      inclFlp = IncidentLightFlip[IncidentLight];
      MediaFlp = MediaFlip[Media];
      solFlp = SolutionNew[MediaFlp, inclFlp, OutputPPPMultiplier -> -1, opts];
      ehReflFlp = GetSolReflectedLight[solFlp];
      ehTrFlp = GetSolTransmittedLight[solFlp];
      ehRefl = EHFlip[ehTrFlp];
      ehTr = EHFlip[ehReflFlp];
      (*Ok Media is not actually correct but we DO NOT CARE NOW *)

      If[!utll,
        (
          solToAvg = SolutionCombine[ehZero, ehRefl, ehZero, GetSolPPP[solFlp], GetSolDelta[solFlp], MediaFlp, inclFlp, opts, GetSolM1[solFlp][[1]], GetSolM1[solFlp][[2]], GetSolCoeff[solFlp], GetSolFreeTerm[solFlp], GetSolEGSys1[solFlp], GetSolEGSys2[solFlp]];
        ),
        (
          solToAvg = SolutionCombine[ehZero, ehRefl, ehZero, GetSolPPP[solFlp], GetSolDelta[solFlp], MediaFlp, inclFlp, opts, GetSolM1[solFlp][[1]], GetSolM1[solFlp][[2]], GetSolCoeff[solFlp], GetSolFreeTerm[solFlp], GetSolEGSys1[solFlp], GetSolEGSys2[solFlp], GetSolBeta0Sol[solFlp], GetSolBeta90Sol[solFlp]];
        )
      ];


      inclNext = IncidentLightNew[lambda, fita, beta, ehTr];
      (*Print["SSMakeUpStep:solToAvg = ",solToAvg];*)(*Print["SSMakeUpStep:inclNext = ",inclNext];*)

      retval = {solToAvg, inclNext};

      (* PCDILEVELALL; PCDILEVELDETAILED; PCDILEVELMEDIUM; PCDILEVELSHORT; *)
      If[pdi == True && pdil >= PCDILEVELMEDIUM,
        Print["SSMakeUpStep::inclNext = ", Chop[N[inclNext]]];
        Print["SSMakeUpStep::retval = ", Chop[N[retval]]];
      ];

      If[pdi == True,
        Print["SSMakeUpStep::completed ================================================="];
        Print["   "];
      ];

      Return[retval];
    ];
(* ============================================== *)
SolutionSumNew[Media_, IncidentLight_, optsRaw___] :=
    Module[{retval, opts, napt, Film, flmLen, lambda, fita, beta, gamm, n1, n2, nOut, solFirst, len, solStp, MediaBound, incLght, pdi, pdil, optsNew, ampl, ehField},
      opts = Flatten[{optsRaw}];

      pdi = PrintCommonDebugInfo /. opts /. Options[BerremanCommon];
      pdil = PrintCommonDebugInfoLevel /. opts /. Options[BerremanCommon];

      If[pdi == True,
        Print["   "];
        Print["SolutionSumNew::start ================================================="];
      ];

      ehField = IncidentLightEH[IncidentLight];
      ampl = PoyntingS[ehField, 3];

      If[pdi == True,
        Print["SolutionSumNew:: ehField = ", ehField, ", ampl = ", ampl];
      ];
      optsNew = Flatten[{IgnoreZeroIFullBase -> True, ZeroIFullBaseReplaceValue -> ampl, opts}];

      napt = Max[(NoOfAveragingPoints /. opts /. Options[BerremanDirect]), 2];
      solFirst = SSMakeFirstStep[Media, IncidentLight, optsNew];
      retval = {solFirst[[1]]};
      solStp = solFirst;

      Do[
        (
          incLght = solStp[[2]];
          solStp = SSMakeDownStep[Media, incLght, optsNew];
          AppendTo[retval, solStp[[1]]];

          incLght = solStp[[2]];
          solStp = SSMakeUpStep[Media, incLght, optsNew];
          AppendTo[retval, solStp[[1]]];
        ), {i, napt}
      ];

      (* PCDILEVELALL; PCDILEVELDETAILED; PCDILEVELMEDIUM; PCDILEVELSHORT; *)
      If[pdi == True && pdil >= PCDILEVELSHORT,
        Print["SolutionSumNew::retval = ", Evaluate[retval]];
        Print["==================================================="];
        Print["    "];
      ];

      Return[retval];
    ];
(* ============================================== *)
(*
FuncList=GetFuncList[Calc];
len=Length[FuncList];
IsAverageArr=Table[True,{len}];
utll=UseThickLastLayer/.opts/.Options[BerremanDirect];
avgtyp=AveragingType/.opts/.Options[BerremanDirect];
lstTbl=Table[List,{i,len}];
fnelhlp1=MapThread[Apply,{lstTbl,FuncList}];
fnelhlp2=MapThread[Head,{FuncList}];
ueFuncListHlp1=Table[If[Head[fnelhlp1[[i]]]===Head[{}],fnelhlp2[[i]],fnelhlp1[[i]]],{i,len}];
solArrHlp1=Table[If[Head[fnelhlp1[[i]]]===Head[{}],fnelhlp1[[i]],{}],{i,len}];

Print["AveragingFunc::FuncList = ", FuncList];
Print["AveragingFunc::IsAverageArr = ", IsAverageArr];
Print["AveragingFunc::lstTbl = ", lstTbl];


solArr=GetSolArr[Calc,idx,len];
solArrHlp2=Table[Join[solArrHlp1[[i]],solArr[[i]]],{i,len}];

If[pdi\[Equal] True,
Print["GetFuncArr::solArrHlp2 = ", solArrHlp2];
];

retval=MapThread[Apply,{ueFuncListHlp1,solArrHlp2}];
*)

AveragingFunc[f_, solavg_, IsAverage_ : True] :=
    Module[{len, retval = 0, navg, optf, naIdx = 1, fnelhlp1, fnelhlp2, ueFuncListHlp1, solArrHlp1, solArrHlp2, hlpArr, hlpArrI, hlpArrR, hlpArrT, pdi, pdil, opts, retval1, val, FullSol, fita, eField, hField, ehVec, kVec, ehAmpl, ehArg, afc, param},
      opts = GetSolOptions[solavg[[1, 1]]];
      pdi = PrintCommonDebugInfo /. opts /. Options[BerremanCommon];
      pdil = PrintCommonDebugInfoLevel /. opts /. Options[BerremanCommon];

      If[pdi == True,
        Print["   "];
        Print["AveragingFunc::start ================================================="];
      ];

      len = Length[solavg[[1]]];
      (*optf=Options[f];*)
      fnelhlp1 = Apply[List, f];
      fnelhlp2 = Head[f];
      ueFuncListHlp1 = If[Head[fnelhlp1] === Head[{}], fnelhlp2, fnelhlp1];
      solArrHlp1 = If[Head[fnelhlp1] === Head[{}], fnelhlp1, {}];

      optf = Options[ueFuncListHlp1];
      navg = NonAverageable /. optf /. Options[FieldAlgebra];
      naIdx = NonAverageableIndex /. optf /. Options[FieldAlgebra];
      afc = AveragingFuncCall /. optf /. Options[FieldAlgebra];

      If[pdi == True,
        Print["AveragingFunc::fnelhlp1 = ", fnelhlp1];
        Print["AveragingFunc::fnelhlp2 = ", fnelhlp2];
        Print["AveragingFunc::ueFuncListHlp1 = ", ueFuncListHlp1];
        Print["AveragingFunc::solArrHlp1 = ", solArrHlp1];
        Print["AveragingFunc::optf = ", optf];
        Print["AveragingFunc::navg = ", navg];
        Print["AveragingFunc::naIdx = ", naIdx];

        Print["AveragingFunc:: f = ", f, ", navg  = ", navg, ", naIdx = ", naIdx, ", IsAverage = ", IsAverage, ",  len = ", len];
        Print["AveragingFunc::solArrHlp1 = ", solArrHlp1];
      ];

      If[navg == True,
        (
          solArrHlp2 = Join[solArrHlp1, {solavg[[1, naIdx]]}];

          If[ToString[afc] == ToString[None],
            (
              retval = Apply[ueFuncListHlp1, solArrHlp2];
            ),
            (
            (* We neeed to call the underlying function afc and then call ueFuncListHlp1 using the result as a parameter *)
              param = Apply[afc, solArrHlp2];

              (* Print["AveragingFunc::Calling ", afc, "[", param, "]"]; *)

              retval = ueFuncListHlp1[param];

            (*
    Print["AveragingFunc::afc = ", afc, ", ueFuncListHlp1 = ",ueFuncListHlp1, ", param = ", param, ", retval = ", retval];
    *)
            )
          ];

          If[pdi == True,
            Print["AveragingFunc::navg = ", navg];
            Print["AveragingFunc::solArrHlp2 = ", solArrHlp2];
          ];
        ),
        (
          If[ToString[afc] == ToString[None],
            (
              retval = Sum[Apply[ueFuncListHlp1, Join[solArrHlp1, {solavg[[1, i]]}]], {i, len}];
            ),
            (
            (* We neeed to perform averaging using the underlying function afc and then call ueFuncListHlp1 using the result as a parameter *)
              param = Sum[Apply[afc, Join[solArrHlp1, {solavg[[1, i]]}]], {i, len}];

              (* Print["AveragingFunc::Calling ", afc, "[", param, "]"]; *)

              retval = ueFuncListHlp1[param];

            (*
    Print["AveragingFunc::afc = ", afc, ", ueFuncListHlp1 = ",ueFuncListHlp1, ", param = ", param, ", retval = ", retval];
    *)
            )
          ];

          (*
    (* ... for debugging purposes ...*)
    retval1=0;

    Do[
    (
    FullSol=solavg[[1,i]];
    fita=-IncidentLightFita[GetSolIncidentLightInfo[FullSol]];
    eField=Flatten[GetSolReflectedLightE[FullSol]];
    hField=Flatten[GetSolReflectedLightH[FullSol]];
    ehVec=Cross[hField,eField];
    ehAmpl=Sqrt[Abs[ehVec.Conjugate[ehVec]]];
    ehArg=Arg[ehVec[[3]]];
    kVec=If[ehAmpl>0,ehVec*Exp[-I*(ehArg+Pi)]/ehAmpl,{0,0,0}];

    val=Apply[ueFuncListHlp1,Join[solArrHlp1,{FullSol}]];

    Print["AveragingFunc::ueFuncListHlp1 = , ", ueFuncListHlp1, ", i = ", i, ", val = ", val, ", fita(�) = ", N[fita*(180/Pi)], ", eField = ", eField, ", hField = ", hField, ", ehVec = ", ehVec, ", ehArg = ", ehArg, ", kVec = ", kVec];

    retval1+=val;
    ),{i,1,len}
    ];

    Print["AveragingFunc::retval = ",retval, ", retval1 = ", retval1];
    Print[strSeparatorSmall];
    *)

          If[IsAverage == True, retval /= len];

          If[pdi == True,
            Print["AveragingFunc::navg = ", navg];
          ];
        )
      ];

      If[pdi == True,
      (*
    Print["AveragingFunc::hlpArr = ",hlpArr];
    Print["AveragingFunc::hlpArrI = ",hlpArrI // MatrixForm];
    Print["AveragingFunc::hlpArrR = ",hlpArrR // MatrixForm];
    Print["AveragingFunc::hlpArrT = ",hlpArrT // MatrixForm];
    *)

        Print["AveragingFunc::retval = ", retval];
        Print["AveragingFunc completed."];
        Print["..."];
        Print["..."];
      ];

      Return[retval];
    ];
(* ============================================== *)
MakeFileName[prefix_, Media_, thk_, opts___] :=
    Module[{retval, strFileName, Film, flmLen, eps, nMatr, kMatr}, strFileName = prefix;
    (* Print["MakeFileName::starting..."]; *)
    Film = MediaFilm[Media];
    flmLen = FilmLength[Film];
    eps = FilmLayerEpsilon[Film[[1]]];

    (*
    Print["MakeFileName::Film = ", Film];
    Print["MakeFileName::flmLen = ", flmLen];
    Print["MakeFileName::eps = ", eps];
    *)

    nMatr = Re[MatrixPower[eps, 1 / 2]];
    kMatr = Im[MatrixPower[eps, 1 / 2]];
    strFileName = strFileName <> "__" <> ToString[flmLen] <> "L";
    strFileName = strFileName <> "__h" <> ToString[thk];
    strFileName = strFileName <> "__n" <> ToString[Round[nMatr[[1, 1]] * 100]] <> "_" <> ToString[Round[nMatr[[2, 2]] * 100]] <> "_" <> ToString[Round[nMatr[[3, 3]] * 100]];
    If[Abs[Det[kMatr]] > 10^-7 || Abs[kMatr[[1, 1]]] > 10^-3 || Abs[kMatr[[2, 2]]] > 10^-3 || Abs[kMatr[[3, 3]]] > 10^-3, strFileName = strFileName <> "__k" <> ToString[Round[kMatr[[1, 1]] * 1000]] <> "_" <> ToString[Round[kMatr[[2, 2]] * 1000]] <> "_" <> ToString[Round[kMatr[[3, 3]] * 1000]]];
    (*Print["File Name = ",strFileName];*)

    Return[strFileName];
    ];
(* ============================================== *)
PerformAllCalculations[layeredSystem_?LayeredSystemQ, funcList_, description_, rawOpts___] :=
    Module[{opts, media, vars, extraOptions, optsFinal, performCalc, plotFigures, calc, coll, plotOpts, plotOpts2D, reqBetaLst, ii, reqBeta},
    (* Print["PerformAllCalculations::Starting..."]; *)

      opts = ProcessOptions[rawOpts];
      media = LayeredSystemGetMedia[layeredSystem];
      vars = LayeredSystemGetVarList[layeredSystem];
      extraOptions = LayeredSystemGetExtraOptions[layeredSystem];

      performCalc = BDPerformCalculations /. opts /. Options[BerremanDirect];
      plotFigures = BDPlotFigures /. opts /. Options[BerremanDirect];


      reqBetaLst = Table[RequiresCalculateBeta0and90 /. Options[funcList[[ii]]] /. Options[FieldAlgebra], {ii, 1, Length[funcList]}];

      reqBeta = Apply[Or, reqBetaLst];

      optsFinal = Join[{CalculateBeta0and90 -> reqBeta }, extraOptions, opts];

      (* Print["PerformAllCalculations::optsFinal = ", optsFinal]; *)

      If[performCalc,
        (
        (* Print["PerformAllCalculations::Calculating..."]; *)

        (* Print["PerformAllCalculations::media = ", media];
        Print["PerformAllCalculations::vars = ", vars];
        Print["PerformAllCalculations::funcList = ", funcList]; *)

          calc = CalcNew[media, vars, funcList, description, optsFinal];
          Print[GetRotationInfo[calc]];
          Print[strSeparator];

          coll = CalcCollectionNew[OutDir, BaseFileName, "No Description so far."];
          CalcCollectionAddCalc[coll, calc];
          CalcCollectionPerform[coll];
          CalcCollectionSave[coll, True];
          CalcCollectionSave[coll];
          PrintTimeUsed[];
        )
      ];

      If[plotFigures,
        (
          Print["PerformAllCalculations::Plotting figures..."];
          plotOpts = PlotOptions3D /. opts /. Options[BerremanDirect];
          CalcPlot3D[calc, True, plotOpts];
          plotOpts2D = PlotOptions2D /. opts /. Options[BerremanDirect];
          CalcPlot[calc, plotOpts2D];

          PrintTimeUsed[];
        )
      ];

      Return[coll];
    ];
(* ============================================== *)
