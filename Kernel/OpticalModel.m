(* ============================================== *)
(* :Summary: This module defines optical model logic. *)
(* ============================================== *)
(* :Author: Konstantin K. Konstantinov *)
(* :Email: konstantin.k.konstantinov@gmail.com *)
(* :License type: GPL v3 or any later version, see http://www.gnu.org/licenses/ *)
(* :Copyright: K^3, 2001 - 2018 *)
(* :Version: Revision: 6.03.001, Date: 2018/05/05 *)
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
Options[OpticalModel] = {OpticalModelVersion -> 6.03};
(* ============================================== *)
ClassName = "ClassNameFunc";
CalculateMullerMatrixName = "CalculateMullerMatrixFunc";
GetVarListName = "GetVarListFunc";
(* ============================================== *)
IndeterminateMuellerMatrix = Table[Indeterminate, {iii, 1, 4}, {jjj, 1, 4}];
(* ============================================== *)
CreateOpticalModel[description_, funcList_, opts___] := Module[{model, elemList},
  elemList = {};
  model = {description, elemList, funcList, {opts}};
  Return[model];
];
(* ============================================== *)
CreateOpticalElement[elementDescriptor_, elemParamList_] := 0;
(* ============================================== *)ModelGetDescription[model : {___}] := model[[1]];
ModeGetDescription[model : {___}] := model[[1]];
ModelGetElementList[model : {___}] := model[[2]];
ModeGetFuncList[model : {___}] := model[[3]];
ModeGetOptions[model : {___}] := Flatten[{model[[4]]}];
(* ============================================== *)
Attributes[ModelSetElementList] = {HoldFirst};
ModelSetElementList[model_, elementListNew : {___}] := Module[{},
  model[[2]] = elementListNew;
  Return[model];
];
(* ============================================== *)
Attributes[ModelAddOpticalElement] = {HoldFirst};
ModelAddOpticalElement[model_, element_] := Module[{elementList, elementListNew},
  elementList = ModelGetElementList[model];
  elementListNew = Join[elementList, {element}];
  (* model= *)
  ModelSetElementList[model, elementListNew];
  Return[model];
];
(* ============================================== *)
ModelGetVarList[model_] := Module[{elemList, callTbl, element, len, ii, getVarListFunc, varLst, varLstAll},
  elemList = ModelGetElementList[model];
  len = Length[elemList];

  varLstAll = {};

  For[ii = 1, ii <= len, ii++,
    (
      element = elemList[[ii]];
      callTbl = ElementGetCallTable[element];
      getVarListFunc = CallTableGetMember[callTbl, GetVarListName];

      If[getVarListFunc == Indeterminate,
        (
          Print["ModelGetVarList::Invalid GetVarList function for element: ", element];
          Return[Indeterminate];
        )
      ];

      varLst = Apply[getVarListFunc, {element}];
      varLstAll = Join[varLstAll, varLst];
    )
  ];
  Return[varLstAll];
];
(* ============================================== *)
ModelCalculateMuellerMatrix[model_, varValueLst_?VectorQ] := Module[{muellerMatrix, muellerMatrixTotal, len, ii, jj, elemList, element, callTbl, funcPointer, elemValueList, dataTbl, elemVarLen, elemVarStartIdx},
  muellerMatrixTotal = IdentityMatrix[4];

  elemList = ModelGetElementList[model];
  len = Length[elemList];
  elemVarStartIdx = 1;

  For[ii = 1, ii <= len, ii++,
    (
      element = elemList[[ii]];
      callTbl = ElementGetCallTable[element];
      funcPointer = CallTableGetMember[callTbl, CalculateMullerMatrixName];
      dataTbl = ElementGetDataTable[element];
      elemVarLen = DataTableGetValueListLength[dataTbl];

      If[funcPointer == Indeterminate,
        (
          Print["ModelGetVarList::Invalid GetVarList function for element: ", element];
          Return[IndeterminateMuellerMatrix];
        )
      ];

      elemValueList = Table[varValueLst[[jj]], {jj, elemVarStartIdx, elemVarStartIdx + elemVarLen - 1}];
      elemVarStartIdx = elemVarStartIdx + elemVarLen;

      muellerMatrix = Apply[funcPointer, {element, elemValueList}];
      muellerMatrixTotal = muellerMatrix . muellerMatrixTotal;

    (*
    Print[strSeparator];

    Print["ModelCalculateMuellerMatrix::ii = ", ii];
    (* Print["ModelCalculateMuellerMatrix::element = ", element]; *)
    Print["ModelCalculateMuellerMatrix::callTbl = ", callTbl];
    Print["ModelCalculateMuellerMatrix::funcPointer = ", funcPointer];
    (* Print["ModelCalculateMuellerMatrix::dataTbl = ", dataTbl]; *)
    Print["ModelCalculateMuellerMatrix::elemVarLen = ", elemVarLen];
    Print["ModelCalculateMuellerMatrix::elemValueList = ", N[elemValueList]];
    Print["ModelCalculateMuellerMatrix::muellerMatrix = ", N[Chop[muellerMatrix]]// MatrixForm];
    Print["ModelCalculateMuellerMatrix::muellerMatrixTotal = ", N[Chop[muellerMatrixTotal]]// MatrixForm];

    Print[strSeparator];
    Print[strSeparator];
    *)
    )
  ];

  Return[muellerMatrixTotal];
];
(* ============================================== *)
ModelPlot3D[model_, SwapXY_, pltOptsRaw___] := Module[{FuncList, FuncNameList, len, CalcGridArr, pltPoints, pltPointsX, pltPointsY, fCalc, xVar, yVar, xStart, xEnd, yStart, yEnd, plotOpts, varLst, vLen, xName, yName, zName, xVarCnt, yVarCnt, var, xAssigned, yAssigned, varNew, opts, i, j, k, f, usePltLbl, prtfDesc, FuncDescList, useParallelTbl, meshVar, colorFunc, colorFuncScaling, data, plt3Dtype, funcName, fTmp, isNumericX, isNumericY, xVarr, yVarr, MyTable, MyApproximateFunc, funcLen, modOpts},
(* Print["ModelPlot3D"]; *)
  modOpts = ModeGetOptions[model];
  FuncList = Flatten[{ModeGetFuncList[model]}];
  FuncDescList = GetFuncDescList[FuncList, modOpts];
  FuncNameList = GetFuncNameList[FuncList, modOpts];
  funcLen = Length[FuncList];

  (*
  Print["ModelPlot3D::FuncList = ", FuncList];
  Print["ModelPlot3D::modOpts = ", modOpts];
  Print["ModelPlot3D::FuncDescList = ", FuncDescList];
  Print["ModelPlot3D::FuncNameList = ", FuncNameList];
  *)

  prtfDesc = PrintFunctionDescription /. modOpts /. Options[BerremanDirect];
  pltPoints = PlotPoints /. Flatten[{pltOptsRaw}] /. {PlotPoints -> 25};
  pltPointsX = Flatten[{{pltPoints}, {pltPoints}}][[1]];
  pltPointsY = Flatten[{{pltPoints}, {pltPoints}}][[2]];
  (*
  Print["ModelPlot3D::pltPointsX = ", pltPointsX];
  Print["ModelPlot3D::pltPointsY = ", pltPointsY];
  *)
  meshVar = {pltPointsX - 2, pltPointsY - 2};
  usePltLbl = UsePlotLabel3D /. Flatten[{pltOptsRaw}] /. Options[BerremanDirect] /. {UsePlotLabel3D -> True};
  useParallelTbl = UseParallelTable /. Flatten[{pltOptsRaw}] /. Flatten[{modOpts}] /. Options[BerremanInit];

  varLst = ModelGetVarList[model];
  varNew = varLst;
  vLen = Length[varLst];

  xAssigned = False;
  yAssigned = False;

  opts = {PrintTimeEstimate -> False, PrintCalculationProgress -> False, PrintCalculationDetails -> False, modOpts};

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

  (*
  Print["=============="];
  Print["ModelPlot3D::xName = ",xName];
  Print["ModelPlot3D::xAssigned = ", xAssigned, ", yAssigned = ", yAssigned];
  *)

  If[xAssigned === True && yAssigned === True,
    len = Min[Length[FuncList], Length[FuncNameList]];

    fCalc[xVar_, yVar_] :=
        Module[{varN, outpt, retval, mm, varValueLst, jj},
          varN = varNew;
          varN[[xVarCnt, 1]] = xVar;
          varN[[xVarCnt, 2]] = xVar;
          varN[[yVarCnt, 1]] = yVar;
          varN[[yVarCnt, 2]] = yVar;

          varValueLst = Table[varN[[jj, 1]], {jj, 1, vLen}];

          (* Print["ModelPlot3D::varValueLst = ",varValueLst]; *)

          mm = ModelCalculateMuellerMatrix[model, varValueLst];

          retval = Table[Apply[FuncList[[jj]], {mm}], {jj, 1, funcLen}];

          Return[retval];
        ];

    (* Print["ModelPlot3D::fCalc[",xStart,", ",yStart,"] = ",fCalc[xStart,yStart]]; *)

    If[useParallelTbl == False,
      CalcGridArr = Table[fCalc[xStart + (i - 1) * (xEnd - xStart) / (pltPointsX - 1), yStart + (j - 1) * (yEnd - yStart) / (pltPointsY - 1)], {i, 1, pltPointsX}, {j, 1, pltPointsY}],
      (
      (* Print["ModelPlot3D::Using parallel table ..."]; *)
        DistributeDefinitions[model, fCalc, xStart, xEnd, vLen, pltPointsX, yStart, yEnd, pltPointsY, xVarCnt, yVarCnt, varNew, FuncList, opts, funcLen];
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
        Print["ModelPlot3D::xVar = ",xVar,", yVar = ",yVar];
        Print["ModelPlot3D::isNumericX = ",isNumericX,", isNumericY = ",isNumericY];
        *)

        If[isNumericX && isNumericY,
          iii = Max[Min[1 + Round[(pltPointsX - 1) * (xVar - xStart) / (xEnd - xStart)], pltPointsX], 1];
          jjj = Max[Min[1 + Round[(pltPointsY - 1) * (yVar - yStart) / (yEnd - yStart)], pltPointsY], 1];
          outpt = CalcGridArr[[iii, jjj]];

          (*
          Print["ModelPlot3D::iii = ",iii,", jjj = ",jjj,", k = ",k];
          Print["ModelPlot3D::outpt = ",outpt];
          *)

          (* retval=N[ChopValue[outpt[[1,k]],clcOpts]]; *)
          retval = N[ChopValue[outpt[[k]], modOpts]];
          ,
          retval = -10;
        ];
        SetAttributes[f, NumericFunction];

        fTmp = fTmp <> ToString[retval] <> ", ";
        (* Print["ModelPlot3D::f::retval = ",retval]; *)
        Return[retval];
      ];

      zName = FuncNameList[[k]];
      funcName = FuncList[[k]];

      MyTable = Flatten[Table[{xVar, yVar, f[xVar, yVar]}, {xVar, xStart, xEnd, (xEnd - xStart) / (pltPointsX - 1)}, {yVar, yStart, yEnd, (yEnd - yStart) / (pltPointsY - 1)}], 1];
      MyApproximateFunc = Interpolation[MyTable];

      (*
      Print["ModelPlot3D::CalcGridArr = ", N[CalcGridArr]];
      Print["ModelPlot3D::xStart = ", xStart];
      Print["ModelPlot3D::xEnd = ", xEnd];
      Print["ModelPlot3D::yStart = ", yStart];
      Print["ModelPlot3D::yEnd = ", yEnd];
      Print["ModelPlot3D::pltPointsX = ", pltPointsX];
      Print["ModelPlot3D::pltPointsY = ", pltPointsY];
      Print["ModelPlot3D::zName = ", zName];
      Print["ModelPlot3D::FuncList = ", FuncList];
      *)

      plt3Dtype = "Plot3D";

      (* Print["ModelPlot3D::funcName = ", funcName,", plt3Dtype = ",plt3Dtype]; *)
      (* Print["Before Plot"]; *)
      (* Print[Evaluate[plotOpts]]; *)

      If[plt3Dtype == "RevolutionPlot3D",
        If[usePltLbl === True,
          plotOpts = {pltOptsRaw, AxesLabel -> {xName, yName, ""}, PlotLabel -> zName, ImageSize -> BDIMAGESIZE, Mesh -> meshVar, MaxRecursion -> 0 (* ,ColorFunction\[Rule]colorFunc,ColorFunctionScaling \[Rule] colorFuncScaling *)}, plotOpts = {pltOptsRaw, AxesLabel -> {xName, yName, zName}, ImageSize -> BDIMAGESIZE, Mesh -> meshVar, MaxRecursion -> 0 (*,ColorFunction\[Rule]colorFunc,ColorFunctionScaling \[Rule] colorFuncScaling *) }
        ];
        Print[RevolutionPlot3D[N[f[xVar, yVar]], {xVar, xStart, xEnd}, {yVar, yStart, yEnd}, Evaluate[plotOpts]]];
        ,
        If[usePltLbl === True,
          plotOpts = {pltOptsRaw, AxesLabel -> {xName, yName, ""}, PlotLabel -> zName, Compiled -> False, LabelStyle -> BDPLTTEXTOPTS, ImageSize -> BDIMAGESIZE, Mesh -> meshVar, MaxRecursion -> 0(* ,ColorFunction\[Rule]colorFunc,ColorFunctionScaling \[Rule] colorFuncScaling *)}, plotOpts = {pltOptsRaw, AxesLabel -> {xName, yName, zName}, Compiled -> False, LabelStyle -> BDPLTTEXTOPTS, ImageSize -> BDIMAGESIZE, Mesh -> meshVar, MaxRecursion -> 0 (*,ColorFunction\[Rule]colorFunc,ColorFunctionScaling \[Rule] colorFuncScaling *)}
        ];
        (* Print["ModelPlot3D::plotOpts = ", plotOpts]; *)

        If[$VersionNumber >= 10, plotOpts = Join[plotOpts, {PlotTheme -> {"Classic", "ClassicLights"}}]];

        Print[Plot3D[MyApproximateFunc[xVar, yVar], {xVar, xStart, xEnd}, {yVar, yStart, yEnd}, Evaluate[plotOpts]]];
      ];

      (*
      Print["ModelPlot3D::CalcGridArr = ", CalcGridArr // MatrixForm];
      Print["ModelPlot3D::f::retval = ",fTmp];
      *)

      Print[strSeparator];
      Print["    "];
      Print["    "];

      , {k, len}
    ];
    , Print["ModelPlot3D::Second variable was not found. Cannot perform Plot3D..."];
  ];
];
(* ============================================== *)
StokesVectorNew[varList : {{___}, ___}] := Module[{sVec},
  sVec = 0;
  Return[sVec];
];
(* ============================================== *)
StokesVectorNew[s0_, s1_, s2_, s3_] := Module[{sVec},
  sVec = {False, {{s0, s0, 1, "S0", 1}, {s1, s1, 1, "S1", 1}, {s2, s2, 1, "S2", 1}, {s3, s3, 1, "S3", 1}}};
  Return[sVec];
];
(* ============================================== *)
StokesVectorUseIncidentLight[sv : {}] := sv[[1]];
(* ============================================== *)

(*
ApplyParameterValue[];
(* ============================================== *)
ApplyParameterValueToScalar[];
(* ============================================== *)
ApplyParameterValueToList[];
*)
(* ============================================== *)
CallTableGetMember[callTbl_?MatrixQ, memberName_] := Module[{member, len, ii},
  member = Indeterminate;
  len = Length[callTbl];

  For[ii = 1, ii <= len, ii++,
    (
      If[callTbl[[ii, 1]] == memberName,
        (
          member = callTbl[[ii, 2]];
          Break[];
        )
      ];
    )
  ];

  Return[member];
];
(* ============================================== *)
CheckClassName[element_, clsNameList_?VectorQ] := Module[{clsName, len, ii},
  len = Length[clsNameList];

  For[ii = 1, ii <= len, ii++,
    (
      If[CheckClassName[element, clsNameList[[ii]]],
        (
          Return[True];
        )
      ];
    )
  ];
  Return[False];
];
(* ============================================== *)
CheckClassName[element_, clsName_] := Module[{callTbl, len, ii, retVal, member},
  callTbl = ElementGetCallTable[element];
  (*
  Print["CheckClassName::callTbl = ", callTbl];
  Print["CheckClassName::clsName = ", clsName];
  *)

  If[!MatrixQ[callTbl], Return[False]];
  member = CallTableGetMember[callTbl, ClassName];
  (* Print["CheckClassName::member = ", member]; *)

  retVal = False;
  If[member == clsName, retVal = True];
  Return[retVal];
];
(* ============================================== *)
