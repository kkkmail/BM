(* ============================================== *)
(* :Summary: This module defines various optical elements. *)
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
Options[OpticalElements] =
    {
      CheckValues -> True,
      OpticalElementsVersion -> 6.04,
      PrintElementDescription -> False
    };
(* ============================================== *)
(* Stokes vectors and Mueller matricies: see Dennis Goldstein, Polarized Light, 2nd edition, Chapters 4 and 5, 2003, ISBN:0-8247-4053-X *)
(* ============================================== *)
MuellerPolarizerClassName = "MuellerPolarizer";
MuellerPolarizerDescrEN = "Polarizer with attenuations px and py.";
MuellerPolarizerDescrRU = "Polarizer \:0441 \:043a\:043e\:044d\:0444\:0444\:0438\:0446\:0438\:0435\:043d\:0442\:0430\:043c\:0438 \:043e\:0441\:043b\:0430\:0431\:043b\:0435\:043d\:0438\:044f px \:0438 py.";
(* ============================================== *)
MuellerPolarizerTrigClassName = "MuellerPolarizerTrig";
MuellerPolarizerTrigDescrEN = "Polarizer (trig) with total attenuation p and angle of rotation \[Alpha].";
MuellerPolarizerTrigDescrRU = "Polarizer (trig) \:0441 \:043a\:043e\:044d\:0444\:0444\:0438\:0446\:0438\:0435\:043d\:0442\:043e\:043c \:043e\:0441\:043b\:0430\:0431\:043b\:0435\:043d\:0438\:044f p \:0438 \:0443\:0433\:043b\:043e\:043c \:043f\:043e\:0432\:043e\:0440\:043e\:0442\:0430 \[Alpha].";
(* ============================================== *)
MuellerRetarderClassName = "MuellerRetarder";
MuellerRetarderDescrEN = "Retarder with a fast axis at angle \[Theta] and phase shift \[Delta].";
MuellerRetarderDescrRU = "Retarder \:0441 \:0431\:044b\:0441\:0442\:0440\:043e\:0439 \:043e\:0441\:044c\:044e \:043f\:043e\:0434 \:0443\:0433\:043b\:043e\:043c \[Theta] \:0438 \:043f\:043e\:0432\:043e\:0440\:043e\:0442\:043e\:043c \:0444\:0430\:0437\:044b \:043d\:0430 \:0443\:0433\:043e\:043b \[Delta].";
(* ============================================== *)
MuellerModulatorClassName = "MuellerModulator";
MuellerModulatorDescrEN = "Modulator";
MuellerModulatorDescrRU = "Modulator";
(* ============================================== *)
MuellerRotatorClassName = "MuellerRotator";
MuellerRotatorDescrEN = "Rotator with the rotation angle \[Theta].";
MuellerRotatorDescrRU = "Rotator \:0441 \:0443\:0433\:043b\:043e\:043c \:043f\:043e\:0432\:043e\:0440\:043e\:0442\:0430 \[Theta].";
(* ============================================== *)
MuellerSampleClassName = "MuellerSample";
MuellerSampleDescrEN = "Sample with parameters \[Psi] and \[CapitalDelta] - it is unclear from which book it came from.";
MuellerSampleDescrRU = "Sample with parameters \[Psi] and \[CapitalDelta] - \:043f\:043e\:043d\:044f\:0442\:0438\:044f \:043d\:0435 \:0438\:043c\:0435\:044e \:043e\:0442\:043a\:0443\:0434\:0430 \:043e\:043d \:0432\:0437\:044f\:043b\:0441\:044f \:0438 \:0447\:0442\:043e \:043e\:043d \:0434\:0435\:043b\:0430\:0435\:0442.";
(* ============================================== *)
MuellerClassNameLst =
    {
      MuellerPolarizerClassName,
      MuellerPolarizerTrigClassName,
      MuellerRetarderClassName,
      MuellerModulatorClassName,
      MuellerRotatorClassName,
      MuellerSampleClassName
    };

MuellerDescriptionEnLst =
    {
      MuellerPolarizerDescrEN,
      MuellerPolarizerTrigDescrEN,
      MuellerRetarderDescrEN,
      MuellerModulatorDescrEN,
      MuellerRotatorDescrEN,
      MuellerSampleDescrEN
    };

MuellerDescriptionRuLst =
    {
      MuellerPolarizerDescrRU,
      MuellerPolarizerTrigDescrRU,
      MuellerRetarderDescrRU,
      MuellerModulatorDescrRU,
      MuellerRotatorDescrRU,
      MuellerSampleDescrRU
    };

(* ============================================== *)
IdxMuellerPolarizer = 1;
IdxMuellerPolarizerTrig = 2;
IdxMuellerRetarder = 3;
IdxMuellerModulator = 4;
IdxMuellerRotator = 5;
IdxMuellerSample = 6;
(* ============================================== *)
BerremanDirectClassName = "BerremanDirect";
(* ============================================== *)
ElementGetCallTable[element_] := element[[1]];
ElementGetDataTable[element_] := element[[2]];
ElementGetDescription[element_] := DataTableGetDescription[ElementGetDataTable[element]];

ElementGetValues[element_] :=
    Module[{valList, dataTbl, varList, varListLen, ii},
      dataTbl = ElementGetDataTable[element];
      varList = DataTableGetVarList[dataTbl];
      varListLen = DataTableGetValueListLength[dataTbl];
      valList = Table[varList[[ii, 1]], {ii, 1, varListLen}];
      Return[valList];
    ];
(* ============================================== *)
DataTableGetCalc[dataTbl : {___}] := dataTbl[[1]];
DataTableGetFuncPointer[dataTbl : {___}] := dataTbl[[1]];
DataTableGetValueListLength[dataTbl : {___}] := dataTbl[[2]];
DataTableGetVarList[dataTbl : {___}] := dataTbl[[3]];
DataTableGetDescription[dataTbl : {___}] := dataTbl[[4]];
(* ============================================== *)
CreatePolarizer[mptPx_, mptPy_, optsRaw___] := CreateMuellerElement[MuellerPolarizerClassName, {mptPx, mptPy}, optsRaw];
CreatePolarizerTrig[mptP_, mptGamma_, optsRaw___] := CreateMuellerElement[MuellerPolarizerTrigClassName, {mptP, mptGamma}, optsRaw];
CreateRetarader[mpTheta_, mpDelta_, optsRaw___] := CreateMuellerElement[MuellerRetarderClassName, {mpTheta, mpDelta}, optsRaw];
CreateModulator[mpTheta_, optsRaw___] := CreateMuellerElement[MuellerModulatorClassName, {mpTheta}, optsRaw];
CreateRotator[mpRt_, optsRaw___] := CreateMuellerElement[MuellerRotatorClassName, {mpRt}, optsRaw];
CreateSample[mpPsi_, mpDelta_, optsRaw___] := CreateMuellerElement[MuellerSampleClassName, {mpPsi, mpDelta}, optsRaw];
(* ============================================== *)
CreateMuellerElement[clsName_, varList_?MatrixQ, optsRaw___] :=
    Module[{element, callTbl, dataTbl, dataTblIndt, indtVal, ii, descrTbl, opts, printElementDescriptionVal},
      callTbl = {{ClassName, clsName}, {CalculateMullerMatrixName, CalculateMuellerMatrix}, {GetVarListName, GetVarListMuellerElement}};

      opts = ProcessOptions[optsRaw];
      printElementDescriptionVal = PrintElementDescription /. opts /. Options[OpticalElements];
      indtVal = {Indeterminate, Indeterminate, Indeterminate, Indeterminate, Indeterminate};
      dataTblIndt = {MuellerIndeterminate, 0, {}};
      dataTbl = dataTblIndt;

      descrTbl = If[UseRussianLanguageValue, MuellerDescriptionRuLst, MuellerDescriptionEnLst, MuellerDescriptionEnLst];

      dataTbl = Which[
        clsName == MuellerPolarizerClassName, {MuellerPolarizer, 2, varList, descrTbl[[IdxMuellerPolarizer]]},
        clsName == MuellerPolarizerTrigClassName, {MuellerPolarizerTrig, 2, varList, descrTbl[[IdxMuellerPolarizerTrig]]},
        clsName == MuellerRetarderClassName, {MuellerRetarder, 2, varList, descrTbl[[IdxMuellerRetarder]]},
        clsName == MuellerModulatorClassName, {MuellerModulator, 1, varList, descrTbl[[IdxMuellerModulator]]},
        clsName == MuellerRotatorClassName, {MuellerRotator, 1, varList, descrTbl[[IdxMuellerRotator]]},
        clsName == MuellerSampleClassName, {MuellerSample, 2, varList, descrTbl[[IdxMuellerSample]]}
      ];

      If[dataTbl[[2]] != Length[varList],
        (
          Print["CreateMuellerElement::Invalid varList length for a given class."];
          dataTbl = dataTblIndt;
          dataTbl[[3]] = Join[dataTbl[[3]], Table[indtVal, {ii, 1, dataTbl[[2]]}]];
        )
      ];

      element = {callTbl, dataTbl};

      If[printElementDescriptionVal, Print[ElementGetDescription[element]]];

      Return[element];
    ];
(* ============================================== *)
CalculateMuellerMatrix[element_, valueList_?VectorQ, optsRaw___] :=
    Module[{muellerMatrix, dataTbl, argList, funcName},
      If[!CheckClassName[element, MuellerClassNameLst],
        (
          Print["CalculateMuellerMatrix::ClassName is invalid."];
          Return[IndeterminateMuellerMatrix];
        )
      ];

      dataTbl = ElementGetDataTable[element];

      (*
      If[!VectorQ[dataTbl],
      (
      Print["CalculateMuellerMatrix::DataTable is invalid."];
      Return[IndeterminateMuellerMatrix];
      )
      ];

      If[Length[dataTbl]\[NotEqual] 2,
      (
      Print["CalculateMuellerMatrix::DataTable length is invalid."];
      Return[IndeterminateMuellerMatrix];
      )
      ];
      *)

      If[Length[valueList] != DataTableGetValueListLength[dataTbl],
        (
          Print["CalculateMuellerMatrix::valueList length is invalid."];
          Return[IndeterminateMuellerMatrix];
        )
      ];

      argList = Join[valueList, {optsRaw}];
      funcName = DataTableGetFuncPointer[dataTbl];

      muellerMatrix = Apply[funcName, argList];

      Return[muellerMatrix];
    ];
(* ============================================== *)
GetVarListMuellerElement[element_] :=

    Module[{dataTbl, varLst},
      If[!CheckClassName[element, MuellerClassNameLst],
        (
          Print["GetVarListMuellerElement::ClassName is invalid."];
          Return[{}];
        )
      ];
      dataTbl = ElementGetDataTable[element];
      varLst = DataTableGetVarList[dataTbl];

      Return[varLst];
    ];
(* ============================================== *)
MuellerIndeterminate[optsRaw___] := IndeterminateMuellerMatrix;
(* ============================================== *)
MuellerPolarizer[pxVal_, pyVal_, optsRaw___] :=
    Module[{muellerMatrix, px, py, opts, chkVal},
      opts = Flatten[{optsRaw}];
      chkVal = CheckValues /. opts /. Options[OpticalElements];

      px = pxVal;
      px = pyVal;

      If[chkVal,
        (
          If[NumericQ[pxVal], px = Min[Abs[pxVal], 1]];
          If[NumericQ[pyVal], py = Min[Abs[pyVal], 1]];
        )
      ];

      muellerMatrix = (1 / 2) * {{px^2 + py^2, px^2 - py^2, 0, 0}, {px^2 - py^2, px^2 + py^2, 0, 0}, {0, 0, 2 * px * py, 0}, {0, 0, 0, 2 * px * py}};

      Return[muellerMatrix];
    ];
(* ============================================== *)
MuellerPolarizerTrig[pVal_, gammaVal_, optsRaw___] :=
    Module[{pxVal, pyVal, muellerMatrix, opts, chkVal, p, gamma},
      opts = Flatten[{optsRaw}];
      chkVal = CheckValues /. opts /. Options[OpticalElements];

      p = pVal;
      gamma = gammaVal;

      If[chkVal,
        (
          If[NumericQ[pVal], p = Min[Abs[pVal], 1]];
          If[NumericQ[gammaVal], gamma = Min[Abs[gammaVal], Pi / 4]];
        )
      ];

      pxVal = p * Cos[2 * gamma];
      pyVal = p * Sin[2 * gamma];

      muellerMatrix = MuellerPolarizer[pxVal, pyVal, optsRaw];

      Return[muellerMatrix];
    ];
(* ============================================== *)
MuellerRetarderClassName = "MuellerRetarder";
(* ============================================== *)
MuellerRetarderSimple[delta_, optsRaw___] :=
    Module[{muellerMatrix, opts, chkVal},
      muellerMatrix = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, Cos[delta], Sin[delta]}, {0, 0, -Sin[delta], Cos[delta]}};
      Return[muellerMatrix];
    ];
(* ============================================== *)
MuellerRetarder[theta_, delta_, optsRaw___] :=
    Module[{muellerMatrix, opts, chkVal},
      muellerMatrix = {{1, 0, 0, 0}, {0, Cos[2 * theta]^2 + Sin[2 * theta]^2 * Cos[delta], (1 - Cos[delta]) * Sin[2 * theta] * Cos[2 * theta], -Sin[2 * theta] * Sin[delta]}, {0, (1 - Cos[delta]) * Sin[2 * theta] * Cos[2 * theta], Sin[2 * theta]^2 + Cos[2 * theta]^2 * Cos[delta], Cos[2 * theta] * Sin[delta]}, {0, Sin[2 * theta] * Sin[delta], -Cos[2 * theta] * Sin[delta], Cos[delta]}};
      Return[muellerMatrix];
    ];
(* ============================================== *)
MuellerModulatorClassName = "MuellerModulator";
(* ============================================== *)
MuellerModulator[fi_, optsRaw___] :=
    Module[{muellerMatrix, opts, chkVal},
      muellerMatrix = MuellerRetarderSimple[-fi, optsRaw];
      Return[muellerMatrix];
    ];
(* ============================================== *)
MuellerRotator[theta_, optsRaw___] :=
    Module[{muellerMatrix, opts, chkVal},
      muellerMatrix = {{1, 0, 0, 0}, {0, Cos[2 * theta], Sin[2 * theta], 0}, {0, -Sin[2 * theta], Cos[2 * theta], 0}, {0, 0, 0, 1}};
      Return[muellerMatrix];
    ];
(* ============================================== *)
MuellerSample[psi_, delta_, optsRaw___] :=
    Module[{muellerMatrix, opts, chkVal},
      muellerMatrix = {{1, -Cos[2 * psi], 0, 0}, {-Cos[2 * psi], 1, 0, 0}, {0, 0, Sin[2 * psi] * Cos[delta], Sin[2 * psi] * Sin[delta]}, {0, 0, -Sin[2 * psi] * Sin[delta], Sin[2 * psi] * Cos[delta]}};
      Return[muellerMatrix];
    ];
(* ============================================== *)
(* ============================================== *)
(* Berreman Direct *)
(* ============================================== *)
CreateElementBM[calc_] :=
    Module[{callTbl, dataTbl, element, varLst, vLen},
      callTbl = {{ClassName, BerremanDirectClassName}, {CalculateMullerMatrixName, CalculateMuellerMatrixBM}, {GetVarListName, GetVarListBM}};

      varLst = CalcVarList[calc];
      vLen = Length[varLst];

      dataTbl = {calc, vLen, varLst};

      element = {callTbl, dataTbl};
      Return[element];
    ];
(* ============================================== *)
CalculateMuellerMatrixBM[element_, valueList_?VectorQ, optsRaw___] :=
    Module[{muellerMatrix, dataTbl, argList, funcName, Media, Calc, varLst, varNew, FuncList, clc, eld, ii, vLen, opts, fullSol, outpt},

    (* Print["CalculateMuellerMatrixBM::Starting..."]; *)

      If[!CheckClassName[element, BerremanDirectClassName],
        (
          Print["CalculateMuellerMatrixBM::ClassName is invalid."];
          Return[IndeterminateMuellerMatrix];
        )
      ];

      dataTbl = ElementGetDataTable[element];

      (*
      If[Length[dataTbl]\[NotEqual] 2,
      (
      Print["CalculateMuellerMatrixBM::DataTable length is invalid."];
      Return[IndeterminateMuellerMatrix];
      )
      ];
      *)

      If[Length[valueList] != DataTableGetValueListLength[dataTbl],
        (
          Print["CalculateMuellerMatrixBM::valueList length is invalid."];
          Return[IndeterminateMuellerMatrix];
        )
      ];

      Calc = DataTableGetCalc[dataTbl];
      Media = CalcMedia[Calc];
      varLst = CalcVarList[Calc];
      varNew = varLst;
      vLen = Length[varLst];

      For[ii = 1, ii <= vLen, ii++,
        (
          varNew[[ii, 1]] = valueList[[ii]];
          varNew[[ii, 2]] = valueList[[ii]];
        )
      ];

      (*
      Print["CalculateMuellerMatrixBM::Calc = ", Calc];
      Print["CalculateMuellerMatrixBM::Media = ", Media];
      Print["CalculateMuellerMatrixBM::varLst = ", varLst];
      Print["CalculateMuellerMatrixBM::varNew = ", varNew];
      Print["CalculateMuellerMatrixBM::vLen = ", vLen];
      *)

      FuncList = {MuellerMatrixR};
      opts = {CalculateBeta0and90 -> True, PrintTimeEstimate -> False, PrintCalculationProgress -> False, PrintCalculationDetails -> False, CalcOptions[Calc]};
      clc = CalcNew[Media, varNew, FuncList, "", opts];
      CalcPerform[clc];

      (*
      Print[strSeparator];
      Print["CalculateMuellerMatrixBM::clc = ", clc];
      *)

      outpt = CalcGetOutput[clc];

      (* Print["CalculateMuellerMatrixBM::outpt = ", outpt]; *)

      muellerMatrix = outpt[[1, 1]];

      Return[muellerMatrix];
    ];
(* ============================================== *)
GetVarListBM[element_] :=
    Module[{dataTbl, varLst, Calc},
      If[!CheckClassName[element, BerremanDirectClassName],
        (
          Print["GetVarListBM::ClassName is invalid."];
          Return[Indeterminate];
        )
      ];

      dataTbl = ElementGetDataTable[element];
      Calc = DataTableGetCalc[dataTbl];
      varLst = CalcVarList[Calc];
      varLst[[VarListBetaIdx, 2]] = varLst[[VarListBetaIdx, 1]];
      (* varLst[[VarListGammaIdx,2]]=varLst[[VarListGammaIdx,1]]; *)
      varLst[[VarListEllipticityIdx, 2]] = varLst[[VarListEllipticityIdx, 1]];

      Return[varLst];
    ];
(* ============================================== *)
GetIncidentLightInfoBM[element_] :=
    Module[{dataTbl, varLst, Calc, inclght},
      If[!CheckClassName[element, BerremanDirectClassName],
        (
          Print["GetIncidentLightBM::ClassName is invalid."];
          Return[Indeterminate];
        )
      ];

      dataTbl = ElementGetDataTable[element];
      Calc = DataTableGetCalc[dataTbl];
      varLst = CalcVarList[Calc];

      inclght = {VarListGetLambda[varLst], VarListGetFita[varLst], VarListGetBeta[varLst], VarListGetGamma[varLst], VarListGetEllipticity[varLst]};

      Return[inclght];
    ];
(* ============================================== *)
GetMuellerElementDescription[element_] :=
    Module[{descr},

      If[!CheckClassName[element, MuellerClassNameLst],
        (
          Print["GetMuellerElementDescription::ClassName is invalid."];
          Return["Error!!!"];
        )
      ];

      dataTbl = ElementGetDataTable[element];


      If[Length[valueList] != DataTableGetValueListLength[dataTbl],
        (
          Print["CalculateMuellerMatrix::valueList length is invalid."];
          Return[IndeterminateMuellerMatrix];
        )
      ];

      argList = Join[valueList, {optsRaw}];
      funcName = DataTableGetFuncPointer[dataTbl];

      muellerMatrix = Apply[funcName, argList];

      Return[muellerMatrix];

      Return[descr];
    ];
(* ============================================== *)
