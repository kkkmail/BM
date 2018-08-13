(* ============================================== *)
(* :Summary: This module defines initialization operations. *)
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
BerremanInitVersionValue = 6.03;
(* ============================================== *)
Options[BerremanInit] = {BerremanInitVersion -> BerremanInitVersionValue, UseParallelTable -> True, UseRussianLanguage -> True};
(* ============================================== *)
Off[General::spell1];
Off[General::spell];
Off[General::"obspkg"];
Off[Solve::ifun];
(* ============================================== *)
UseRussianLanguageValue = True;
(* ============================================== *)
(*ProcessOptions ensures that options are in a flat list. It wraps List over raw options if necessary.*)
ProcessOptions[rawOpts___] := Module[{opts},
  opts = Flatten[{rawOpts}];
  Return[opts];
];
(* ============================================== *)
BerremanInitialize[optsRaw___] := Module[{opts},
  If[$VersionNumber < 10.0,
    (
      Print["Mathematica version is ", $VersionNumber, " Loading Geometry`Rotations` and LinearAlgebra`MatrixManipulation`"];

      Needs["Geometry`Rotations`"];
      Needs["LinearAlgebra`MatrixManipulation`"];

      Print["Initializing BooleanQ."];
      BooleanQ[x_] := If[Element[x, Booleans], True, False, False];
    ),
    (
      Print["Mathematica version is ", $VersionNumber, " Assigning RotationMatrix3D and BlockMatrix"];

      RotationMatrix3D[phi_, theta_, psi_] := (RotationMatrix[Pi - psi, {0, 0, 1}] . RotationMatrix[theta, {1, 0, 0}] . RotationMatrix[Pi - phi, {0, 0, 1}]);

      BlockMatrix[{{AAA_, BBB_}, {CCC_, DDD_}}] := ArrayFlatten[{{AAA, BBB}, {CCC, DDD}}];
    )
  ];

  Needs["VectorAnalysis`"];
  Needs["VariationalMethods`"];
];


(* ============================================== *)
strSeparator := "==============================================";
(* strCRLF=FromCharacterCode[10]<>FromCharacterCode[13]; *)
strCRLF = FromCharacterCode[10];
(* ============================================== *)
InitializeBM[pathList_] := InitializeBM[pathList, True];
(* ============================================== *)
InitializeBM[pathList_, useParallelTbl_, optsRaw___] := Module[{pathVar, useParTbl, opts},
  opts = ProcessOptions[optsRaw];

  UseRussianLanguageValue = UseRussianLanguage /. opts /. Options[BerremanInit];

  If[UseRussianLanguageValue,
    (
      Print["\:0418\:043d\:0438\:0446\:0438\:0430\:043b\:0438\:0437\:0430\:0446\:0438\:044f..."];
    ),
    (
      Print["Initializing..."];
    )
  ];

  BerremanInitialize[optsRaw];
  (* Print["pathList = ", pathList]; *)
  (* Print["useParallelTbl = ", useParallelTbl]; *)

  Print["Loading modules ..."];
  Get["BerremanCommon.m", Path -> pathList];
  Get["BerremanDirect.m", Path -> pathList];
  Get["FieldAlgebra.m", Path -> pathList];
  Get["BerremanInverse.m", Path -> pathList];
  Get["FieldIO.m", Path -> pathList];
  Get["OpticalModel.m", Path -> pathList];
  Get["OpticalElements.m", Path -> pathList];
  Get["OpticalModelFunctions.m", Path -> pathList];
  Get["OpticalDispersion.m", Path -> pathList];
  Print["... completed."];

  OutputCopyright[];

  If[useParallelTbl == True,
    Print["Starting parallel initialization ..."];
    DistributeDefinitions[pathList];
    ParallelEvaluate[Off[General::"obspkg"]];
    ParallelEvaluate[Get["BerremanInit.m", Path -> pathList]];
    ParallelEvaluate[Get["BerremanCommon.m", Path -> pathList]];
    ParallelEvaluate[Get["BerremanDirect.m", Path -> pathList]];
    ParallelEvaluate[Get["FieldAlgebra.m", Path -> pathList]];
    ParallelEvaluate[Get["BerremanInverse.m", Path -> pathList]];
    ParallelEvaluate[Get["FieldIO.m", Path -> pathList]];
    ParallelEvaluate[Get["OpticalModel.m", Path -> pathList]];
    ParallelEvaluate[Get["OpticalElements.m", Path -> pathList]];
    ParallelEvaluate[Get["OpticalModelFunctions.m", Path -> pathList]];
    Print["... completed"];
    ,
    Options[BerremanInit] = {BerremanInitVersion -> BerremanInitVersionValue, UseParallelTable -> False};
  ];

  (* Print["Options[BerremanInit] = ", Options[BerremanInit]]; *)

  If[UseRussianLanguageValue,
    (
      Print["\:0418\:043d\:0438\:0446\:0438\:0430\:043b\:0438\:0437\:0430\:0446\:0438\:044f \:0437\:0430\:0432\:0435\:0440\:0448\:0435\:043d\:0430."];
    ),
    (
      Print["Initialization completed."];
    )
  ];

  Print[strSeparator];
];
(* ============================================== *)
