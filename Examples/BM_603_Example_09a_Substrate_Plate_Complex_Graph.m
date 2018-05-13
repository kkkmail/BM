(* ============================================== *)
ClearAll["Global`*"];
(* ============================================== *)
BaseDir = "C:\\GitHub\\BM\\";
PathList = {BaseDir <> "Kernel\\"};
OutDir = BaseDir <> "Calc\\";
(* ============================================== *)
BaseFileName = "Example_09";
(* ============================================== *)
useParallelTbl = False;
Get["BerremanInit.m", Path -> PathList];
InitializeBM[PathList, useParallelTbl];
(* ============================================== *)
opts =
    {
      BDPlotFigures -> True,
      UseEulerAngles -> False,
      NoOfAveragingPoints -> 3
    };
(* ============================================== *)

FuncList =
    {
      EpsComponent[1, 1, 1, 1],
      {EpsComponent[1, 1, 2, 2], EpsComponent[1, 1, 3, 3]},
      StokesVectorR[1],
      StokesVectorR[2],
      StokesVectorR[3],
      StokesVectorR[4],
      RFull,
      TFull,
      StokesGammaR,
      StokesGammaDegreeR,
      StokesChiR,
      StokesChiDegreeR,
      StokesPolarizedR,
      XirDegree,
      Elr,
      PsiPPDegree,
      DeltaPPDegree,
      {Rx, Ry},
      {Tx, Ty}
    };
(* ============================================== *)
systemDescription = "Uniaxial slightly absorbing thick substrate plate - dispersion calculations.";
Print["!!! For absorbing plate I > R + T !!!"];
(* ============================================== *)
Print["Параметры падающего света..."];
nUpper = 1;

lambda = {200, 800, 100, "λ", nm};
fita = {0, 0, 5, "ϕ", Degree};
beta = {0, 90, 45, "β", Degree};
gamma = {0, 0, 30, "γ", Degree};
ellipt = {0, 0, 0.25, "e"};
fi = {0, 0, 1, "φ", Degree};

incidentLight = CreateIncidentRay[nUpper, lambda, fita, beta, ellipt];
OutputIncidentRayInfo[incidentLight];
(* ============================================== *)
Print["Оптические параметры толстой пластинки"];
Print["Для расчетов для различных толщин пластинки нужно поменять значение thickness"];
thickness = 1 mm;

epsFuncT[lambda_] := Module[{nVal1, nVal2, nVal3, epsRet},
  nVal1 = refrIndex$La3Ga5SiO14$Ordinary[lambda];
  nVal2 = refrIndex$La3Ga5SiO14$Ordinary[lambda];
  nVal3 = refrIndex$La3Ga5SiO14$ExtraOrdinary[lambda];
  epsRet = EpsilonFromN[nVal1, nVal2, nVal3];
  Return[N[epsRet]];
];

muT := DiagonalMatrix[{1, 1, 1}];

rhoFuncT[lambda_] := Module[{nVal1, nVal2, nVal3, rhoRet},
  rhoRet = DiagonalMatrix[{g11$La3Ga5SiO14[lambda], 0, g33$La3Ga5SiO14[lambda]}];
  Return[N[rhoRet]];
];

fiThickPlate = {0, 0, 30, Subscript["φ", "t"], Degree};
thetaThickPlate = {0, 0, 30, Subscript["θ", "t"], Degree};
psiThickPlate = {0, 0, 30, Subscript["ψ", "t"], Degree};
rotationAnglesThickPlate = {fiThickPlate, thetaThickPlate, psiThickPlate};

thickPlate = CreateThickPlate[thickness, rotationAnglesThickPlate, epsFuncT, muT, rhoFuncT];
(* ============================================== *)
Print["Оптические параметры нижней среды..."];
nLower = 1;
lowerMedia = CreateSemiInfiniteMediaFromN[nLower];
(* ============================================== *)
If[useParallelTbl == True,
  Print["Distributing definitions for parallel calculations..."];
  DistributeDefinitions[epsFunc1, epsFuncT];
];
(* ============================================== *)
Print["Создаем оптическую систему..."];
layeredSystem = CreateLayeredSystem[incidentLight, gamma, thickPlate, lowerMedia];
OutputLayeredSystem[layeredSystem];
(* ============================================== *)
Print["Производим вычисления для различных значений параметров...."];
allCalc = PerformAllCalculations[layeredSystem, FuncList, systemDescription, opts];
(* ============================================== *)
