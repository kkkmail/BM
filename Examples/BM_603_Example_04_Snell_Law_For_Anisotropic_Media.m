(* ============================================== *)
ClearAll["Global`*"];
(* ============================================== *)
BaseDir = "C:\\GitHub\\BM\\";
PathList = {BaseDir <> "Kernel\\"};
OutDir = BaseDir <> "Calc\\";
(* ============================================== *)
BaseFileName = "Example_04";
(* ============================================== *)
useParallelTbl = False;
Get["BerremanInit.m", Path -> PathList];
InitializeBM[PathList, useParallelTbl];
(* ============================================== *)
opts =
    {
      BDPlotFigures -> True,
      UseEulerAngles -> False
    };
(* ============================================== *)
FuncList =
    {
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
systemDescription = "Snell Law: Active, anisotropic, absorbing, etc... semi-infinite media, NO film.";
Print["!!! For absorbing plate I > R + T !!!"];
(* ============================================== *)
Print["Параметры падающего света..."];
nUpper = 1;

lambda = {600, 600, 1, "λ", nm};
fita = {0, 85, 85, "ϕ", Degree};
beta = {0, 0, 30, "β", Degree};
gamma = {0, 0, 30, "γ", Degree};
ellipt = {0, 0, 0.5, "e"};

incidentLight = CreateIncidentRay[nUpper, lambda, fita, beta, ellipt];
OutputIncidentRayInfo[incidentLight];
(* ============================================== *)
Print["Оптические параметры нижней среды..."];

fiLower = {0, 0, 30, Subscript["φ", "1"], Degree};
thetaLower = {0, 0, 30, Subscript["θ", "1"], Degree};
psiLower = {0, 0, 30, Subscript["ψ", "1"], Degree};
rotationAnglesLower = {fiLower, thetaLower, psiLower};

epsLower =
    {
      {2.471 + I * 0.0432, -0.000357 + I * 0.002945, 0},
      {-0.000357 + I * 0.002945, 2.465 + I * 0.0425, 0},
      {0, 0, 2.51 + I * 0.048}
    };

muLower = DiagonalMatrix[{1, 2, 1}];
rhoLower = I * 0.1 * DiagonalMatrix[{2, 1, 1}];

Print["epsLower = ", epsLower // MatrixForm];
Print["muLower = ", muLower // MatrixForm];
Print["rhoLower = ", rhoLower // MatrixForm];

lowerMedia = CreateSemiInfiniteMedia[rotationAnglesLower, epsLower, muLower, rhoLower];
(* ============================================== *)
Print["Создаем оптическую систему..."];
layeredSystem = CreateLayeredSystem[incidentLight, gamma, lowerMedia];
OutputLayeredSystem[layeredSystem];
(* ============================================== *)
Print["Производим вычисления для различных значений параметров...."];
allCalc = PerformAllCalculations[layeredSystem, FuncList, systemDescription, opts];
(* ============================================== *)
