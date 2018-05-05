(* ============================================== *)
ClearAll["Global`*"];
(* ============================================== *)
PathList = {"C:\\GitHub\\BM\\Kernel\\"};
BaseDir = "C:\\GitHub\\BM\\Kernel\\";
BaseFileName = "Example_09";
OutDir = BaseDir <> "Calc\\";
(* ============================================== *)
useParallelTbl = False;
Get["BerremanInit.m", Path -> PathList];
InitializeBM[PathList, useParallelTbl];
(* ============================================== *)
(*
opts =
    {
      UseThickLastLayer -> False,
      PrintTimeEstimate -> False,
      RotateAll -> True,
      NoOfAveragingPoints -> 3,
      TransmittedAnalyzerAngle -> 0 Degree,
      TransmittedAnalyzerParallelAmplitude -> 1,
      TransmittedAnalyzerCrossedAmplitude -> 0,
      ReflectedAnalyzerAngle -> 0 Degree,
      ReflectedAnalyzerParallelAmplitude -> 1,
      ReflectedAnalyzerCrossedAmplitude -> 0,
      AnalyzerAngleAbsoluteValue -> False,
      AbsoluteAzimuth -> False,
      PrintCalculationProgress -> False,
      PrintFunctionDebugInfo -> False,
      PrintCommonDebugInfo -> False,
      PrintCommonDebugInfoLevel -> 4,
      ChopTolerance -> 10^-5
    };
*)

opts =
    {
      BDPlotFigures -> True,
      UseEulerAngles -> False
    };
(* ============================================== *)
(* FuncList={EpsComponent[1,1,1,1],{EpsComponent[1,1,2,2],EpsComponent[1,1,3,3]},{EltEG[1],EltEG[2],EltEG[3],EltEG[4]},{XitEGDegree[1],XitEGDegree[2],XitEGDegree[3],XitEGDegree[4]},{IFull,TFull},RFull,{Ix,Tx},{Iy,Ty},{Rx,Ry},{Eli,Elr,Elt},{XiiDegree, XirDegree,XitDegree},{Sin2Xii,Sin2Xir,Sin2Xit}}; *)

FuncList =
    {
      EpsComponent[1, 1, 1, 1],
      {EpsComponent[1, 1, 2, 2], EpsComponent[1, 1, 3, 3]}
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
systemDescription = "TODO - FIX: One Layer biaxial thin film on slighlty absorbing substrate plate.";
Print["!!! For absorbing plate I > R + T !!!"];
(* ============================================== *)
Print["Параметры падающего света..."];
nUpper = 1;

lambda = {400, 800, 100, "λ", nm};
fita = {60, 60, 5, "ϕ", Degree};
beta = {0, 0, 45, "β", Degree};
gamm = {0, 0, 30, "γ", Degree};
ellipt = {0, 1, 0.25, "e"};
fi = {0, 0, 1, "φ", Degree};

incidentLight = CreateIncidentRay[nUpper, lambda, fita, beta, ellipt];
OutputIncidentRayInfo[incidentLight];
(* ============================================== *)
kcoeff = 2.5;
lambdaNullcoeff = 100 nm;
(* ============================================== *)
epsFunc[lamd_] := Module[{nVal, epsRet},
  nVal = Sqrt[1 + kcoeff * lamd^2 / (lamd^2 - lambdaNullcoeff^2)];
  epsRet = EpsilonFromN[nVal];
  Return[N[epsRet]];
];

If[useParallelTbl == True,
  Print["Distributing definitions for parallel calculations..."];
  DistributeDefinitions[epsFunc, kcoeff, lambdaNullcoeff];
];
(* ============================================== *)
Print["Оптические параметры первого тонкого слоя."];
thicknessLayer1 = {75, 75, 10, "h", nm};

fiLayer1 = {0, 0, 30, Subscript["φ", "1"], Degree};
thetaLayer1 = {0, 0, 30, Subscript["θ", "1"], Degree};
psiLayer1 = {0, 0, 30, Subscript["ψ", "1"], Degree};
rotationAnglesLayer1 = {fiLayer1, thetaLayer1, psiLayer1};

epsLayer1 = EpsilonFromN[1.50, 2.00, 1.75];
Print["epsLayer1 = ", epsLayer1 // MatrixForm];

layer1 = CreateFilm[thicknessLayer1, rotationAnglesLayer1, epsLayer1];
(* ============================================== *)
Print["Оптические параметры толстой пластинки"];
Print["Для расчетов для различных толщин пластинки нужно поменять значение thickness"];
nSubstr = 1.5;
kSubstr = 3 * 10^-6;
thickness = 1 mm;
thickPlate = CreateThickPlateFromN[thickness, nSubstr + I * kSubstr];
(* ============================================== *)
Print["Оптические параметры нижней среды..."];
nLower = 1;
lowerMedia = CreateSemiInfiniteMediaFromN[nLower];
(* ============================================== *)
Print["Создаем оптическую систему..."];
layeredSystem = CreateLayeredSystem[incidentLight, gamma, layer1, thickPlate, lowerMedia];
OutputLayeredSystem[layeredSystem];
(* ============================================== *)
Print["Производим вычисления для различных значений параметров...."];
allCalc = PerformAllCalculations[layeredSystem, FuncList, systemDescription, opts];
(* ============================================== *)

(*
Film = FilmNew[];
(* ============================================== *)
VarListAddLayer[VarList, {fi1, theta1, psi1, thk1}];FilmAddLayer[Film, FilmLayerNew[0, epsFunc]];
(* ============================================== *)
EpsSubstr = EpsilonFromN[n2];
Media = MediaNew[n1, n2, gamm, Film, "Nothing", nOut, h2, EpsSubstr];
*)
(* ============================================== *)
(*
OutputCopyright[];
time1 = SessionTime[];

Calc = CalcNew[Media, VarList, FuncList, "One Layer biaxial thin film on slighlty absorbing substrate plate.", opts];
coll = CalcCollectionNew[OutDir, BaseFileName, "No Description so far."];
CalcCollectionAddCalc[coll, Calc];
CalcCollectionPerform[coll];
CalcCollectionSave[coll, True];
CalcCollectionSave[coll];

time2 = SessionTime[]; timeused = time2 - time1; Print["Time used: ", timeused];

plotOpts = {PlotPoints -> 25, Method -> {PlotDivision -> 3}, PlotRange -> All};
plotOpts3D = {PlotPoints -> 25, PlotRange -> All};

Print["Plotting epsFunc ..."];
Plot[epsFunc[lamb nm][[1, 1]], {lamb, 400, 800}, Frame -> True, GridLines -> Automatic, PlotStyle -> {Thickness[0.005]}]

Print["Plotting figures..."];
CalcPlot[Calc, plotOpts];
CalcPlot3D[Calc, False, plotOpts3D];
time3 = SessionTime[]; timeused = time3 - time2; Print["Time used: ", timeused];timeusedT = time3 - time1;Print["Total time used: ", timeusedT];
*)
