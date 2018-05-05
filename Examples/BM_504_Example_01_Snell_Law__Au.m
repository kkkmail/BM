(* ============================================== *)
ClearAll["Global`*"];
(* ============================================== *)
PathList={"W:\\Math\\BM\\"};
BaseDir="W:\\Math\\BM\\";
BaseFileName="Example_01";
OutDir=BaseDir<>"Calc\\";
(* ============================================== *)
useParallelTbl=False;
Get["BerremanInit.m",Path-> PathList];
Initialize[PathList, useParallelTbl];
(* ============================================== *)
opts={UseThickLastLayer->False,PrintTimeEstimate->False,RotateAll->True,NoOfAveragingPoints->5,TransmittedAnalyzerAngle->0 Degree,TransmittedAnalyzerParallelAmplitude->1,TransmittedAnalyzerCrossedAmplitude->0,ReflectedAnalyzerAngle->0 Degree,ReflectedAnalyzerParallelAmplitude->1,ReflectedAnalyzerCrossedAmplitude->0,AnalyzerAngleAbsoluteValue->False,AbsoluteAzimuth->False,CalculateBeta0and90->True, UseSolveInSolutionNew -> False ,PrintCalculationProgress -> False,PrintFunctionDebugInfo -> False,PrintCommonDebugInfo -> False,PrintCommonDebugInfoLevel -> 4};
(*==============================================*)
n1=1;
n2=0.35+I*2.45;
nOut=n2;
h2=1 mm;
(*==============================================*)
(*
FuncList={PsiPPDegree,DeltaPPDegree,{EltEG[1],EltEG[2],EltEG[3],EltEG[4]},{XitEGDegree[1],XitEGDegree[2],XitEGDegree[3],XitEGDegree[4]},{IFull,RFull,TFull},{Ix,Iy,Rx,Ry,Tx,Ty},{Eli,Elr,Elt},{XiiDegree, XirDegree,XitDegree},{Sin2Xii,Sin2Xir,Sin2Xit}};
*)
FuncList={PsiPPDegree,DeltaPPDegree,{Rx,Ry,Tx,Ty}};
(*==============================================*)
lambda={600,600,1,"λ",nm};
fita={0,85,5,"ϕ", Degree};
beta={0,0,45,"β", Degree};
gamm={0, 0,30,"γ",Degree};
ellipt={0,1,0.5,"e"};

fi={0,0,1,"φ", Degree};theta={0,0,1,"θ", Degree};psi={0,0,1,"ψ", Degree};
VarList=VarListNew[{lambda, fita, beta, gamm, ellipt},{fi, theta, psi}];
(*==============================================*)
Film=FilmNew[];
Media=MediaNew[n1,n2,gamm,Film,"Nothing",nOut,h2];
(*==============================================*)

OutputCopyright[];
time1=SessionTime[];

Calc=CalcNew[Media,VarList,FuncList,"Snell Law: Two semi-infinite isotropic media, NO film.",opts];
Print[GetRotationInfo[Calc]];
Print[strSeparator];

coll=CalcCollectionNew[OutDir,BaseFileName,"No Description so far."];
CalcCollectionAddCalc[coll,Calc];
CalcCollectionPerform[coll];
CalcCollectionSave[coll,True];
CalcCollectionSave[coll];
time2=SessionTime[]; timeused=time2-time1; Print["Time used: ", timeused];

Print["Plotting figures..."];
plotOpts = {PlotPoints ->25, PlotRange -> All (*, ColorFunction  "Pastel" *)};
CalcPlot3D[Calc,True,plotOpts];

plotOpts2D = {PlotPoints ->25, PlotRange -> All, MaxRecursion -> 3};
CalcPlot[Calc,plotOpts2D];

time3=SessionTime[]; timeused=time3-time2; Print["Time used: ", timeused];timeusedT=time3-time1; Print["Total time used: ", timeusedT];
(*==============================================*)
