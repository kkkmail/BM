(* ============================================== *)
ClearAll["Global`*"];
(* ============================================== *)
PathList={"W:\\Math\\BM\\"};
BaseDir="W:\\Math\\BM\\";
BaseFileName="Example_05";
OutDir=BaseDir<>"Calc\\";
(* ============================================== *)
useParallelTbl=False;
Get["BerremanInit.m",Path-> PathList];
Initialize[PathList, useParallelTbl];
(* ============================================== *)
opts={UseThickLastLayer -> True,  PrintTimeEstimate -> True, RotateAll-> True,NoOfAveragingPoints -> 5,TransmittedAnalyzerAngle -> 0 Degree,TransmittedAnalyzerParallelAmplitude -> 1,TransmittedAnalyzerCrossedAmplitude -> 0, ReflectedAnalyzerAngle -> 0 Degree, ReflectedAnalyzerParallelAmplitude -> 1,ReflectedAnalyzerCrossedAmplitude -> 0,AnalyzerAngleAbsoluteValue -> False,AbsoluteAzimuth->False}; 
(* ============================================== *)
OutputThickLastLayerInfo[];

n1=1; n2=1.5;nOut=1;

Print["Для расчетов для различных толщин пластинки нужно поменять значение h2"];
h2=10 mkm;

FuncList={{EltEG[1],EltEG[2],EltEG[3],EltEG[4]},{XitEGDegree[1],XitEGDegree[2],XitEGDegree[3],XitEGDegree[4]},{IFull,RFull,TFull},{Ix,Iy,Rx,Ry,Tx,Ty},{Eli,Elr,Elt},{XiiDegree, XirDegree,XitDegree},{Sin2Xii,Sin2Xir,Sin2Xit}};

lambda={600,600,1,"λ",nm};fita={0,85,5,"ϕ", Degree};beta={0,90,45,"β", Degree};gamm={0, 0,30,"γ",Degree};ellipt={0,0,0.5,"e"};
fi={0,0,1,"φ", Degree};theta={0,0,1,"θ", Degree};psi={0,0,1,"ψ", Degree};
VarList=VarListNew[{lambda, fita, beta, gamm, ellipt},{fi, theta, psi}];
(* ============================================== *)
Film = FilmNew[];

(*
EpsSubstr=EpsilonFromN[1.50, Complex[2.00,0.0001], Complex[1.75,0.005]];
MuSubstr=DiagonalMatrix[{1,1.1,1}];
RoSubstr=I * 0.01*DiagonalMatrix[{2,1,1}];
*)

EpsSubstr={{2.471+I*0.0432,-0.000357+I*0.002945,0},{-0.000357+I*0.002945,2.465+I*0.0425,0},{0,0,2.51+I*0.048}};
MuSubstr=DiagonalMatrix[{1,1,1}];
RoSubstr=DiagonalMatrix[{0,0,0}]; 

Media = MediaNew[n1,n2,gamm,Film, "Nothing",nOut,h2,EpsSubstr ,MuSubstr,RoSubstr];
(* ============================================== *)

OutputCopyright[];
time1=SessionTime[];

Print[""];
Print["!!! For absorbing plate I > R + T !!!"];
Print[""];

Calc=CalcNew[Media, VarList, FuncList, "Snell Law: Active, anisotropic, absorbing, etc... thick plate between air, NO film.",opts];
coll=CalcCollectionNew[OutDir, BaseFileName, "No Description so far."];
CalcCollectionAddCalc[coll, Calc];
CalcCollectionPerform[coll];
CalcCollectionSave[coll, True];
CalcCollectionSave[coll];

time2=SessionTime[]; timeused=time2-time1; Print["Time used: ", timeused];

plotOpts = {PlotPoints ->25, PlotRange -> All};
Print["Plotting figures..."];
CalcPlot3D[Calc,True,plotOpts];

plotOpts2D = {PlotPoints ->25, PlotRange -> All, MaxRecursion -> 3};
CalcPlot[Calc,plotOpts2D];

time3=SessionTime[]; timeused=time3-time2; Print["Time used: ", timeused];timeusedT=time3-time1;Print["Total time used: ", timeusedT];
