(* ============================================== *)
ClearAll["Global`*"];
(* ============================================== *)
PathList={"W:\\Math\\BM\\"};
BaseDir="W:\\Math\\BM\\";
BaseFileName="Example_06";
OutDir=BaseDir<>"Calc\\";
(* ============================================== *)
useParallelTbl=False;
Get["BerremanInit.m",Path-> PathList];
Initialize[PathList, useParallelTbl];
(* ============================================== *)
opts={BDPlotFigures -> True,UseEulerAngles->False};
(* ============================================== *)
(*
FuncList={{StokesVectorI[1],StokesVectorI[2],StokesVectorI[3],StokesVectorI[4]},{StokesVectorR[1],StokesVectorR[2],StokesVectorR[3],StokesVectorR[4]},{EltEG[1],EltEG[2],EltEG[3],EltEG[4]},{XitEGDegree[1],XitEGDegree[2],XitEGDegree[3],XitEGDegree[4]},{IFull,RFull,TFull},{Ix,Iy,Rx,Ry,Tx,Ty},{Eli,Elr,Elt},{XiiDegree, XirDegree,XitDegree},{Sin2Xii,Sin2Xir,Sin2Xit}};
*)
(*
FuncList={StokesVectorR[1],StokesVectorR[2],StokesVectorR[3],StokesVectorR[4],RFull,TFull, StokesGammaR, StokesGammaDegreeR,StokesChiR, StokesChiDegreeR, StokesPolarizedR,XirDegree,Elr,PsiPPDegree,DeltaPPDegree,{Rx,Ry},{Tx,Ty}};
*)
FuncList={RFull,TFull,{Rx,Ry},{Tx,Ty}};

(* ============================================== *)
systemDescription="One Layer biaxial thin film between two semi-infinite media.";
(* ============================================== *)
Print["Параметры падающего света..."];
nUpper=1; 

lambda={200,1000,400,"λ",nm};fita={0,75,15,"ϕ", Degree};beta={0,0,45,"β", Degree};gamma={0, 0,30,"γ",Degree};
ellipt={0,0,0.5,"e"};

incidentLight=CreateIncidentRay[nUpper,lambda,fita,beta, ellipt];
OutputIncidentRayInfo[incidentLight];
(* ============================================== *)
Print["Оптические параметры первого тонкого слоя."];
fiLayer1={0, 0, 30,Subscript["φ","1"], Degree};thetaLayer1={0,0,30,Subscript["θ","1"],Degree};psiLayer1={0,0,30,Subscript["ψ","1"], Degree};
rotationAnglesLayer1={fiLayer1,thetaLayer1,psiLayer1};

thicknessLayer1={100,100,10,"h",nm};

epsLayer1=EpsilonFromN[1.46];
Print["epsLayer1 = ", epsLayer1 // MatrixForm];

layer1=CreateFilm[thicknessLayer1,rotationAnglesLayer1,epsLayer1];
(* ============================================== *)
Print["Оптические параметры нижней среды..."];
fi={0,0,1,"φ", Degree};
theta={0,0,1,"θ", Degree};
psi={0,0,1,"ψ", Degree};
rotationAngles={fi,theta,psi};
(*
nLower=1.5;
lowerMedia=CreateSemiInfiniteMediaFromN[nLower];
*)
epsLower=EpsilonFromN[3.87+I*0.0165];
Print["epsLower = ", epsLower // MatrixForm];

(*
muLower=DiagonalMatrix[{1,1,1}];
rhoLower=I*DiagonalMatrix[{0,0,0}];
Print["muLower = ", muLower // MatrixForm];
Print["rhoLower = ", rhoLower // MatrixForm];
*)
(* lowerMedia=CreateSemiInfiniteMedia[rotationAngles,epsLower,muLower,rhoLower];  *)
lowerMedia=CreateSemiInfiniteMedia[rotationAngles,epsLower];
(* Print ["lowerMedia = ", lowerMedia]; *)
(* ============================================== *)
Print["Создаем оптическую систему..."];
layeredSystem = CreateLayeredSystem[incidentLight,gamma,layer1,lowerMedia];
OutputLayeredSystem[layeredSystem];
(* ============================================== *)
Print["Производим вычисления для различных значений параметров...."];
allCalc=PerformAllCalculations[layeredSystem,FuncList, systemDescription,opts];
(* ============================================== *)

