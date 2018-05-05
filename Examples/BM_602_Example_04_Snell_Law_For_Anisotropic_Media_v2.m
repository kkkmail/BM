(* ============================================== *)
ClearAll["Global`*"];
(* ============================================== *)
PathList={"W:\\Math\\BM\\"};
BaseDir="W:\\Math\\BM\\";
BaseFileName="Example_04";
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

FuncList={StokesVectorR[1],StokesVectorR[2],StokesVectorR[3],StokesVectorR[4],RFull,TFull, StokesGammaR, StokesGammaDegreeR,StokesChiR, StokesChiDegreeR, StokesPolarizedR,XirDegree,Elr,PsiPPDegree,DeltaPPDegree,{Rx,Ry},{Tx,Ty}};

(* ============================================== *)
systemDescription="Snell Law: Active, anisotropic, absorbing, etc... semi-infinite media, NO film.";(* ============================================== *)
Print["Параметры падающего света..."];
nUpper=1; 

lambda={632.8,632.8,1,"λ",nm};
fita={65.5,66.5,1,"ϕ", Degree};
beta={0,0.5,0.5,"β", Degree};
gamma={0, 0,30,"γ",Degree};
ellipt={0,0,0.5,"e"};

incidentLight=CreateIncidentRay[nUpper,lambda,fita,beta, ellipt];
OutputIncidentRayInfo[incidentLight];
(* ============================================== *)
Print["Оптические параметры нижней среды..."];
fi={0,0,1,"φ", Degree};
theta={0,0,1,"θ", Degree};
psi={0,0,1,"ψ", Degree};
rotationAngles={fi,theta,psi};

epsLower=EpsilonFromN[2.2597,2.2597,2.4125];
muLower=DiagonalMatrix[{1,1,1}];
rhoLower=I*{{1.9*10^-3,-3.5*10^-3,0},{3.5*10^-3,1.9*10^-3,0},{0,0,-5.7*10^-3}}; 

Print["epsLower = ", epsLower // MatrixForm];
Print["muLower = ", muLower // MatrixForm];
Print["rhoLower = ", rhoLower // MatrixForm];

lowerMedia=CreateSemiInfiniteMedia[rotationAngles,epsLower,muLower,rhoLower];
(* ============================================== *)
Print["Создаем оптическую систему..."];
layeredSystem = CreateLayeredSystem[incidentLight,gamma,lowerMedia];
OutputLayeredSystem[layeredSystem];
(* ============================================== *)
Print["Производим вычисления для различных значений параметров...."];
allCalc=PerformAllCalculations[layeredSystem,FuncList, systemDescription,opts];
(* ============================================== *)