(* ============================================== *)
ClearAll["Global`*"];
(* ============================================== *)
PathList={"W:\\Math\\BM\\"};
BaseDir="W:\\Math\\BM\\";
BaseFileName="Example_03a";
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
systemDescription="Snell Law: Anisotropic thick plate between air, NO film.";
Print["!!! For absorbing plate I > R + T !!!"];
(* ============================================== *)
Print["Параметры падающего света..."];
nUpper=1;

lambda={600,600,1,"λ",nm};fita={0,85,85,"ϕ", Degree};beta={0,0,45,"β", Degree};gamma={0, 0,30,"γ",Degree};
ellipt={-1,1,0.5,"e"};

incidentLight=CreateIncidentRay[nUpper,lambda,fita,beta, ellipt];
OutputIncidentRayInfo[incidentLight];
(* ============================================== *)
Print["Для расчетов для различных толщин пластинки нужно поменять значение thickness"];
thickness=1 mm;

fiPlate={0, 0, 30,Subscript["φ","1"], Degree};thetaPlate={0,0,30,Subscript["θ","1"],Degree};psiPlate={0,0,30,Subscript["ψ","1"], Degree};
rotationAnglesPlate={fiPlate,thetaPlate,psiPlate};

epsPlate=EpsilonFromN[1.50,2.00,1.75];
muPlate=DiagonalMatrix[{1,1,1}];
rhoPlate=I*{{1.9*10^-3,-3.5*10^-3,0},{3.5*10^-3,1.9*10^-3,0},{0,0,-5.7*10^-3}};

thickPlate=CreateThickPlate[thickness,rotationAnglesPlate,epsPlate,muPlate,rhoPlate];
(* ============================================== *)
Print["Оптические параметры нижней среды..."];
nLower=1;
lowerMedia=CreateSemiInfiniteMediaFromN[nLower];
(* ============================================== *)
Print["Создаем оптическую систему..."];
layeredSystem = CreateLayeredSystem[incidentLight,gamma,thickPlate,lowerMedia];
OutputLayeredSystem[layeredSystem];
(* ============================================== *)
Print["Производим вычисления для различных значений параметров...."];
allCalc=PerformAllCalculations[layeredSystem,FuncList, systemDescription,opts];
(* ============================================== *)