(* ==============================================*)
Print["См.: Goldstein_PolarizedLight_Excerpts.pdf"];
Print["     Dennis Goldstein, Polarized Light, 2nd edition, 2003, главы 4 и 5, ISBN:0-8247-4053-X"];
(* ==============================================*)
ClearAll["Global`*"];
(* ==============================================*)
PathList={"W:\\Math\\BM\\"};
BaseDir="W:\\Math\\BM\\";
BaseFileName="Example_00a";OutDir=BaseDir<>"Calc\\";
(* ==============================================*)
useParallelTbl=False;
Get["BerremanInit.m",Path-> PathList];
Initialize[PathList, useParallelTbl];
(* ==============================================*)
(*
FuncList={{StokesVectorI[1],StokesVectorI[2],StokesVectorI[3],StokesVectorI[4]},{StokesVectorR[1],StokesVectorR[2],StokesVectorR[3],StokesVectorR[4]},{EltEG[1],EltEG[2],EltEG[3],EltEG[4]},{XitEGDegree[1],XitEGDegree[2],XitEGDegree[3],XitEGDegree[4]},{IFull,RFull,TFull},{Ix,Iy,Rx,Ry,Tx,Ty},{Eli,Elr,Elt},{XiiDegree, XirDegree,XitDegree},{Sin2Xii,Sin2Xir,Sin2Xit}};
*)

FuncList={StokesVectorR[1],StokesVectorR[2],StokesVectorR[3],StokesVectorR[4],RFull,TFull, StokesGammaR, StokesGammaDegreeR,StokesChiR, StokesChiDegreeR, StokesPolarizedR,XirDegree,Elr,PsiPPDegree,DeltaPPDegree,{Rx,Ry},{Tx,Ty}};
(* ==============================================*)
opts={BDPlotFigures -> True,PrintElementDescription->True};
(* ==============================================*)
systemDescription="...";
(* ==============================================*)
Print["Параметры падающего света..."];
nUpper=1; 

lambda={600,600,1,"λ",nm};fita={0,85,5,"ϕ", Degree};beta={0,90,45,"β", Degree};gamma={0, 0,30,"γ",Degree};
ellipt={0,0,0.5,"e"};

incidentLight=CreateIncidentRay[nUpper,lambda,fita,beta, ellipt];
OutputIncidentRayInfo[incidentLight];
(* ==============================================*)

(* ==============================================*)
