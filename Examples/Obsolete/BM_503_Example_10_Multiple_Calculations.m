(* ============================================== *)
ClearAll["Global`*"];
(* ============================================== *)
PathList={"W:\\Math\\BM\\"};
BaseDir="W:\\Math\\BM\\";
BaseFileName="Example_10";
OutDir=BaseDir<>"Calc\\";
(* ============================================== *)
useParallelTbl=False;
Get["BerremanInit.m",Path-> PathList];
Initialize[PathList, useParallelTbl];
(* ============================================== *)
(* thk = 0 - считает границу 2-х полубесконечных сред;
все последующие thk (i)-считает однослойную пленку толщины thk (i) на подложке с n = n2,
оптич const пленки задаются eps1 и RoSubstr *)
(* ============================================== *)
OutputCopyright[];
time1=SessionTime[];
(* ==============================================*)
opts={UseThickLastLayer->False,PrintTimeEstimate->False,RotateAll->True,PrintCalculationProgress->False,NoOfAveragingPoints->1,TransmittedAnalyzerAngle->0 Degree,TransmittedAnalyzerParallelAmplitude->1,TransmittedAnalyzerCrossedAmplitude->0,ReflectedAnalyzerAngle->0 Degree,ReflectedAnalyzerParallelAmplitude->1,ReflectedAnalyzerCrossedAmplitude->0,AnalyzerAngleAbsoluteValue->False,AbsoluteAzimuth->False,EigenValueIndex->4, AddOnEigenValuesSort -> False,ChopTolerance->10^-5};
(* ==============================================*)
(* Options for 3D (plotOpts) and 2D plots (plotOpts2D) *)
blnRotatePlot=True;
plotOpts={PlotPoints->25,PlotRange->All (* , ColorFunctionFunction[{x,y,z},Hue[.65(1-z)]], ColorFunctionScaling{True,True,True} *) };
plotOpts2D={PlotPoints->25,Method -> {PlotDivision->5},PlotRange->All (* ,PlotStyle{{Thickness[0.009],Hue[0.7]}} *)};
(* ==============================================*)
(* n1Arr - Array of refractive indices for upper media *)
n1Arr={1.0 (*, 1.5, 2.0 *)};n1ArrLen=Length[n1Arr];
Print["n1Arr = ", n1Arr];

(* n2Arr - Array of refractive indices for lower media media EXCEPT the case of zero thickness, in which case film properties are used to initialize the semi-infinite lower media... *)
n2Arr={1.0, 1.5, 2.0};n2ArrLen=Length[n2Arr];
Print["n2Arr = ", n2Arr];

(* nOut is the refractive index of the output media when we solve for the film on a thick substrate plate (UseThickLastLayer  True). In which case h2 is the thickness of the substrate plate...*)
nOut=1.0;h2=1 mm;

(*FuncList={DetM2,M2EValRe[1],M2EValIm[1],M2EValRe[2],M2EValIm[2],M2EValRe[3],M2EValIm[3],M2EValRe[4],M2EValIm[4], Rx,Ry ,XirDegree,ElrDegree, XitDegree,EltDegree,RFull,Ty,Tx,TFull,IFull};
*)

FuncList={(* {EltEG[1],EltEG[2],EltEG[3],EltEG[4]},{XitEGDegree[1],XitEGDegree[2],XitEGDegree[3],XitEGDegree[4]}, *){IFull,RFull,TFull},{Ix,Iy,Rx,Ry,Tx,Ty},{Eli,Elr,Elt},{XiiDegree, XirDegree,XitDegree},{Sin2Xii,Sin2Xir,Sin2Xit}};

lambda={630,630,10,"λ",nm};fita={0,89,5,"ϕ",Degree};beta={0,0,90,"β",Degree};gamm={0,90,30,"γ",Degree};ellipt={0,0,0.5,"e"};

fi={0,0,20,"φ",Degree};theta={0,0,20,"θ",Degree};psi={0,0,20,"ψ",Degree};
VarList=VarListNew[{lambda,fita,beta,gamm,ellipt},{fi,theta,psi}];
(* ==============================================*)
fi1={0,0,1,Subscript["φ","1"],Degree};theta1={0,0,1,Subscript["θ","1"],Degree};psi1={0,0,1,Subscript["ψ","1"],Degree};
(* ==============================================*)

thk0={0,0,1,"h",nm};
thk1={400,400,1,"h",nm};
thk2={500,500,1,"h",nm};
thk3={1000,1000,1,"h",nm};
thk4={300,300,1,"h",nm};
thk5={600,1500,1,"h",nm};
thk6={1000,1000,1,"h",nm};

thkArr={thk0,thk1 (*,thk2,thk3,thk4,thk5,thk6*)};

thkArrLen=Length[thkArr];
(* ==============================================*)
strDescr0="No Description.";

strDescrArr={strDescr0,strDescr0,strDescr0};
(* ==============================================*)
Print["Building Epsilon Array..."];

(* ... with rotation ...*)
eps1=EpsilonFromN[1.50,1.75,1.50];
rotEps=RotationNew[0,0 Degree,0,UseEulerAngles->True];
eps1rot=Transform[eps1,rotEps];
Print["eps1 = ",MatrixForm[N[eps1]],",","eps1_rot = ",MatrixForm[N[eps1rot]]];
eps1 = eps1rot;

(* ... without rotation... *)
eps2=EpsilonFromN[1.50,1.50,1.50];
Print["eps1 = ",MatrixForm[N[eps1]]];

(* Array of Epsilons *)
epsArr={eps1, eps2};
epsArrLen=Length[epsArr];
Print ["epsArr = ", epsArr];

(* ==============================================*)
(* Building Ro and Mu (same for all calculations so far). *)
Print["Building Ro and Mu..."];
RoSubstr=I*DiagonalMatrix[{0.02,0.02,0.02}];
rotRo=RotationNew[0,0 Degree,0,UseEulerAngles->True];
RoSubstr=Transform[RoSubstr,rotRo];
Print["RoSubstr = ",I*MatrixForm[Im[RoSubstr]]];

MuSubstr=DiagonalMatrix[{1.0,1.0,1.0}];
rotMuSubstr=RotationNew[0,0 Degree,0,UseEulerAngles->True];
MuSubstr=Transform[MuSubstr,rotMuSubstr];
Print["MuSubstr = ",MatrixForm[N[MuSubstr]]];

(* ==============================================*)
inclght=IncidentLightNew[630 nm,0,0,0];
(* ==============================================*)
fNamePrefix="F_UHI012_6";
fNamePrefixNoFilm="F_UHI012_6NF";
fNamePrefixNoFilmIsotropic="F_UHI012_6NFI";
divLine="===========================================================================";
(* ==============================================*)

coll=CalcCollectionNew[OutDir,BaseFileName,"No Description so far."];
Print["Plotting figures..."];

Do[
Do[
Film=FilmNew[];
VarList=VarListNew[{lambda,fita,beta,gamm,ellipt},{fi,theta,psi}];
VarListAddLayer[VarList,{fi1,theta1,psi1,thkArr[[thkIter]]}];
FilmAddLayer[Film,FilmLayerNew[0,epsArr[[epsIter]],MuSubstr,RoSubstr]];

Do[
Do[
n1=n1Arr[[n1Iter]];
n2=n2Arr[[n2Iter]];
(* ==============================================*)
If[thkArr[[thkIter,1]]===0,
If[n2Iter===1,
(* ==============================================*)
(* Calculation for infinite substrate with the same properties as the film *)
Media=MediaNew[n1,n2,gamm,Film,strDescrArr[[epsIter]],nOut,h2,epsArr[[epsIter]],MuSubstr,RoSubstr];
fName=MakeFileName[fNamePrefixNoFilm,Media,thkArr[[thkIter,1]],opts];
Calc=CalcNew[Media,VarList,FuncList,strDescrArr[[epsIter]],opts];
Print[divLine];
Print["Thick anisotropic substrate, no film. n1 = ",n1];
Print["EpsSubstr = ",MatrixForm[N[epsArr[[epsIter]]]]];
Print["RoSubstr = ",MatrixForm[N[RoSubstr]]];
Print["MuSubstr = ",MatrixForm[N[MuSubstr]]];
(*MMM1=MMM[MediaUpperEpsilon[Media],MediaUpperMu[Media],MediaUpperRo[Media],MediaUpperRoT[Media],lambda,fita,n1];
MMM2=MMM[MediaLowerEpsilon[Media],MediaLowerMu[Media],MediaLowerRo[Media],MediaLowerRoT[Media],lambda,fita,n1];*)
CalcPlot[Calc,plotOpts2D];
CalcPlot3D[Calc,blnRotatePlot,plotOpts];
CalcCollectionAddCalc[coll,Calc,fName];
];,
(* ==============================================*)
(* Calculation for the film on an isotropic infinite substrate *)
Media=MediaNew[n1,n2,gamm,Film,strDescrArr[[epsIter]],nOut];
fName=MakeFileName[fNamePrefix,Media,thkArr[[thkIter,1]],opts];
Calc=CalcNew[Media,VarList,FuncList,strDescrArr[[epsIter]],opts];
Print[divLine];
Print["Thick isotropic substrate. n1 = ",n1,", n2 = ",n2];
Print["EpsSubstr = ",MatrixForm[N[DiagonalMatrix[{n2*n2, n2*n2, n2*n2}]]]];
Print["  "];
Print["Film thickness = ",thkArr[[thkIter,1]]];
Print["Eps = ",MatrixForm[N[epsArr[[epsIter]]]]];
Print["Ro = ",MatrixForm[N[RoSubstr]]];
Print["Mu = ",MatrixForm[N[MuSubstr]]];
CalcPlot[Calc,plotOpts2D];
CalcPlot3D[Calc,blnRotatePlot,plotOpts];
CalcCollectionAddCalc[coll,Calc,fName];
(* ==============================================*)
];,{n1Iter,n1ArrLen}
];,{n2Iter,n2ArrLen}
];,{epsIter,epsArrLen}
];,{thkIter,thkArrLen}
];

time2=SessionTime[]; timeused=time2-time1; Print["Time used: ", timeused];
(* ==============================================*)
Print["Starting Calculations..."];
(*CalcCollectionPlot3D[coll,True,plotOpts];
CalcCollectionPerform[coll];
CalcCollectionSave[coll,True];*)
(* ==============================================*)
time3=SessionTime[]; timeused=time3-time2; Print["Time used: ", timeused];timeusedT=time3-time1;Print["Total time used: ", timeusedT];


