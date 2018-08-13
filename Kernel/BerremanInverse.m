(* :Author: Konstantin K.Konstantinov *)
(* :Summary: This module defines inverse task functionality for Berreman Method. *)
(* :Copyright: K^3, 2001 - 2018 *)
(* :Version: Revision: 6.03.001, Date: 2018/05/05 *)
(* :Mathematica Version: 11.2 *)
(*==============================================*)
strUseAllOutputColumns = "ALL";
BIIMTOLERANCE = 10^-20;
BIEPSTYPESTDTWOSTEPTRAPSPARENT = 1;
BIEPSTYPESTDTWOSTEPABSORBING = 101;
(*==============================================*)
Options[BerremanInverse] =
    {
      BerremanInverseVersion -> 6.03,
      LambdaMultiplier -> (1 / 10^9),
      AngleMultiplier -> Degree,
      DataStartRow -> 2,
      LambdaColumn -> 3,
      FitaColumn -> 4,
      BetaColumn -> 5,
      GammaColumn -> 6,
      EllipticityColumn -> 7,
      OutputStartColumn -> 10,
      OutputColumns ->
          {
            {strUseAllOutputColumns},
            {strUseAllOutputColumns}
          },
      MinizationMultiplier -> 10^9,
      UseStepMonitor -> True,
      UseEvaluationMonitor -> False,
      UseNMinimize -> False,
      EpsilonType -> BIEPSTYPESTDTWOSTEPTRAPSPARENT,
      AngleRoundingMultiplier -> 100,
      UseArbitraryPhase -> False,
      EpsilonLowerBoundary -> 1.9,
      EpsilonUpperBoundary -> 4,
      EpsilonNonDiagLowerBoundary -> -0.4,
      EpsilonNonDiagUpperBoundary -> 0.4,
      MFHlpDataSet -> 1,
      FitaTolerance -> (1 / 10^4),
      EpsilonIMLowerBoundary -> -BIIMTOLERANCE,
      EpsilonIMUpperBoundary -> 4,
      EpsilonIMNonDiagLowerBoundary -> -1,
      EpsilonIMNonDiagUpperBoundary -> 1,
      MFHlpAbsorbingEpsilon -> False,
      EpsilonStart -> EpsilonFromN[1.5, 1.9, 1.7],
      PrintEpsilon -> True,
      EvaluationMonitorCount -> 1,
      StepMonitorCount -> 1,
      EvaluationMonitorSecondStepCount -> 1,
      StepMonitorSecondStepCount -> 1
    };
(*==============================================*)
(*EpsilonType:0-standard transparent (6 variables)*)
(*==============================================*)
ComplexUnitStepAND[cplxNum_] :=
    Module[{retval},
    (*Print["num = ",cplxNum];*)
      Return[UnitStep[Re[cplxNum]] * UnitStep[Im[cplxNum]]]
    ];

ComplexUnitStepOR[cplxNum_] :=
    Module[{retval},
      retval = UnitStep[Min[Re[cplxNum]] + UnitStep[Im[cplxNum]], 1];
      (*Print["num = ",cplxNum,", retval = ",retval];*)
      Return[retval]
    ];

MFHlpSetParLimits[par_, minVal_, maxVal_, minIMVal_ : -BIIMTOLERANCE, maxIMVal_ : BIIMTOLERANCE] :=
    Module[{retval},
      retval = ComplexUnitStepOR[Complex[minVal, minIMVal] - par] + ComplexUnitStepOR[par - Complex[maxVal, maxIMVal]];
      Return[retval];

    ];

MFHlpSetEpsilonLimits[Epsilon : {{_, _, _}, {_, _, _}, {_, _, _}}, opts___] :=
    Module[{epsLB, epsUB, epsUP, epsNDLP, epsNDLB, epsNDUB, epsIMLB, epsIMUB, epsIMUP, epsIMNDLP, epsIMNDLB, epsIMNDUB, retval},
      epsLB = EpsilonLowerBoundary /. opts /. Options[BerremanInverse];
      epsUB = EpsilonUpperBoundary /. opts /. Options[BerremanInverse];
      epsNDLB = EpsilonNonDiagLowerBoundary /. opts /. Options[BerremanInverse];
      epsNDUB = EpsilonNonDiagUpperBoundary /. opts /. Options[BerremanInverse];
      epsIMLB = EpsilonIMLowerBoundary /. opts /. Options[BerremanInverse];
      epsIMUB = EpsilonIMUpperBoundary /. opts /. Options[BerremanInverse];
      epsIMNDLB = EpsilonIMNonDiagLowerBoundary /. opts /. Options[BerremanInverse];
      epsIMNDUB = EpsilonIMNonDiagUpperBoundary /. opts /. Options[BerremanInverse];
      retval = MFHlpSetParLimits[Epsilon[[1, 1]], epsLB, epsUB, epsIMLB, epsIMUB];
      retval += MFHlpSetParLimits[Epsilon[[2, 2]], epsLB, epsUB, epsIMLB, epsIMUB];
      retval += MFHlpSetParLimits[Epsilon[[3, 3]], epsLB, epsUB, epsIMLB, epsIMUB];
      retval += MFHlpSetParLimits[Epsilon[[1, 2]], epsNDLB, epsNDUB, epsIMNDLB, epsIMNDUB];
      retval += MFHlpSetParLimits[Epsilon[[1, 3]], epsNDLB, epsNDUB, epsIMNDLB, epsIMNDUB];
      retval += MFHlpSetParLimits[Epsilon[[2, 3]], epsNDLB, epsNDUB, epsIMNDLB, epsIMNDUB];

      Return[retval];
    ];

(*==============================================*)
MFHlpCounter = 0;
MFHlpDisp = 10;

MFHlp[Epsilon : {{_, _, _}, {_, _, _}, {_, _, _}}, invSol_, mfoptsInpt___] :=
    Module[{Media, sol, retval, len, incLght, lambda, fita, beta, gamm, funcLen, solArr, utll, solAvg, solArrAvg, opts, mfMult, n1, n2, nOut, h1, h2, epsSubtr, Film, Layer, MediaTrf, varList, flmLen, IsAverageArr, msDST, prtEps, avgtyp, funcArr, mfopts, ampl, elpct, useParallelTbl, solArray, inptArray},
      retval = 0;
      mfopts = Flatten[{mfoptsInpt}];

      (* Print["MFHlp::Starting..."]; *)
      (*Print[" MFHlp. Epsilon = ",Epsilon];*)(*invSol:[[1,1]]-lambda,[[1,2]]-{n1,n2,nOut},[[1,3]]-{h1,h2},[[1,4]]-inptData,invSol[[2]]-Options*)(*inptData:[[1]]-dstColLen,[[2]]-{inpt1,outp1,funcList1,funcNameList1},[[3]]-...*)(*InputVarList:InputVarList[[i,1]]-fita,InputVarList[[i,2]]-beta,InputVarList[[i,3]]-gamma,*)

      lambda = invSol[[1, 1]];
      opts = InverseSolutionOptions[invSol];

      (*Print["mfopts = ",mfopts]; Print["opts = ",opts];*)

      utll = UseThickLastLayer /. opts /. Options[BerremanDirect];
      mfMult = MinizationMultiplier /. opts /. Options[BerremanInverse];
      msDST = Max[Min[(MFHlpDataSet /. mfopts /. Options[BerremanInverse]), 2], 1];
      useParallelTbl = UseParallelTable /. Flatten[{opts}] /. Options[BerremanInit];

      (* Print["msDST = ",msDST]; *)

      prtEps = PrintEpsilon /. opts /. Options[BerremanInverse];
      len = Length[invSol[[1, 4, msDST + 1, 1]]];
      funcLen = Length[invSol[[1, 4, msDST + 1, 3]]];

      (*
      Print["invSol[[1,4,msDST+1]] = ",invSol[[1,4,msDST+1]]];
      Print["invSol[[1,4,msDST+1,1]] = ",invSol[[1,4,msDST+1,1]]];
      Print["invSol[[1,4,msDST+1,2]] = ",invSol[[1,4,msDST+1,2]]];
      Print["invSol[[1,4,msDST+1,3]] = ",invSol[[1,4,msDST+1,3]]];
      *)

      (* Print["len = ",len,", funcLen = ",funcLen]; *)

      n1 = invSol[[1, 2, 1]];
      n2 = invSol[[1, 2, 2]];
      nOut = invSol[[1, 2, 3]];
      h1 = invSol[[1, 3, 1]];
      h2 = invSol[[1, 3, 2]];

      (* Print["CHECK h2 Correctness: ",h2]; *)

      epsSubtr = EpsilonFromN[n2];
      Film = FilmNew[];
      Layer = FilmLayerNew[h1, Epsilon];
      Film = FilmAddLayer[Film, Layer];

      (*If[Round[Chop[h2*10^9]]=!=0,(*Print["Using dual layer in MFHlp..."];*)Layer=FilmLayerNew[h2,epsSubtr];Film=FilmAddLayer[Film,Layer];];*)

      flmLen = FilmLength[Film];
      varList = Table[0, {8 + 4 * flmLen}];
      IsAverageArr = Table[True, {funcLen}];
      avgtyp = AveragingType /. opts /. Options[BerremanDirect];
      If[avgtyp === BDAVGTYPESERIES, IsAverageArr = Table[False, {funcLen}]];

      (* Print["MFHlp::about to run DO"]; *)

      ampl = 1;

      If[useParallelTbl == False,
        Do[
          fita = invSol[[1, 4, msDST + 1, 1, i, 1]];
          beta = invSol[[1, 4, msDST + 1, 1, i, 2]];
          gamm = invSol[[1, 4, msDST + 1, 1, i, 3]];
          elpct = invSol[[1, 4, msDST + 1, 1, i, 4]];

          incLght = IncidentLightNew[lambda, fita, beta, n1, ampl, elpct];
          Media = MediaNew[n1, n2, gamm, Film, "MFHlp", nOut];
          varList[[1]] = lambda;
          varList[[2]] = fita;
          varList[[3]] = beta;
          varList[[4]] = gamm;
          varList[[5]] = elpct;
          varList[[12]] = h1;
          If[flmLen === 2, varList[[16]] = h2;];
          MediaTrf = TransformMedia[Media, varList, opts];

          If[utll === True,
            solAvg = Chop[SolutionAverageNew[MediaTrf, incLght, opts]];
            solArrAvg = Table[{solAvg}, {funcLen}];
            funcArr = MapThread[AveragingFunc, {invSol[[1, 4, msDST + 1, 3]], solArrAvg, IsAverageArr}];
            ,
            sol = Chop[SolutionNew[MediaTrf, incLght, opts]];
            solArr = Table[{sol}, {funcLen}];
            funcArr = MapThread[Apply, {invSol[[1, 4, msDST + 1, 3]], solArr}];
          ];

          retval += Sum[((funcArr[[j]] - invSol[[1, 4, msDST + 1, 2, i, j]])^2), {j, funcLen}];, {i, len}
        ];
        ,
      (*
      If[Mod[MFHlpCounter,MFHlpDisp]\[Equal] 0,
      Print["MFHlp::using parallel table... Length = ", len];
      ];
      MFHlpCounter++;
      *)

        inptArray = Table[0, {iii, 1, len}];

        Do[
          fita = invSol[[1, 4, msDST + 1, 1, i, 1]];
          beta = invSol[[1, 4, msDST + 1, 1, i, 2]];
          gamm = invSol[[1, 4, msDST + 1, 1, i, 3]];
          elpct = invSol[[1, 4, msDST + 1, 1, i, 4]];

          incLght = IncidentLightNew[lambda, fita, beta, n1, ampl, elpct];
          Media = MediaNew[n1, n2, gamm, Film, "MFHlp", nOut];
          varList[[1]] = lambda;
          varList[[2]] = fita;
          varList[[3]] = beta;
          varList[[4]] = gamm;
          varList[[5]] = elpct;
          varList[[12]] = h1;
          If[flmLen === 2, varList[[16]] = h2;];
          MediaTrf = TransformMedia[Media, varList, opts];
          inptArray[[i]] = {MediaTrf, incLght, opts};
          , {i, len}
        ];

        DistributeDefinitions[inptArray];
        If[utll === True,
          solArray = ParallelTable[Chop[SolutionAverageNew[inptArray[[i]][[1]], inptArray[[i]][[2]], inptArray[[i]][[3]]]], {i, len}];
          ,
          solArray = ParallelTable[Chop[SolutionNew[inptArray[[i]][[1]], inptArray[[i]][[2]], inptArray[[i]][[3]]]], {i, len}];
        ];

        Do[
          sol = solArray[[i]];
          solArr = Table[{sol}, {funcLen}];
          If[utll === True,
            funcArr = MapThread[AveragingFunc, {invSol[[1, 4, msDST + 1, 3]], solArr, IsAverageArr}];
            ,
            funcArr = MapThread[Apply, {invSol[[1, 4, msDST + 1, 3]], solArr}];
          ];
          retval += Sum[((funcArr[[j]] - invSol[[1, 4, msDST + 1, 2, i, j]])^2), {j, funcLen}];, {i, len}
        ];
      ];

      retval /= len;
      retval += 1000 * MFHlpSetEpsilonLimits[Epsilon, opts];
      retval *= mfMult;
      If[prtEps === True, Print[" MFHlp::Epsilon = ", Epsilon, ", retval = ", retval, ", UTLL = ", utll, ", CHECK h2 Correctness: ", h2]];
      Return[retval];
    ];
(*==============================================*)
ProcessInverseOptions[opts___] :=
    Module[{retval, lmbCol, fitaCol, betaCol, gammaCol, outStartCol, dtStart, lmbMult, angMult, outCols, fTolr, epsStart, optprc, elpctCol}, optprc = Flatten[{opts}];

    lmbCol = LambdaColumn /. optprc /. Options[BerremanInverse];
    fitaCol = FitaColumn /. optprc /. Options[BerremanInverse];
    betaCol = BetaColumn /. optprc /. Options[BerremanInverse];
    gammaCol = GammaColumn /. optprc /. Options[BerremanInverse];
    elpctCol = EllipticityColumn /. optprc /. Options[BerremanInverse];

    outStartCol = OutputStartColumn /. optprc /. Options[BerremanInverse];
    dtStart = DataStartRow /. optprc /. Options[BerremanInverse];
    lmbMult = LambdaMultiplier /. optprc /. Options[BerremanInverse];
    angMult = AngleMultiplier /. optprc /. Options[BerremanInverse];
    outCols = Flatten[{OutputColumns /. optprc /. Options[BerremanInverse]}, 1];
    fTolr = FitaTolerance /. optprc /. Options[BerremanInverse];
    epsStart = EpsilonStart /. optprc /. Options[BerremanInverse];
    If[Length[epsStart] =!= 3 || (Length[epsStart] === 3 && Length[epsStart[[1]]] =!= 3 && Length[epsStart[[2]]] =!= 3 && Length[epsStart[[3]]] =!= 3), epsStart = EpsilonFromN[1.5]];

    (* 1, 2, 3, 4, 5 - lambda, fita, beta, gamma, ellipticity columns, 6 - start output column, 7 - data start row, 8 - lambda multiplier,9 - angles multiplier, 10 - output columns in format {{...},{...}}, 11 - fitaTolerance, 12 - Start Epsilon *)

    retval = {lmbCol, fitaCol, betaCol, gammaCol, elpctCol, outStartCol, dtStart, lmbMult, angMult, outCols, fTolr, epsStart};
    Return[retval];
    ];

(*==============================================*)
(* 1, 2, 3, 4, 5 - lambda, fita, beta, gamma, ellipticity columns, 6 - start output column, 7 - data start row, 8 - lambda multiplier,9 - angles multiplier, 10 - output columns in format {{...},{...}}, 11 - fitaTolerance, 12 - Start Epsilon *)
ProcessInputData[InptList_, FuncList_, IdxOpt_] :=
    Module[{retval, len, inptRow, outptRow, outLen, row, startRow, rowLen, outColsLen1, outColsLen2, inpt1 = {}, outpt1 = {}, inpt2 = {}, outpt2 = {}, outCols, dstColLen, fl1 = {}, fl2 = {}, fTolr, outptRow2, elpctCol},

    (*
    Print["ProcessInputData::Starting..."];
    Print["FuncList = ",FuncList];
    *)

      len = Length[InptList];
      startRow = IdxOpt[[7]];
      rowLen = Length[InptList[[1]]];
      outCols = IdxOpt[[10]];
      (* Print["outCols = ",outCols]; *)

      fTolr = IdxOpt[[11]];
      dstColLen = Length[outCols];
      (* Print["dstColLen = ", dstColLen]; *)

      (*Ok if dstColLen\[Equal]1 - load all into one DST, if dstColLen\[Equal]2,then load data with fita\[NotEqual]0 into first dst and with fita\[Equal]0 into second DST*)(*dstColLen>2 is not supported yet!!!*)(*inptRow:1-fita,2-beta,3-gamma,4-ellipticity*)

      Which[dstColLen === 1, outColsLen1 = Length[outCols[[1]]];
      (*fl1=FuncList[[1]];*)
      (*Print["FuncList = ",FuncList];*)
      fl1 = FuncList;

      (*Print["fl1 = ",fl1]; Print["fl2 = ",fl2];*)

      Do[row = InptList[[i]];
      inptRow = {row[[IdxOpt[[2]]]] * IdxOpt[[9]], row[[IdxOpt[[3]]]] * IdxOpt[[9]], row[[IdxOpt[[4]]]] * IdxOpt[[9]], row[[IdxOpt[[5]]]]};
      outptRow = If[outCols[[1, 1]] === strUseAllOutputColumns, Table[row[[j]], {j, IdxOpt[[6]], rowLen}], Table[row[[outCols[[1, j]]]], {j, outColsLen1}]];
      inpt1 = Join[inpt1, {inptRow}];
      outpt1 = Join[outpt1, {outptRow}];, {i, startRow, len}];
        ,
        dstColLen === 2, outColsLen1 = Length[outCols[[1]]];

      (*Print["FuncList = ",FuncList];*)
      (*fl1=FuncList[[1]];*)

      fl1 = FuncList;
      outColsLen2 = Length[outCols[[2]]];

      (*fl2=FuncList[[2]];*)
      fl2 = FuncList;
      (*Print["fl1 = ",fl1];Print["fl2 = ",fl2];*)
      Do[row = InptList[[i]];
      inptRow = {row[[IdxOpt[[2]]]] * IdxOpt[[9]], row[[IdxOpt[[3]]]] * IdxOpt[[9]], row[[IdxOpt[[4]]]] * IdxOpt[[9]], row[[IdxOpt[[5]]]]};
      outptRow = If[outCols[[1, 1]] === strUseAllOutputColumns, Table[row[[j]], {j, IdxOpt[[6]], rowLen}], Table[row[[outCols[[1, j]]]], {j, outColsLen1}]];
      outptRow2 = If[outCols[[2, 1]] === strUseAllOutputColumns, Table[row[[j]], {j, IdxOpt[[6]], rowLen}], Table[row[[outCols[[2, j]]]], {j, outColsLen2}]];
      If[Abs[inptRow[[1]]] >= fTolr, inpt1 = Join[inpt1, {inptRow}];
      outpt1 = Join[outpt1, {outptRow}], inpt2 = Join[inpt2, {inptRow}];
      outpt2 = Join[outpt2, {outptRow}]];, {i, startRow, len}];, True, Print["Invalid number of output datasets specified: ", dstColLen, ". Maximum allowed is 2."];
      Abort[];];
      retval = {dstColLen, {inpt1, outpt1, fl1}, {inpt2, outpt2, fl2}};

      (*
      Print["fl1 = ",fl1];
      Print["fl2 = ",fl2];
      Print["FuncList = ",FuncList];
      Print["ProcessInputData::retval = ",retval];
      Print["ProcessInputData::retval[[1]] = ",retval[[1]]];
      Print["ProcessInputData::retval[[2]] = ",retval[[2]]];
      Print["ProcessInputData::retval[[3]] = ",retval[[3]]];
      *)

      Return[retval];
    ];
(*==============================================*)
(*RefrIndList={n1,n2,nOut},InptList=OutList from FieldIO WITHOUT Film angle variables,that's:*)
(*{Input,No,lambda,fita,beta,gamma,h,Output,Func1,Func2,Func3,...*)

InverseSolutionNew[lambda_, RefrIndList : {_, _, _}, ThicknessList : {_, _}, InptList_, FuncList_, Description_ : "", opts___] :=
    Module[{inptVar, outptFunc, retval, n1, n2, nOut, optprc, status, IdxOpt, inptData, h1, h2, epsStart},
      Print["Initializing Inverse Problem..."];
      n1 = RefrIndList[[1]];
      n2 = RefrIndList[[2]];
      nOut = RefrIndList[[3]];
      h1 = ThicknessList[[1]];
      h2 = ThicknessList[[2]];
      IdxOpt = ProcessInverseOptions[opts];
      optprc = Flatten[{opts}];
      epsStart = IdxOpt[[12]];
      inptData = ProcessInputData[InptList, FuncList, IdxOpt];

      (*Print["inptData = ",inptData];*)
      (*Print["inptVar = ",MatrixForm[inptVar]]; *)
      (* Print["outptFunc = ",MatrixForm[outptFunc]];*)

      status = 0;

      (*Ok this is what we have,retval[[1]]-input data,retval[[2]] options,retval[[3]]-{status},*)(*inptData:1-lambda,2-{n1,n2,nOut},3-{h1,h2},4-{{inptVar1,OuptFunc1},{inptVar2,OuptFunc2}}*)(*retval[[4]]-various output:1-Epsilon,2-{angles},3,4-TBD*)

      retval = {{lambda, {n1, n2, nOut}, {h1, h2}, inptData, Description, epsStart}, optprc, {status}, {{}, {}, {0}, {}}};
      Print["Done."];
      Return[retval];

    ];

(*==============================================*)
InverseSolutionEpsilonStart[InvSol_] := Module[{}, Return[InvSol[[1, 6]]];];
(*==============================================*)
InverseSolutionOptions[InvSol_] := Module[{}, Return[InvSol[[2]]];];
(*==============================================*)
ProcessMinSolEps[Epsilon : {{_, _, _}, {_, _, _}, {_, _, _}}, InvSol_] :=
    Module[{retval, egVal, epsRet, fi, theta, psi, edif, opts, solAng, MFA, FiThetaPsi, angMult},
      opts = InverseSolutionOptions[InvSol];
      angMult = AngleRoundingMultiplier /. opts /. Options[BerremanInverse];
      egVal = Eigenvalues[Epsilon];
      epsRet = {{egVal[[3]], 0, 0}, {0, egVal[[1]], 0}, {0, 0, egVal[[2]]}};
      edif[fi_, theta_, psi_] := (Transform[epsRet, RotationNew[fi, theta, psi, opts]] - Epsilon);
      MFA[fi_, theta_, psi_] := (Sum[Sum[Abs[edif[fi, theta, psi][[i, j]]]^2, {j, 1, 3}], {i, 1, 3}]);
      solAng = FindMinimum[MFA[fi, theta, psi], {{fi, 0, 0.1}, {theta, 0, 0.1}, {psi, 0, 0.1}} (*,StepMonitor\[RuleDelayed]Print[{fi/Degree,theta/Degree,psi/Degree}],EvaluationMonitor\[RuleDelayed]Print[{fi/Degree,theta/Degree,psi/Degree}]*)];
      FiThetaPsi = ({fi, theta, psi} /. solAng[[2]]);
      retval = {epsRet, FiThetaPsi};
      Print["Epsilon = ", MatrixForm[Chop[epsRet]], ", n = ", MatrixForm[Chop[MatrixPower[epsRet, 1 / 2]]], ", Rotation angles = ", MatrixForm[N[Round[FiThetaPsi * angMult / Degree] / angMult]]];
      Return[retval];

    ];
(*==============================================*)
(*==============================================*)
Attributes[InverseSolutionRunStdTransparent] = {HoldFirst};
InverseSolutionRunStdTransparent[InvSol_] :=
    Module[{retval, opts, stpMonFlg, evalMonFlg, nMin, epsTyp, minSol, evCnt, stpCnt, minVal, TimeStart, Epsilon, procEps, hlpVal, eps1, eps2, eps3, eps12, eps13, eps23, varList, lmbMult, varPointsList, varCondList, stpMon, evalMon, FuncName, meth, evalMonCnt},
      retval = InvSol;
      opts = InverseSolutionOptions[InvSol];
      stpMonFlg = UseStepMonitor /. opts /. Options[BerremanInverse];
      evalMonFlg = UseEvaluationMonitor /. opts /. Options[BerremanInverse];
      evalMonCnt = EvaluationMonitorCount /. opts /. Options[BerremanInverse];

      nMin = UseNMinimize /. opts /. Options[BerremanInverse];
      epsTyp = EpsilonType /. opts /. Options[BerremanInverse];
      lmbMult = LambdaMultiplier /. opts /. Options[BerremanInverse];
      evCnt = 0;
      stpCnt = 0;minVal = 10^10;TimeStart = TimeUsed[];
      Print["InverseSolutionRunStdTransparent - IS NOT WORKING PROPERLY (and not yet updated in 4.02...)!!! "];
      Abort[];
      meth = (Method -> "DifferentialEvolution");
      MF[eps1_, eps2_, eps3_, eps12_, eps13_, eps23_] = Hold[MFHlp[{{eps1, eps12, eps13}, {eps12, eps2, eps23}, {eps13, eps23, eps3}}, InvSol]];
      FuncName = MF;
      varList = {eps1, eps2, eps3, eps12, eps13, eps23};
      stpMon := (StepMonitor :> None);
      evalMon := (EvaluationMonitor :> None);
      varPointsList = {{eps1, 1.4^2, 1.6^2}, {eps2, 1.7^2, 1.8^2}, {eps3, 1.6^2, 1.7^2}, {eps12, 0, 0.1}, {eps13, 0, 0.1}, {eps23, 0, 0.1}};
      varCondList = (eps1 >= 1.4^2 && eps1 <= 1.8^2 && eps2 >= 1.4^2 && eps2 <= 1.8^2 && eps3 >= 1.4^2 && eps3 <= 1.8^2 && eps12 >= -0.2 && eps12 <= 0.2 && eps13 >= -0.2 && eps13 <= 0.2 && eps23 >= -0.2 && eps23 <= 0.2);
      If[stpMonFlg === True, stpMon := (StepMonitor :> (Print["Step: ", stpCnt, ", Vars: ", {(minVal = ReleaseHold[Apply[FuncName, varList]]), varList}, ", Time used: ", (TimeUsed[] - TimeStart)];stpCnt++;))];
      If[evalMonFlg === True, evalMon := (EvaluationMonitor :> (Print[" Evaluation: ", evCnt, ", minVal = ", minVal, ", Vars: ", varList, ", Time used: ", (TimeUsed[] - TimeStart)];evCnt++;))];
      minSol = Apply[FindMinimum, {Apply[FuncName, varList], varPointsList, stpMon, evalMon}];
      If[nMin === True, Print["Using NMinimize -> DifferentialEvolution method."];
      minSol = Apply[NMinimize, {{Apply[FuncName, varList], varCondList}, varList, Method -> "DifferentialEvolution" meth, stpMon, evalMon}];, Print["Using FindMinimum."];
      minSol = Apply[FindMinimum, {Apply[FuncName, varList], varPointsList, stpMon, evalMon}];];
      Print["minSol[[2]] = ", minSol[[2]]];
      Epsilon = {{eps1, eps12, eps13}, {eps12, eps2, eps23}, {eps13, eps23, eps3}} /. minSol[[2]];
      procEps = ProcessMinSolEps[Epsilon, InvSol];

      (*retval[[4,1]]-Epsilon,retval[[4,2]]-{Angles}*)

      retval[[4, 1]] = procEps[[1]];
      retval[[4, 2]] = procEps[[2]];
      InvSol = retval;
      Return[retval];

    ];

(*==============================================*)
InverseSolutionEpsilon[InvSol_] := Module[{}, Return[InvSol[[4, 1]]];];
InverseSolutionAngles[InvSol_] := Module[{}, Return[InvSol[[4, 2]]];];
(*==============================================*)
(*BIEPSTYPESTDTWOSTEPTRAPSPARENT=1;BIEPSTYPESTDTWOSTEPABSORBING=101;*)

Attributes[InverseSolutionRun] = {HoldFirst};
InverseSolutionRun[InvSol_] :=
    Module[{retval, opts, stpMon, evalMon, nMin, epsTyp},
      opts = InverseSolutionOptions[InvSol];
      epsTyp = EpsilonType /. opts /. Options[BerremanInverse];

      Which[epsTyp === 0, retval = InverseSolutionRunStdTransparent[InvSol];, epsTyp === BIEPSTYPESTDTWOSTEPTRAPSPARENT, retval = ISRunTwoStepStdTransparent[InvSol];, epsTyp === BIEPSTYPESTDTWOSTEPABSORBING, retval = ISRunTwoStepStdAbsorbing[InvSol];, True, (Print["Invalid Epsilon type specified in InverseSolutionRun"];Abort[];)];

      InvSol = retval;
      Return[retval];
    ];
(*==============================================*)
InverseSolutionSave[InvSol_] :=
    Module[{Calc, retval = 0},
      Return[retval];
    ];
(*==============================================*)
EpsilonComp[eps0_, deltaEps_, nu_] :=
    Module[{retEps},
      retEps = {{eps0 - deltaEps, nu, 0}, {nu, eps0 + deltaEps, 0}, {0, 0, eps0}};
      Return[retEps];
    ];

EpsilonFull[eps0_, deltaEps_, nu_, eps13_, eps23_, eps33_] :=
    Module[{retEps, eps11, eps22, eps12},
      eps11 = eps0 - deltaEps + eps13^2 / eps33;
      eps22 = eps0 + deltaEps + eps23^2 / eps33;
      eps12 = nu + eps13 * eps23 / eps33;
      retEps = {{eps11, eps12, eps13}, {eps12, eps22, eps23}, {eps13, eps23, eps33}};
      Return[retEps];
    ];
(*==============================================*)
EpsilonCompHlp[n0_, deltaEps_, nu_] := EpsilonComp[n0^2, n0^2 * deltaEps, n0^2 * nu];
(*==============================================*)
Attributes[ISRunTwoStepStdTransparent] = {HoldFirst};
ISRunTwoStepStdTransparent[InvSol_] :=
    Module[{MF, varList, varPointsList, FuncName, stpMon, evalMon, minSol, minVal, TimeStart = TimeUsed[], evCnt = 0, stpCnt = 0, n0Sol, deltaEpsSol, nuSol, mfopts, epsSol, procEps, retval, epsStart, nStart, n0Start, deltaEpsStart, nuStart, minSol1, minSol2, deltaEps33Sol, eps13Sol, eps23Sol, opts, evalMonCnt, stpMonCnt, evalMonCnt2, stpMonCnt2, evalMonCntHlp, stpMonCntHlp},
      Print["Running Inverse Solution: Two Step Standard Transparent."];
      retval = InvSol;
      Print["Starting step ONE..."];

      opts = InverseSolutionOptions[InvSol];
      evalMonCnt = EvaluationMonitorCount /. opts /. Options[BerremanInverse];
      stpMonCnt = StepMonitorCount /. opts /. Options[BerremanInverse];
      evalMonCnt2 = EvaluationMonitorSecondStepCount /. opts /. Options[BerremanInverse];
      stpMonCnt2 = StepMonitorSecondStepCount /. opts /. Options[BerremanInverse];

      evalMonCntHlp = evalMonCnt;
      stpMonCntHlp = stpMonCnt;

      (*
      Print["opts = ", opts];
      Print["evalMonCntHlp = ", evalMonCntHlp];
      Print["stpMonCntHlp = ", stpMonCntHlp];
      *)

      mfopts = {MFHlpDataSet -> 2};
      MF1[n0_, deltaEps_, nu_] = Hold[MFHlp[EpsilonComp[n0^2, n0^2 * deltaEps, n0^2 * nu], InvSol, mfopts]];
      epsStart = InverseSolutionEpsilonStart[InvSol];
      nStart = Re[MatrixPower[epsStart, 0.5]];
      n0Start = Sqrt[Re[(epsStart[[1, 1]] + epsStart[[2, 2]]) / 2]];
      deltaEpsStart = Re[(epsStart[[2, 2]] - epsStart[[1, 1]]) / 2 / n0Start^2];
      nuStart = Re[epsStart[[1, 2]] / n0Start^2];
      varList = {n0, deltaEps, nu};
      varPointsList = {{n0, n0Start, n0Start + 0.1}, {deltaEps, deltaEpsStart, deltaEpsStart + 0.1}, {nu, nuStart, nuStart + 0.1}};
      FuncName = MF1;
      minVal = 10^10;
      stpMon := (StepMonitor :> (If[Mod[stpCnt, stpMonCntHlp] == 0, Print["Step Count: ", stpCnt, ", Vars: ", {(minVal = ReleaseHold[Apply[FuncName, varList]]), varList}, ", Time used: ", ConvertToTime[TimeUsed[] - TimeStart]]];stpCnt++;));
      evalMon := (EvaluationMonitor :> (If[Mod[evCnt, evalMonCntHlp] == 0, Print[" Evaluation Count: ", evCnt, ", minVal = ", minVal, ", Vars: ", varList, ", Time used: ", ConvertToTime[TimeUsed[] - TimeStart]]];evCnt++;));

      Print["Running FindMinimum on Step ONE..."];
      minSol1 = Apply[FindMinimum, {Apply[FuncName, varList], varPointsList, stpMon, evalMon}];

      Print["minSol1 = ", minSol1];
      n0Sol = (n0 /. minSol1[[2]]);
      deltaEpsSol = (deltaEps /. minSol1[[2]]);
      nuSol = (nu /. minSol1[[2]]);
      Print["Step ONE completed."];

      Print["====================="];

      Print["Starting step TWO..."];
      evalMonCntHlp = evalMonCnt2;
      stpMonCntHlp = stpMonCnt2;
      evCnt = 0;
      stpCnt = 0;

      (*
      Print["evalMonCntHlp = ", evalMonCntHlp];
      Print["stpMonCntHlp = ", stpMonCntHlp];
      *)

      mfopts = {MFHlpDataSet -> 1};
      MF2[deltaEps33_, eps13_, eps23_] = Hold[MFHlp[EpsilonFull[n0Sol^2, n0Sol^2 * deltaEpsSol, n0Sol^2 * nuSol, n0Sol^2 * eps13, n0Sol^2 * eps23, n0Sol^2 * (1 + deltaEps33)], InvSol, mfopts]];
      varList = {deltaEps33, eps13, eps23};
      varPointsList = {{deltaEps33, 0, 0.1}, {eps13, 0, 0.1}, {eps23, 0, 0.1}};
      FuncName = MF2;minVal = 10^10;

      Print["Running FindMinimum on Step TWO..."];
      minSol2 = Apply[FindMinimum, {Apply[FuncName, varList], varPointsList, stpMon, evalMon}];

      Print["minSol2 = ", minSol2];
      deltaEps33Sol = (deltaEps33 /. minSol2[[2]]);
      eps13Sol = (eps13 /. minSol2[[2]]);
      eps23Sol = (eps23 /. minSol2[[2]]);
      Print["Step TWO completed."];
      epsSol = EpsilonFull[n0Sol^2, n0Sol^2 * deltaEpsSol, n0Sol^2 * nuSol, n0Sol^2 * eps13Sol, n0Sol^2 * eps23Sol, n0Sol^2 * (1 + deltaEps33Sol)];
      procEps = ProcessMinSolEps[epsSol, InvSol];

      (*retval[[4,1]]-Epsilon,retval[[4,2]]-{Angles}*)

      retval[[4, 1]] = procEps[[1]];
      retval[[4, 2]] = procEps[[2]];
      InvSol = retval;
      Return[retval];];
(*==============================================*)
EpsilonCompHlp[n0_, deltaEps_, nu_, EpsIM1_, EpsIM2_, nuIM_] :=
    EpsilonComp[n0^2 * Complex[1, (EpsIM1 + EpsIM2) / 2], n0^2 * Complex[deltaEps, (EpsIM1 - EpsIM2) / 2], n0^2 * Complex[nu, nuIM]];

EpsilonFullHlp[n0_, deltaEps_, nu_, deltaEps33_, eps13_, eps23_, EpsIM1_, EpsIM2_, nuIM_, EpsIM3_, EpsIM13_, EpsIM23_] :=
    EpsilonFull[n0^2 * Complex[1, (EpsIM1 + EpsIM2) / 2], n0^2 * Complex[deltaEps, (EpsIM1 - EpsIM2) / 2], n0^2 * Complex[nu, nuIM], n0^2 * Complex[eps13, EpsIM13], n0^2 * Complex[eps23, EpsIM23], n0^2 * Complex[1 + deltaEps33, EpsIM3 + (EpsIM1 + EpsIM2) / 2]];
(*==============================================*)
Attributes[ISRunTwoStepStdAbsorbing] = {HoldFirst};
ISRunTwoStepStdAbsorbing[InvSol_] :=
    Module[{MF, varList, varPointsList, FuncName, stpMon, evalMon, minSol, minVal, TimeStart = TimeUsed[], evCnt = 0, stpCnt = 0, n0Sol, deltaEpsSol, nuSol, mfopts, epsSol, procEps, retval, minSol1, EpsIM1Sol, minSol2, deltaEps33Sol, eps13Sol, EpsIM3Sol, EpsIM23Sol, nuIMSol, EpsIM2Sol, eps23Sol, EpsIM13Sol, opts, evalMonCnt, stpMonCnt, evalMonCnt2, stpMonCnt2, evalMonCntHlp, stpMonCntHlp},
      Print["Running Inverse Solution: Two Step Standard Absorbing."];
      retval = InvSol;
      Print["Starting step ONE..."];

      opts = InverseSolutionOptions[InvSol];
      evalMonCnt = EvaluationMonitorCount /. opts /. Options[BerremanInverse];
      stpMonCnt = StepMonitorCount /. opts /. Options[BerremanInverse];
      evalMonCnt2 = EvaluationMonitorSecondStepCount /. opts /. Options[BerremanInverse];
      stpMonCnt2 = StepMonitorSecondStepCount /. opts /. Options[BerremanInverse];

      evalMonCntHlp = evalMonCnt;
      stpMonCntHlp = stpMonCnt;

      (*
      Print["opts = ", opts];
      Print["evalMonCnt = ", evalMonCnt];
      Print["stpMonCnt = ", stpMonCnt];
      Print["evalMonCnt2 = ", evalMonCnt2];
      Print["stpMonCnt2 = ", stpMonCnt2];
      *)

      mfopts = {MFHlpDataSet -> 2, MFHlpAbsorbingEpsilon -> True};
      MF1[n0_, deltaEps_, nu_, EpsIM1_, EpsIM2_, nuIM_] = Hold[MFHlp[EpsilonCompHlp[n0, deltaEps, nu, EpsIM1, EpsIM2, nuIM], InvSol, mfopts]];
      varList = {n0, deltaEps, nu, EpsIM1, EpsIM2, nuIM};
      varPointsList = {{n0, 1.5, 1.6}, {deltaEps, 0, 0.1}, {nu, 0, 0.1}, {EpsIM1, 0, 0.1}, {EpsIM2, 0, 0.1}, {nuIM, 0, 0.1}};
      FuncName = MF1;
      minVal = 10^10;
      stpMon := (StepMonitor :> (If[Mod[stpCnt, stpMonCntHlp] == 0, Print["Step Count: ", stpCnt, ", Vars: ", {(minVal = ReleaseHold[Apply[FuncName, varList]]), varList}, ", Time used: ", ConvertToTime[TimeUsed[] - TimeStart]]];stpCnt++;));
      evalMon := (EvaluationMonitor :> (If[Mod[evCnt, evalMonCntHlp] == 0, Print[" Evaluation Count: ", evCnt, ", minVal = ", minVal, ", Vars: ", varList, ", Time used: ", ConvertToTime[TimeUsed[] - TimeStart]]];evCnt++;));
      minSol1 = Apply[FindMinimum, {Apply[FuncName, varList], varPointsList, stpMon, evalMon}];

      Print["minSol1 = ", minSol1];
      n0Sol = (n0 /. minSol1[[2]]);
      deltaEpsSol = (deltaEps /. minSol1[[2]]);
      nuSol = (nu /. minSol1[[2]]);
      EpsIM1Sol = (EpsIM1 /. minSol1[[2]]);
      EpsIM2Sol = (EpsIM2 /. minSol1[[2]]);
      nuIMSol = (nuIM /. minSol1[[2]]);
      Print["varList = ", (varList /. minSol1[[2]])];
      Print["Step ONE completed."];

      Print["====================="];

      Print["Starting step TWO..."];
      evalMonCntHlp = evalMonCnt2;
      stpMonCntHlp = stpMonCnt2;
      evCnt = 0;
      stpCnt = 0;

      mfopts = {MFHlpDataSet -> 1, MFHlpAbsorbingEpsilon -> True};
      MF2[deltaEps33_, eps13_, eps23_, EpsIM3_, EpsIM13_, EpsIM23_] = Hold[MFHlp[EpsilonFullHlp[n0Sol, deltaEpsSol, nuSol, deltaEps33, eps13, eps23, EpsIM1Sol, EpsIM2Sol, nuIMSol, EpsIM3, EpsIM13, EpsIM23], InvSol, mfopts]];
      varList = {deltaEps33, eps13, eps23, EpsIM3, EpsIM13, EpsIM23};
      varPointsList = {{deltaEps33, 0, 0.1}, {eps13, 0, 0.1}, {eps23, 0, 0.1}, {EpsIM3, 0, 0.1}, {EpsIM13, 0, 0.1}, {EpsIM23, 0, 0.1}};
      FuncName = MF2;
      minVal = 10^10;
      minSol2 = Apply[FindMinimum, {Apply[FuncName, varList], varPointsList, stpMon, evalMon}];

      Print["minSol2 = ", minSol2];
      deltaEps33Sol = (deltaEps33 /. minSol2[[2]]);
      eps13Sol = (eps13 /. minSol2[[2]]);
      eps23Sol = (eps23 /. minSol2[[2]]);
      EpsIM3Sol = (EpsIM3 /. minSol2[[2]]);
      EpsIM13Sol = (EpsIM13 /. minSol2[[2]]);
      EpsIM23Sol = (EpsIM23 /. minSol2[[2]]);
      Print["Step TWO completed."];
      epsSol = EpsilonFullHlp[n0Sol, deltaEpsSol, nuSol, deltaEps33Sol, eps13Sol, eps23Sol, EpsIM1Sol, EpsIM2Sol, nuIMSol, EpsIM3Sol, EpsIM13Sol, EpsIM23Sol];
      procEps = ProcessMinSolEps[epsSol, InvSol];
      (*retval[[4,1]]-Epsilon,retval[[4,2]]-{Angles}*)
      retval[[4, 1]] = procEps[[1]];
      retval[[4, 2]] = procEps[[2]];
      InvSol = retval;
      Return[retval];
    ];
(*==============================================*)
