(* ============================================== *)
(* :Summary: This module defines some commonly used Berreman Matrix transformations. *)
(* ============================================== *)
(* :Author: Konstantin K. Konstantinov *)
(* :Email: konstantin.k.konstantinov@gmail.com *)
(* :License type: GPL v3 or any later version, see http://www.gnu.org/licenses/ *)
(* :Copyright: K^3, 2001 - 2018 *)
(* :Version: Revision: 6.03.001, Date: 2018/05/05 *)
(* :Mathematica Version: 11.2 *)
(* ============================================== *)
(* This program is free software: you can redistribute it and/or modify it under the terms *)
(* of the GNU General Public License as published by the Free Software Foundation, *)
(* either version 3 of the License, or any later version. This program is distributed in the hope that  *)
(* it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. *)
(* You should have received a copy of the GNU General Public License along with this program. *)
(* If not, see <http://www.gnu.org/licenses/>. *)
(* ============================================== *)
Options[BerremanCommon] =
    {
      BerremanCommonVersion -> 6.03,
      UseEulerAngles -> False,
      CalculateBoundarySolution -> False,
      CalculateDelta -> False,
      OutputPPPMultiplier -> 0,
      UseSolveInSolutionNew -> False,
      SwapEigenValues -> True,
      AddOnEigenValuesSort -> 1,
      UseNumericEigenValues -> True,
      PrintCommonDebugInfo -> False,
      PrintCommonDebugInfoLevel -> 0,
      CalculateBeta0and90 -> False,
      UseQuietSolve -> False
    };
(* ============================================== *)
SubstanceIdx = 0;
(* ============================================== *)
IncidentRayClassName = "IncidentRay";
(* ============================================== *)
SemiInfiniteMediaClassName = "SemiInfiniteMedia";
FilmClassName = "Film";
ThickPlateClassName = "ThickPlate";

SubstanceClassList = {SemiInfiniteMediaClassName, FilmClassName, ThickPlateClassName};
(* ============================================== *)
LayeredSystemClassName = "LayeredSystem";
(* ============================================== *)
ObjectGetClassName[obj_] := Module[{className, err},
  className = Indeterminate;
  err = False;

  If[Head[obj] == List,
    (
      If[Length[obj] > 0,
        (
          If[StringQ[obj[[1]]],
            (
              className = obj[[1]];
            ),
            (
              err = True;
            )
          ];
        ),
        (
          err = True;
        )
      ];
    ),
    (
      err = True;
    )
  ];

  If[err, Print["ObjectGetClassName::Incorrect object: ", obj]];

  Return[className];
];
(* ============================================== *)
ObjectGetContent[obj_] := Module[{content, err},
  content = Indeterminate;
  err = False;

  If[Head[obj] == List,
    (
      If[Length[obj] >= 2,
        (
          If[StringQ[obj[[1]]],
            (
              content = obj[[2]];
            ),
            (
              err = True;
            )
          ];
        ),
        (
          err = True;
        )
      ];
    ),
    (
      err = True;
    )
  ];

  If[err, Print["ObjectGetContent::Incorrect object: ", obj]];

  Return[content];
];
(* ============================================== *)
FilmQ[obj_] := If[ObjectGetClassName[obj] == FilmClassName, True, False, False];
ThickPlateQ[obj_] := If[ObjectGetClassName[obj] == ThickPlateClassName, True, False, False];
SemiInfiniteMediaQ[obj_] := If[ObjectGetClassName[obj] == SemiInfiniteMediaClassName, True, False, False];
SubstanceQ[obj_] := If[MemberQ[SubstanceClassList, ObjectGetClassName[obj]], True, False, False];
IncidentRayQ[obj_] := If[ObjectGetClassName[obj] == IncidentRayClassName, True, False, False];
LayeredSystemQ[obj_] := If[ObjectGetClassName[obj] == LayeredSystemClassName, True, False, False];
(* ============================================== *)
SubstanceListQ[objList_] :=
    Module[{retVal, mapVal, andVal},
    (* Print["SubstanceListQ::objList = ", objList]; *)

      If[ListQ[objList],
        (
          mapVal = Map[SubstanceQ, objList];
          (* Print["SubstanceListQ::mapVal = ", mapVal]; *)

          andVal = Apply[And, mapVal];
          (* Print["SubstanceListQ::andVal = ", andVal]; *)

          retVal = andVal;
        ),
        (
          retVal = False;
        )
      ];
      Return[retVal];
    ];
(* ============================================== *)
RotationAnglesQ[angles_] :=
    Module[{retVal},
      retVal = False;

      If[MatrixQ[angles],
        (
          retVal = If[Length[angles] == 3 && Length[angles[[1]]] == 5, True, False, False];
        )
      ];

      Return[retVal];
    ];
(* ============================================== *)
(* tensor must be either a 3x3 matrix or a function. *)
(* If it a function, then we cannot validate it. *)
OpticalTensorQ[tensor_] :=
    Module[{retVal},
      retVal = False;

      If[Head[tensor] === Head[{}],
        (
          If[MatrixQ[tensor],
            (
              retVal = If[Length[tensor] == 3 && Length[tensor[[1]]] == 3, True, False, False];
            )
          ];
        ),
        (
          If[Head[tensor] === Symbol,
            (
              retVal = True;
            ),
            (
              retVal = False;
            )
          ];
        )
      ];

      Return[retVal];
    ];
(* ============================================== *)
VariableQ[obj_] := (VectorQ[obj] && (Length[obj] == 5 || Length[obj] == 4));
(* ============================================== *)
(* If True, then wrap Quiet around Solve. *)
UseQuietSolveValue = False;
(* ============================================== *)
mm = 10^-3;
mkm = 10^-6;
nm = 10^-9;
BCMAXPPPALLOWEDVALUE = 2;
BCMINPPPALLOWEDVALUE = 0.5;
BCSUPPRESSERRORMESSAGES = False;
BCZEROPPPONCALCERR = False;

PCDILEVELALL = 4;
PCDILEVELDETAILED = 3;
PCDILEVELMEDIUM = 2;
PCDILEVELSHORT = 1;
PCDILEVELOFF = 0;
(* ============================================== *)
strSeparatorSmall = "---------------------------------------------";
strCRLF = FromCharacterCode[10];
(* ============================================== *)
tStart = AbsoluteTime[];
tMid = tStart;

PrintTimeUsed[showTime_?BooleanQ] :=
    Module[{tEnd, now, diff, diffTot},
      tEnd = AbsoluteTime[];
      now = DateString[];

      If[showTime,
        (
          diff = tEnd - tMid;

          If[diff < 100,
            (
              diff = N[Round[diff, 10^-3]];
            ),
            (
              diff = Round[diff];
            )
          ];

          diffTot = Round[tEnd - tStart];
          Print[now, ", time used: ", diff, ", total time used: ", diffTot, ".", FromCharacterCode[10] <> strSeparatorSmall]
        ),
        (
          Print["Time used reset."]
        )
      ];

      tMid = tEnd;
    ];

PrintTimeUsed[] := PrintTimeUsed[True];
(* ============================================== *)
II := IdentityMatrix[4];
muMstandard := IdentityMatrix[3];
roMstandard := I * DiagonalMatrix[{0, 0, 0}];
rotMstandard := -I * DiagonalMatrix[{0, 0, 0}];
(* ============================================== *)
JoinRight[lst1_, lst2_] := Transpose[Join[Transpose[lst1], Transpose[lst2]]];
JoinRight[lst1_, lst2_, lst3_] := Transpose[Join[Transpose[lst1], Transpose[lst2], Transpose[lst3]]];
JoinRight[lst1_, lst2_, lst3_, lst4_] := Transpose[Join[Transpose[lst1], Transpose[lst2], Transpose[lst3], Transpose[lst4]]];
MFCN[xx_] := MatrixForm[Chop[N[xx]]];
(* ============================================== *)
TimePad[s_] :=
    Module[{},
      Return[StringDrop["00" <> s, (StringLength["00" <> s] - 2)]];
    ];

ConvertToTime[t_] :=
    Module[{},
      Return[ToString[Floor[t / 3600]] <> ":" <> TimePad[ToString[Mod[Floor[t / 60], 60]]] <> ":" <> TimePad[ToString[Mod[Floor[t], 60]]]];
    ];
(* ============================================== *)
SetAttributes[ProcessOptionNames, Listable];
ProcessOptionNames[(r : (Rule | RuleDelayed))[name_Symbol, val_]] := r[SymbolName[name], val];
ProcessOptionNames[opt_] := opt;
(* ============================================== *)
EpsilonFromN[n1_] :=
    Module[{retval}, retval = {{n1^2, 0, 0}, {0, n1^2, 0}, {0, 0, n1^2}};
    Return[retval];
    ];

EpsilonFromN[n1_, n2_, n3_] :=
    Module[{retval}, retval = {{n1^2, 0, 0}, {0, n2^2, 0}, {0, 0, n3^2}};
    Return[retval];
    ];

deltaZero = DiagonalMatrix[{0, 0, 0, 0}];
(* ============================================== *)
M[epsilon : {{_, _, _}, {_, _, _}, {_, _, _}}, mu : {{_, _, _}, {_, _, _}, {_, _, _}}, ro : {{_, _, _}, {_, _, _}, {_, _, _}}, rotr : {{_, _, _}, {_, _, _}, {_, _, _}}] :=
    BlockMatrix[{{epsilon, ro}, {rotr, mu}}];
(* ============================================== *)
(*Optiva Rotation 1-Fi (angle between crystal axis and deposition direction.)-rotation around z*)
fRopt1[fi_] := RotationMatrix3D[fi, 0, 0];

(*Optiva Rotation 2-Psi (angle between crystal axis and substrate plane.)-rotation around y (in the opposite direction!!!)*)
fRopt2[psi_] := RotationMatrix3D[0, -Pi / 2, 0].RotationMatrix3D[0, Pi / 2, -psi];

(*Optiva Rotation 3-Alpha (rotation of crystal around its axis.)-rotation around x*)
fRopt3[alpha_] := RotationMatrix3D[0, alpha, 0];

(*Optiva Combined Rotation (1,2,3).*)
fRopt[fi_, psi_, alpha_] := fRopt3[alpha].fRopt2[psi].fRopt1[fi];
RM[fi_, theta_, psi_] := fRopt[fi, theta, psi];
RMT[fi_, theta_, psi_] := Transpose[RM[fi, theta, psi]];
(* ============================================== *)
EulerFi[fi_, psi_, alpha_] := ArcCos[(Cos[fi] * Sin[alpha] - Cos[alpha] * Sin[fi] * Sin[psi]) / Sqrt[Cos[psi]^2 * Sin[alpha]^2 + Sin[psi]^2]] * Sign[psi];
EulerTheta[fi_, psi_, alpha_] := ArcCos[Cos[alpha] * Cos[psi]];
EulerPsi[fi_, psi_, alpha_] := -(ArcCos[(Cos[psi] * Sin[alpha]) / Sqrt[Cos[psi]^2 * Sin[alpha]^2 + Sin[psi]^2]] * Sign[psi]);
(* ============================================== *)
RotationNew[fi_, theta_, psi_, opts___] :=
    Module[{useEA, retval},
      useEA = TrueQ[UseEulerAngles /. Flatten[{opts}] /. Flatten[{Options[BerremanCommon]}]];

      If[useEA === True,
        retval =
            {
              RotationMatrix3D[fi, theta, psi],
              Transpose[RotationMatrix3D[fi, theta, psi]],
              {fi, theta, psi},
              {UseEulerAngles -> useEA}
            },
        retval =
            {
              RM[fi, theta, psi],
              RMT[fi, theta, psi],
              {fi, theta, psi},
              {UseEulerAngles -> useEA}
            }
      ];

      Return[retval];
    ];
(* ============================================== *)
(*The order of rotations is VERY important!!! (RMT.ee.RM)*)
Transform[ee_, rotn_] :=
    Module[{},
      Return[(rotn[[2]].ee.rotn[[1]])];
    ];
(* ============================================== *)
PsiAngle[fita_, n1_, n2_] :=
    Module[{},
      Return[ArcSin[(n1 / n2) * Sin[fita]]];
    ];
(* ============================================== *)
(* ============================================== *)
FilmLayerNew[Thickness_, Epsilon_, mu_ : muMstandard, ro_ : roMstandard] :=
    Module[{},
      Return[{Thickness, Epsilon, mu, ro, Transpose[Conjugate[ro]], Epsilon, mu, ro, Transpose[Conjugate[ro]]}];
    ];

FilmLayerTransform[layer_, rotn_, Reset_ : True] :=
    Module[{layerTr},
      If[Reset === True, layerTr = {layer[[1]], Transform[layer[[6]], rotn], Transform[layer[[7]], rotn], Transform[layer[[8]], rotn], Transform[layer[[9]], rotn], layer[[6]], layer[[7]], layer[[8]], layer[[9]]}, layerTr = {layer[[1]], Transform[layer[[2]], rotn], Transform[layer[[3]], rotn], Transform[layer[[4]], rotn], Transform[layer[[5]], rotn], layer[[6]], layer[[7]], layer[[8]], layer[[9]]}];
      Return[layerTr];
    ];

FilmLayerFlip[layer_] :=
    Module[{flpLayer, rotn},
      rotn = RotationNew[0, 0, Pi, UseEulerAngles -> False];
      flpLayer =
          {
            layer[[1]],
            Transform[layer[[2]], rotn],
            Transform[layer[[3]], rotn], Transform[layer[[4]], rotn],
            Transform[layer[[5]], rotn], Transform[layer[[6]], rotn],
            Transform[layer[[7]], rotn], Transform[layer[[8]], rotn],
            Transform[layer[[9]], rotn]
          };
      Return[flpLayer];
    ];
(* ============================================== *)
FilmLayerThickness[FilmLayer_] := Module[{}, Return[FilmLayer[[1]]];];
FilmLayerEpsilon[FilmLayer_] := Module[{}, Return[FilmLayer[[2]]];];
FilmLayerEpsilonBase[FilmLayer_] := Module[{}, Return[FilmLayer[[6]]];];
FilmLayerMu[FilmLayer_] := Module[{}, Return[FilmLayer[[3]]];];
FilmLayerMuBase[FilmLayer_] := Module[{}, Return[FilmLayer[[7]]];];
FilmLayerRo[FilmLayer_] := Module[{}, Return[FilmLayer[[4]]];];
FilmLayerRoBase[FilmLayer_] := Module[{}, Return[FilmLayer[[8]]];];
FilmLayerRoT[FilmLayer_] := Module[{}, Return[FilmLayer[[5]]];];
FilmLayerRoTBase[FilmLayer_] := Module[{}, Return[FilmLayer[[9]]];];
(* ============================================== *)
(* ============================================== *)
FilmNew[] :=
    Module[{}, Return[{}];
    ];

Attributes[FilmAddLayer] = {HoldFirst};

FilmAddLayer[Film_, FilmLayer_] :=
    Module[{},
      Film = Append[Film, FilmLayer];
      Return[Film];
    ];

FilmLength[Film_] :=
    Module[{},
      Return[Length[Film]];
    ];

FilmTransformAll[Film_, rotn_, Reset_ : True] :=
    Module[{len, filmTr},
      len = FilmLength[Film];filmTr = FilmNew[];
      Do[filmTr = FilmAddLayer[filmTr, FilmLayerTransform[Film[[i]], rotn, Reset]], {i, 1, len}];
      Return[filmTr];
    ];

FilmItem[Film_, idx_] :=
    Module[{},
      Return[Film[[idx]]];
    ];
(* ============================================== *)
(* ============================================== *)
(* Media[[i]], 1-n1, 2-n2, 3-gamma, 4-Film, 5-Description, 6-nOut, 7-h2-thickness of the substrate, *)
(* 8-epsilon2, 9-mu2, 10-ro2, 11-epsilon, 12-mu, 13-ro *)
MediaNew[n1_, n2_, gamma_, Film_, Description_ : "", nOut_ : (-1), h2_ : 0, epsilon2_ : {}, mu2_ : muMstandard, ro2_ : roMstandard, epsilon_ : {}, mu_ : muMstandard, ro_ : roMstandard] :=
    Module[{epsVal, eps2Val, retVal},

    (*
    epsVal=If[Head[epsilon]===Head[{}]&&Length[epsilon]===0,EpsilonFromN[Re[n1]],epsilon];
    eps2Val=If[Head[epsilon2]===Head[{}]&&Length[epsilon2]===0,EpsilonFromN[Re[n2]],epsilon2];
    retVal={Re[n1],Re[n2],gamma,Film,ToString[Description],Re[nOut],h2,{epsVal,mu,ro,Transpose[Conjugate[ro]],eps2Val,mu2,ro2,Transpose[Conjugate[ro2]]}};
    *)

      epsVal = If[Head[epsilon] === Head[{}] && Length[epsilon] === 0, EpsilonFromN[n1], epsilon];
      eps2Val = If[Head[epsilon2] === Head[{}] && Length[epsilon2] === 0, EpsilonFromN[n2], epsilon2];
      retVal = {n1, n2, gamma, Film, ToString[Description], nOut, h2, {epsVal, mu, ro, Transpose[Conjugate[ro]], eps2Val, mu2, ro2, Transpose[Conjugate[ro2]]}};

      (*Print["eps2Val = ",eps2Val];*)
      (* Print["MediaNew::retVal = ",retVal]; *)

      Return[retVal];
    ];
(* ============================================== *)
MediaFlip[Media_] :=
    Module[{flpMedia, n1, n2, gamma, Film, Layer, flpFilm, len, Thickness, Epsilon, rotn, flpEps, nOut, Descr, h2},
      n1 = MediaUpperRefractionIndex[Media];
      n2 = MediaLowerRefractionIndex[Media];
      gamma = MediaGamma[Media];
      Film = MediaFilm[Media];
      len = FilmLength[Film];
      flpFilm = FilmNew[];
      nOut = MediaOutRefractionIndex[Media];
      Descr = MediaDescription[Media];
      h2 = MediaSubstrateThickness[Media];
      rotn = RotationNew[0, 0, Pi, UseEulerAngles -> False];

      Do[Layer = FilmLayerFlip[Film[[len + 1 - i]]];
      FilmAddLayer[flpFilm, Layer], {i, len}];
      flpMedia = MediaNew[n2, n1, gamma, flpFilm, Descr, nOut, h2, Transform[MediaUpperEpsilon[Media], rotn], Transform[MediaUpperMu[Media], rotn], Transform[MediaUpperRo[Media], rotn], Transform[MediaLowerEpsilon[Media], rotn], Transform[MediaLowerMu[Media], rotn], Transform[MediaLowerRo[Media], rotn]];
      (*Print["!!! Check Media Flip !!!"];*)
      (* Print["MediaFlip::flpMedia = ", flpMedia]; *)
      Return[flpMedia];
    ];
(* ============================================== *)
MediaUpperRefractionIndex[Media_] := Module[{}, Return[Media[[1]]]];
MediaLowerRefractionIndex[Media_] := Module[{}, Return[Media[[2]]]];
MediaGamma[Media_] := Module[{}, Return[Media[[3]]]];
MediaFilm[Media_] := Module[{}, Return[Media[[4]]]];
MediaFilmLength[Media_] := Module[{}, Return[Length[Media[[4]]]];];
MediaDescription[Media_] := Module[{}, Return[Media[[5]]]];
MediaOutRefractionIndex[Media_] := Module[{}, Return[Media[[6]]]];
MediaSubstrateThickness[Media_] := Module[{}, Return[Media[[7]]]];

MediaUpperEpsilon[Media_] := Module[{}, Return[Media[[8, 1]]]];
MediaUpperMu[Media_] := Module[{}, Return[Media[[8, 2]]]];
MediaUpperRo[Media_] := Module[{}, Return[Media[[8, 3]]]];
MediaUpperRoT[Media_] := Module[{}, Return[Media[[8, 4]]]];

MediaLowerEpsilon[Media_] := Module[{}, Return[Media[[8, 5]]]];
MediaLowerMu[Media_] := Module[{}, Return[Media[[8, 6]]]];
MediaLowerRo[Media_] := Module[{}, Return[Media[[8, 7]]]];
MediaLowerRoT[Media_] := Module[{}, Return[Media[[8, 8]]]];
(* ============================================== *)
(* ============================================== *)
IncidentLightNew[lambda_, fita_, beta_, n1_, Ampl_, ellipticity_] :=
    Module[{EHI, ehField1, ehField2, ehField, iLight1, iLight2, ampl1, ampl2, ellp},
      ellp = Max[Min[ellipticity, 1], -1];
      ampl1 = Ampl / Sqrt[1 + ellp^2];
      ampl2 = Ampl * ellp / Sqrt[1 + ellp^2];

      iLight1 = IncidentLightNew[lambda, fita, beta, n1, ampl1];
      ehField1 = Table[iLight1[[4]][[iii]], {iii, 1, 6}];

      iLight2 = IncidentLightNew[lambda, fita, beta + Pi / 2, n1, ampl2];
      ehField2 = Table[I * iLight2[[4]][[iii]], {iii, 1, 6}];

      ehField = ehField1 + ehField2;

      EHI = {ehField[[1]], ehField[[2]], ehField[[3]], ehField[[4]], ehField[[5]], ehField[[6]], True};
      Return[{lambda, fita, beta, EHI, ellipticity, n1, Ampl}];
    ];

IncidentLightNew[lambda_, fita_, beta_, n1_, Ampl_] :=
    Module[{EHI},
      EHI = {Ampl Cos[beta] Cos[fita], Ampl Sin[beta], -Ampl Cos[beta] Sin[fita], -Ampl n1 Cos[fita] Sin[beta], Ampl n1 Cos[beta], Ampl n1 Sin[beta] Sin[fita], True};
      Return[{lambda, fita, beta, EHI, 0, n1, Ampl}];
    ];

IncidentLightNew[lambda_, fita_, beta_, n1_] :=
    Module[{retval},
      retval = IncidentLightNew[lambda, fita, beta, n1, 1];
      Return[retval];
    ];

IncidentLightNew[lambda_, fita_, beta_, eh : {_, _, _, _, _, _, _}] :=
    Module[{},
      Return[{lambda, fita, beta, eh, 0, Indeterminate, Indeterminate}];
    ];

IncidentLightFlip[inclght_] := Module[{flpLght},
  flpLght = {inclght[[1]], inclght[[2]], inclght[[3]], EHFlip[inclght[[4]]], inclght[[5]], inclght[[6]]};
  Return[flpLght];
];

(* ============================================== *)
IncidentLightLambda[IncidentLight_] := IncidentLight[[1]];
IncidentLightFita[IncidentLight_] := IncidentLight[[2]];
IncidentLightBeta[IncidentLight_] := IncidentLight[[3]];
IncidentLightIsDown[IncidentLight_] := IncidentLight[[4, 7]];
IncidentLightEH[IncidentLight_] := IncidentLight[[4]];
IncidentLightEllipticity[IncidentLight_] := IncidentLight[[5]];
IncidentLighRefrIndex[IncidentLight_] := IncidentLight[[6]];
IncidentLighAmplitude[IncidentLight_] := IncidentLight[[7]];
(* ============================================== *)
(* ============================================== *)
If[$VersionNumber < 10.0,
  (
    Print["Mathematica version is ", $VersionNumber, " Assigning OFULL via AppendColumns."];
    OFULL[f_][x_, y_, z_] :=
        AppendColumns[
          Transpose[{Curl[{(f[x, y, z])[[4, 1]], (f[x, y, z])[[5, 1]], (f[x, y, z])[[6, 1]]}, Cartesian[x, y, z]]}],
          -Transpose[{Curl[{(f[x, y, z])[[1, 1]], (f[x, y, z])[[2, 1]], (f[x, y, z])[[3, 1]]}, Cartesian[x, y, z]]}]];
  ),
  (
    Print["Mathematica version is ", $VersionNumber, " Assigning OFULL via Join."];
    OFULL[f_][x_, y_, z_] :=
        Transpose[
          {
            Join[
              Curl[{(f[x, y, z])[[4, 1]], (f[x, y, z])[[5, 1]], (f[x, y, z])[[6, 1]]}, Cartesian[x, y, z]],
              -Curl[{(f[x, y, z])[[1, 1]], (f[x, y, z])[[2, 1]], (f[x, y, z])[[3, 1]]}, Cartesian[x, y, z]]
            ]
          }
        ];
  )
];

(* ============================================== *)
MMM[eps_, mu_, ro_, rotr_, lambda_, fita_, n1_] :=
    Module[{kx, MaxwellEq, EH, lst3, lst6, sol, xxxNoz, xNoz4, EH4, EH4f, EH4d, EH4df, Dl, Dld, MM, MMd, MMdInv, retval},
    (*
    Print["MMM"];
    Print["eps = ", MatrixForm[N[eps]]];
    Print["mu = ", MatrixForm[N[mu]]];
    Print["ro = ", MatrixForm[ro]];
    Print["rotr = ", MatrixForm[rotr]];
    *)

      EH[x_, y_, z_] := Exp[I * kx * x] * {{Ex0[z]}, {Ey0[z]}, {Ez0[z]}, {Hx0[z]}, {Hy0[z]}, {Hz0[z]}};
      (*Print["EH[x,y,z] = ",EH[x,y,z]];*)
      MaxwellEq = (((I * (Exp[(-I * kx * x)] * ((2 * Pi * I / lambda) * M[eps, mu, ro, rotr].EH[x, y, z] + OFULL[EH][x, y, z]))))) /. x -> 0;
      kx = (2 * Pi / lambda) * n1 * Sin[fita];

      (*
      Print["kx = ",N[kx]];Print["n1 = ",N[n1]];
      Print["fita = ",N[fita]];
      Print["MaxwellEq = ",MatrixForm[Simplify[N[MaxwellEq]]]];
      *)

      lst3 = CoefficientList[MaxwellEq[[3, 1]], {Ez0[z], Hz0[z]}];
      lst6 = CoefficientList[MaxwellEq[[6, 1]], {Ez0[z], Hz0[z]}];

      If[UseQuietSolveValue,
        (
          sol = (Quiet[Solve[{MaxwellEq[[3, 1]] == 0, MaxwellEq[[6, 1]] == 0}, {Ez0[z], Hz0[z]}]])[[1]];
        ),
        (
          sol = (Solve[{MaxwellEq[[3, 1]] == 0, MaxwellEq[[6, 1]] == 0}, {Ez0[z], Hz0[z]}])[[1]];
        )
      ];

      xxxNoz = MaxwellEq /. sol;
      xNoz4 = {{xxxNoz[[1, 1]]}, {xxxNoz[[2, 1]]}, {xxxNoz[[4, 1]]}, {xxxNoz[[5, 1]]}};
      EH4 = {{Ex0[z]}, {Hy0[z]}, {Ey0[z]}, {Hx0[z]}};
      EH4f = {{Ex0[z]}, {Hy0[z]}, {Ey0[z]}, {-Hx0[z]}};
      EH4d = {{Derivative[1][Ex0][z]}, {Derivative[1][Hy0][z]}, {Derivative[1][Ey0][z]}, {Derivative[1][Hx0][z]}};
      EH4df = {{Derivative[1][Ex0][z]}, {Derivative[1][Hy0][z]}, {Derivative[1][Ey0][z]}, {-Derivative[1][Hx0][z]}};
      Dl[q_, s_] := (-Coefficient[xNoz4[[q, 1]], EH4[[s, 1]]] * If[s == 4, -1, 1]);
      Dld[q_, s_] := (Coefficient[xNoz4[[q, 1]], EH4d[[s, 1]]] * If[s == 4, -1, 1]);
      MM = Table[Dl[i, j], {i, 4}, {j, 4}];
      MMd = Table[Dld[i, j], {i, 4}, {j, 4}];
      MMdInv = Inverse[MMd];
      retval = {(MMdInv.MM) / (2 * Pi * I / lambda), sol};
      Print["MMM retval = ", N[retval]];
      Return[retval];
    ];
(* ============================================== *)
PPP[eps_, mu_, ro_, rotr_, lambda_, fita_, n1_, h_] :=
    Module[{pDet, retval, bCalcErr, dummy},
    (*
    Print["PPP Started."];
    Print["MMM in PPP"];
    Print[MatrixForm[Chop[N[MMM[eps, mu, ro, rotr, lambda, fita, n1][[1]]]]]];
    Print["eps in PPP"];Print[MatrixForm[Chop[N[eps]]]];
    Print["mu in PPP"];Print[MatrixForm[Chop[N[mu]]]];
    Print["ro in PPP"];Print[MatrixForm[Chop[N[ro]]]];
    Print["rotr in PPP"];Print[MatrixForm[Chop[N[rotr]]]];
    *)
      bCalcErr = False;
      retval = MatrixExp[((2 * Pi * I / lambda) * h * MMM[eps, mu, ro, rotr, lambda, fita, n1][[1]])];
      pDet = Abs[Det[retval]];
      If[pDet >= BCMAXPPPALLOWEDVALUE, bCalcErr = True;
      If[BCSUPPRESSERRORMESSAGES =!= True, Print["! OVERFLOW IN PPP ! ", "Det[MatrixExp[...]] = ", Chop[N[pDet]]]]];
      If[pDet <= BCMINPPPALLOWEDVALUE, bCalcErr = True;
      If[BCSUPPRESSERRORMESSAGES =!= True, Print["! UNDERFLOW IN PPP ! ", "Det[MatrixExp[...]] = ", Chop[N[pDet]]]]];
      If[bCalcErr === True && BCZEROPPPONCALCERR === True, retval = deltaZero, dummy = 0];
      (*Print["PPP Ended."];*)
      (*Print["Det[MatrixExp[...]] = ",Chop[N[pDet]]];*)

      Print["PPP::retval = ", retval // toFSharpMatrix, ", pDet = ", pDet];

      Return[retval];
    ];

PPPFull[Film_, lambda_, fita_, n1_] :=
    Module[{len, cnt, retval, eps, ro, rotr, mu, h, FilmLayer},
      len = FilmLength[Film];
      retval = II;

      Do[
        FilmLayer = Film[[cnt]];
        h = FilmLayerThickness[FilmLayer];
        eps = FilmLayerEpsilon[FilmLayer];
        mu = FilmLayerMu[FilmLayer];
        ro = FilmLayerRo[FilmLayer];
        rotr = FilmLayerRoT[FilmLayer];

        retval = PPP[eps, mu, ro, rotr, lambda, fita, n1, h] . retval
        , {cnt, 1, len}
      ];

      pDet = Abs[Det[retval]];
      Print["PPPFull::retval = ", retval // toFSharpMatrix, ", pDet = ", pDet];
      Return[retval];
    ];
(* ============================================== *)
(*Energy Density Vector:P=(c/(16*Pi)*(E+Conj[E]) x (H+Conj[H])) - vecror multiplication - !!! CHECK - THIS MAY BE INCORRECT !!!*)
(* ============================================== *)
EGGetOrder[egvl : {_, _, _, _}, egvec_, ehStdRule_, opts___] :=
    Module[{odeg, ODEGhlp, EGValHlpRe, EGValHlpIm, EGUnit, egvReShft, egvRe, egMult, egShft, idxHlp, egvIm, swpev, ehf, pntgS, egMultPng, prec, chpPrec, AddOnSrt, pntgTblX, pntgTblY, pntgTbl},
      Print["EGGetOrder::egvl =", N[egvl] // MatrixForm];
      Print["EGGetOrder::egvec =", N[egvec] // MatrixForm];
      EGUnit = {1, 1, 1, 1};
      egvReShft = 0;
      egMult = 10^6;
      egMultPng = 10^3;
      egShft = 10^3;
      prec = 6;
      chpPrec = 10^-6;
      pntgTbl = Table[PoyntingS[GetEHFull[egvec[[i]], ehStdRule, True], 3], {i, 4}];
      pntgTblX = Table[PoyntingS[GetEHFull[egvec[[i]], ehStdRule, True], 1], {i, 4}];
      pntgTblY = Table[PoyntingS[GetEHFull[egvec[[i]], ehStdRule, True], 2], {i, 4}];
      (*Print["egvl = ",MFCN[egvl]];Print["egvec = ",MFCN[egvec]];
    Print["ehStdRule = ",MFCN[ehStdRule]];
    Print["pntgTbl = ",MFCN[pntgTbl]];
    Print["pntgTblX = ",MFCN[pntgTblX]];
    Print["pntgTblY = ",MFCN[pntgTblY]];*)
      swpev = SwapEigenValues /. opts /. Options[BerremanCommon];
      AddOnSrt = AddOnEigenValuesSort /. opts /. Options[BerremanCommon];
      EGValHlpRe = Sign[Re[egvl]];
      EGValHlpIm = Sign[Im[egvl]];
      (*Print["EGValHlpRe = ",MFCN[EGValHlpRe]];
    Print["EGValHlpIm = ",MFCN[EGValHlpIm]];*)

      ODEGhlp = egMult * SetPrecision[Chop[Im[egvl], chpPrec], prec] + egMultPng * SetPrecision[Chop[pntgTbl, chpPrec], prec] + SetPrecision[Chop[Re[egvl], chpPrec], prec];

      (*Print["ODEGhlp = ",MFCN[ODEGhlp]];*)
      odeg = {Ordering[ODEGhlp, 2], Ordering[ODEGhlp, -2]};
      egvRe = SetPrecision[Chop[Re[egvl], chpPrec], prec];
      egvIm = SetPrecision[Chop[Im[egvl], chpPrec], prec];
      (* The following lines were added to "glue" eigenvalues in version 3.03.023/024 & modified in 3.04.33 *)If[swpev === True, If[(egvIm[[odeg[[1, 1]]]] == 0) && (egvIm[[odeg[[1, 2]]]] == 0) && (egvRe[[odeg[[1, 1]]]] < egvRe[[odeg[[1, 2]]]]), idxHlp = odeg[[1, 1]];odeg[[1, 1]] = odeg[[1, 2]];odeg[[1, 2]] = idxHlp, If[(egvIm[[odeg[[1, 1]]]] != 0) && (egvIm[[odeg[[1, 2]]]] != 0) && (egvRe[[odeg[[1, 1]]]] < egvRe[[odeg[[1, 2]]]]), idxHlp = odeg[[1, 1]];odeg[[1, 1]] = odeg[[1, 2]];odeg[[1, 2]] = idxHlp]];
      If[(egvIm[[odeg[[2, 1]]]] == 0) && (egvIm[[odeg[[2, 2]]]] == 0) && (egvRe[[odeg[[2, 1]]]] < egvRe[[odeg[[2, 2]]]]), idxHlp = odeg[[2, 1]];odeg[[2, 1]] = odeg[[2, 2]];odeg[[2, 2]] = idxHlp, If[(egvIm[[odeg[[2, 1]]]] != 0) && (egvIm[[odeg[[2, 2]]]] != 0) && (egvRe[[odeg[[2, 1]]]] < egvRe[[odeg[[2, 2]]]]), idxHlp = odeg[[2, 1]];odeg[[2, 1]] = odeg[[2, 2]];odeg[[2, 2]] = idxHlp]];];
      (* Print["odeg = ",MFCN[odeg]];*)(*The following lines were added to additionally swap 1/2 and 3/4 eigenvalues in version 3.05.034 *)If[AddOnSrt === 1, If[(pntgTblX[[odeg[[1, 1]]]] < pntgTblX[[odeg[[1, 2]]]]), idxHlp = odeg[[1, 1]];odeg[[1, 1]] = odeg[[1, 2]];odeg[[1, 2]] = idxHlp, idxHlp = 0];];
      If[AddOnSrt === 1, If[(pntgTblX[[odeg[[2, 1]]]] < pntgTblX[[odeg[[2, 2]]]]), idxHlp = odeg[[2, 1]];odeg[[2, 1]] = odeg[[2, 2]];odeg[[2, 2]] = idxHlp, idxHlp = 0];];
      (*Print["egvl = ",MFCN[egvl]];*)(*Print["Retval odeg = ",MFCN[odeg]];*)
      Return[odeg];
    ];
(* ============================================== *)
SolutionNew[Media_, IncidentLight_, optsRaw___] :=
    Module[{retVal, opts, cbeta, retValBeta0, retValBeta90, inclBeta0, inclBeta90, lambda, fita, n1},
      opts = Flatten[{optsRaw}];
      cbeta = CalculateBeta0and90 /. opts /. Options[BerremanCommon];
      (*
    Print["SolutionNew::optsRaw = ", optsRaw];
    Print["SolutionNew::cbeta = ", cbeta];
    *)
      retVal = SolutionNewBase[Media, IncidentLight, optsRaw];

      If[cbeta,
        (
        (* Print["SolutionNew::Processing cbeta."]; *)
          lambda = IncidentLightLambda[IncidentLight];
          fita = IncidentLightFita[IncidentLight];
          n1 = MediaUpperRefractionIndex[Media];
          inclBeta0 = IncidentLightNew[lambda, fita, 0, n1];
          inclBeta90 = IncidentLightNew[lambda, fita, Pi / 2, n1];

          retValBeta0 = SolutionNewBase[Media, inclBeta0, optsRaw];
          retValBeta90 = SolutionNewBase[Media, inclBeta90, optsRaw];
          (* Print["SolutionNew::retVal (old) = ", retVal]; *)
          retVal = Join[retVal, {{retValBeta0, retValBeta90}}];
        (* Print["SolutionNew::retVal = ", retVal]; *)
        )
      ];

      Return[retVal];
    ];
(* ============================================== *)
SolutionNewBase[Media_, IncidentLight_, optsRaw___] :=
    Module[{eps, ro, rotr, mu, eps2, ro2, rotr2, mu2, lambda, fita, n1, n2, beta, gamm, kx, Film, ehIncd, opts, calcBS, len, EHIv, EHRv, EHTv, ehr1, ehr2, eht1, eht2, ssss, delta, calcDlt, FilmLayer, EI, EP, ES, MMM1, MMM2, h2, PPPm, PPPmm, outPPPm, useSolve, sss1, sss2, cf, b, cmf, sol, varLst, free, coeffTbl, freeTbl, s1, s2, s3, s4, EGVal1Hlp, EGVal2Hlp, ODEG1hlp, EGVal1HlpIm, EGVal1HlpRe, ODEG2hlp, EGVal2HlpIm, EGVal2HlpRe, EGUnit, egs1, egs2, egvf1, egvf2, EHTFullEG1, EHTFullEG2, EHTFullEG3, EHTFullEG4, tmp, egvf1Tr, egvf2Tr, useNumEV, pdi, pdil, ehirule, ehrrule, ehtrule, EGVal1, EGVec1, ODEG1, EGVal1Up, EGVal1Dn, EGVec1Up, EGVec1Dn, EGVal2, EGVec2, ODEG2, EGVal2Up, EGVec2Up, EGVal2Dn, EGVec2Dn, ehi1, ehi2, ehisol, cfm, EHIcoeff, EHI, PPPv, coeff, ehrtsol, EHRcoeff, EHR, EHTcoeff, EHT, ehrule, EHIFull, EHTFull, EHRFull, retval(* ,z,Ex0,Ey0,Ez0,Hx0,Hy0,Hz0 *)},

      opts = Flatten[{optsRaw}];
      pdi = PrintCommonDebugInfo /. opts /. Options[BerremanCommon];
      pdil = PrintCommonDebugInfoLevel /. opts /. Options[BerremanCommon];

      If[pdi == True,
        Print["   "];
        Print["SolutionNewBase::start ================================================="];
      ];

      n1 = MediaUpperRefractionIndex[Media];
      n2 = MediaLowerRefractionIndex[Media];
      Film = MediaFilm[Media];
      len = FilmLength[Film];
      gamm = MediaGamma[Media];
      lambda = IncidentLightLambda[IncidentLight];
      fita = IncidentLightFita[IncidentLight];
      beta = IncidentLightBeta[IncidentLight];
      EGUnit = {1, 1, 1, 1};
      calcBS = CalculateBoundarySolution /. opts /. Options[BerremanCommon];
      If[len === 0, calcBS = True];
      calcDlt = CalculateDelta /. opts /. Options[BerremanCommon];
      outPPPm = OutputPPPMultiplier /. opts /. Options[BerremanCommon];
      useSolve = UseSolveInSolutionNew /. opts /. Options[BerremanCommon];
      useNumEV = UseNumericEigenValues /. opts /. Options[BerremanCommon];
      UseQuietSolveValue = UseQuietSolve /. opts /. Options[BerremanCommon];

      kx = (2 * Pi / lambda) * n1 * Sin[fita];
      delta = deltaZero;
      kx = SetAccuracy[kx, 100];

      (* PCDILEVELALL; PCDILEVELDETAILED; PCDILEVELMEDIUM; PCDILEVELSHORT; *)
      If[pdi == True && pdil >= PCDILEVELDETAILED,
        Print["SolutionNewBase:: Media = ", Media];
        Print["SolutionNewBase:: lambda = ", N[lambda / nm], ", beta = ", N[beta / Degree], ", fita = ", N[fita / Degree], ", gamm = ", N[gamm / Degree]];
        Print["SolutionNewBase:: n1 = ", N[n1], ", n2 = ", N[n2]];
        Print["SolutionNewBase:: MediaUpperEpsilon[Media] = ", MediaUpperEpsilon[Media] // MatrixForm, ", MediaLowerEpsilon[Media] = ", MediaLowerEpsilon[Media] // MatrixForm];
      ];

      (* Print["outPPPm = ",N[outPPPm]];Print["kx = ",N[kx]]; *)
      MMM1 = MMM[MediaUpperEpsilon[Media], MediaUpperMu[Media], MediaUpperRo[Media], MediaUpperRoT[Media], lambda, fita, n1];

      (* PCDILEVELALL; PCDILEVELDETAILED; PCDILEVELMEDIUM; PCDILEVELSHORT; *)
      If[pdi == True && pdil >= PCDILEVELDETAILED,
        Print["SolutionNewBase::MMM1 = ", Chop[N[MMM1]]];
      ];

      MMM2 = MMM[MediaLowerEpsilon[Media], MediaLowerMu[Media], MediaLowerRo[Media], MediaLowerRoT[Media], lambda, fita, n1];

      If[pdi == True && pdil >= PCDILEVELDETAILED,
        Print["SolutionNewBase::MMM2 = ", Chop[N[MMM2]]];
      ];

      ehirule = MMM1[[2]];
      ehrrule = ehirule;
      ehtrule = MMM2[[2]];

      (* PCDILEVELALL; PCDILEVELDETAILED; PCDILEVELMEDIUM; PCDILEVELSHORT; *)
      If[pdi == True && pdil >= PCDILEVELDETAILED,
        Print["SolutionNewBase::ehirule = ", Chop[N[Simplify[ehirule]]]];
        Print["SolutionNewBase::ehtrule = ", Chop[N[Simplify[ehtrule]]]];
      ];

      If[useNumEV === True, EGVal1 = Eigenvalues[N[MMM1[[1]]]], EGVal1 = Eigenvalues[MMM1[[1]]]];
      If[pdi == True && pdil >= PCDILEVELDETAILED,
        Print["SolutionNewBase::EGVal1 = ", MatrixForm[EGVal1]];
      ];

      If[useNumEV === True, EGVec1 = Eigenvectors[N[MMM1[[1]]]], EGVec1 = Eigenvectors[MMM1[[1]]]];
      If[pdi == True && pdil >= PCDILEVELALL,
        Print["SolutionNewBase::MMM1[[1]] = ", MatrixForm[Chop[N[MMM1[[1]]]]]];
        Print["SolutionNewBase::EGVec1 = ", MatrixForm[EGVec1]];
      ];

      EGVal1Hlp = Chop[N[EGVal1]];
      Print["ODEG1"];
      ODEG1 = EGGetOrder[EGVal1Hlp, EGVec1, ehirule, opts];
      EGVal1Up = EGVal1[[ODEG1[[1]]]];
      EGVal1Dn := EGVal1[[ODEG1[[2]]]];
      EGVec1Up := Transpose[(EGVec1[[ODEG1[[1]]]])];
      EGVec1Dn := Transpose[(EGVec1[[ODEG1[[2]]]])];

      If[pdi == True && pdil >= PCDILEVELALL,
        Print["SolutionNewBase::ODEG1 = ", ODEG1];
        Print["SolutionNewBase::ODEG1 Ended..."];
      ];

      If[useNumEV === True, EGVal2 = Eigenvalues[N[MMM2[[1]]]], EGVal2 = Eigenvalues[MMM2[[1]]]];
      (* PCDILEVELALL; PCDILEVELDETAILED; PCDILEVELMEDIUM; PCDILEVELSHORT; *)
      If[pdi == True && pdil >= PCDILEVELDETAILED,
        Print["SolutionNewBase::EGVal2 = ", MatrixForm[Chop[N[EGVal2]]]];
      ];

      If[useNumEV === True, EGVec2 = Eigenvectors[N[MMM2[[1]]], Quartics -> True], EGVec2 = Eigenvectors[MMM2[[1]], Quartics -> True]];
      (* Print["MMM2[[1]] = ", MatrixForm[Chop[N[MMM2[[1]]]]]]; Print["EGVec2 = ", MatrixForm[Chop[N[EGVec2]]]]; *)

      EGVal2Hlp = Chop[N[EGVal2]];
      Print["ODEG2"];
      ODEG2 = EGGetOrder[EGVal2Hlp, EGVec2, ehtrule, opts];
      EGVal2Up = EGVal2[[ODEG2[[1]]]];
      EGVal2Dn = EGVal2[[ODEG2[[2]]]];
      EGVec2Up = Transpose[(EGVec2[[ODEG2[[1]]]])];
      EGVec2Dn = Transpose[(EGVec2[[ODEG2[[2]]]])];

      (* PCDILEVELALL; PCDILEVELDETAILED; PCDILEVELMEDIUM; PCDILEVELSHORT; *)
      If[pdi == True && pdil >= PCDILEVELDETAILED,
        Print["SolutionNewBase::=================="];
        Print["MMM1 = ", MatrixForm[Chop[N[MMM1[[1]]]]]];
        Print["MMM2 = ", MatrixForm[Chop[N[MMM2[[1]]]]]];
        Print["EGVec1 = ", MFCN[EGVec1]];
        Print["EGVal1Hlp = ", MatrixForm[EGVal1Hlp]];
        Print["EGVal1 = ", MatrixForm[EGVal1]];
        Print["ODEG1 = ", MFCN[ODEG1]];
        Print["EGVal1Up = ", MFCN[EGVal1Up]];
        Print["EGVal1Dn = ", MFCN[EGVal1Dn]];
        Print["EGVec1Up = ", MFCN[EGVec1Up]];
        Print["EGVec1Dn = ", MFCN[EGVec1Dn]];
        Print["EGVec2 = ", MFCN[EGVec2]];
        Print["EGVal2Hlp = ", MatrixForm[EGVal2Hlp]];
        Print["EGVal2 = ", MatrixForm[EGVal2]];
        Print["ODEG2hlp", ODEG2hlp];
        Print["ODEG2 = ", MFCN[ODEG2]];
        Print["EGVec2Up = ", MFCN[EGVec2Up]];
        Print["EGVec2Dn = ", MFCN[EGVec2Dn]];
        Print["EGVec2Up (full) = ", EGVec2Up];
        Print["EGVec2Dn (full) = ", EGVec2Dn];
        Print["==================::SolutionNewBase"];
        Print["   "];
      ];

      (* PCDILEVELALL; PCDILEVELDETAILED; PCDILEVELMEDIUM; PCDILEVELSHORT; *)
      If[pdi == True && pdil >= PCDILEVELMEDIUM,
        Print["SolutionNewBase::IncidentLight = ", Chop[N[IncidentLight]]];
      ];

      ehIncd = IncidentLightEH[IncidentLight];
      EI = {{ehIncd[[1]]}, {ehIncd[[5]]}, {ehIncd[[2]]}, {-ehIncd[[4]]}};

      (* PCDILEVELALL; PCDILEVELDETAILED; PCDILEVELMEDIUM; PCDILEVELSHORT; *)
      If[pdi == True && pdil >= PCDILEVELDETAILED,
        Print["SolutionNewBase::ehIncd = ", Chop[N[ehIncd]]];
        Print["SolutionNewBase::EI = ", Chop[N[EI]]];
      ];

      (*EI=SetAccuracy[EI,100];*)

      Clear[ehi1, ehi2, ssss];
      ssss = (EGVec1Dn.{{ehi1}, {ehi2}} - EI);

      (* PCDILEVELALL; PCDILEVELDETAILED; PCDILEVELMEDIUM; PCDILEVELSHORT; *)
      If[pdi == True && pdil >= PCDILEVELALL,
        Print["SolutionNewBase::EGVec1Dn", N[EGVec1Dn]];Print["EGVec1Up", N[EGVec1Up]];
        Print["SolutionNewBase::EGVal1Dn = ", N[EGVal1Dn]];Print["EGVal1Up = ", N[EGVal1Up]];
        Print["SolutionNewBase::ssss = ", MatrixForm[Chop[N[ssss]]]];
        Print["SolutionNewBase::ssss[[1,1]] = ", MatrixForm[Chop[N[ssss[[1, 1]]]]]];
        Print["SolutionNewBase::ssss[[3,1]] = ", MatrixForm[Chop[N[ssss[[3, 1]]]]]];
      ];

      If[useSolve === True,
        (
        (*Fuck Wolfram. The code below DOES NOT WORK sometimes!!! So we have to replace it.*)
          If[UseQuietSolveValue,
            (
              ehisol = (Quiet[Solve[{ssss[[1, 1]] == 0, ssss[[3, 1]] == 0}, {ehi1, ehi2}]])[[1]];
            ),
            (
              ehisol = (Solve[{ssss[[1, 1]] == 0, ssss[[3, 1]] == 0}, {ehi1, ehi2}])[[1]];
            )
          ];
        ),
        (
          sss1 = ssss[[1, 1]];
          sss2 = ssss[[3, 1]];
          cf = {{Coefficient[sss1, ehi1], Coefficient[sss1, ehi2]}, {Coefficient[sss2, ehi1], Coefficient[sss2, ehi2]}};
          b = -{{sss1 /. ehi1 -> 0 /. ehi2 -> 0}, {sss2 /. ehi1 -> 0 /. ehi2 -> 0}};
          cfm = Inverse[cf];
          sol = cfm.b;
          ehisol = {ehi1 -> sol[[1, 1]], ehi2 -> sol[[2, 1]]};
        )
      ];

      (* PCDILEVELALL; PCDILEVELDETAILED; PCDILEVELMEDIUM; PCDILEVELSHORT; *)
      If[pdi == True && pdil >= PCDILEVELALL,
        Print["SolutionNewBase::ehisol (NEW) = ", N[ehisol]];
      ];

      EHIcoeff = (Clear[ehi1, ehi2];({{ehi1}, {ehi2}} /. ehisol));
      EHI = EGVec1Dn.EHIcoeff;

      If[pdi == True && pdil >= PCDILEVELALL,
        Print["SolutionNewBase::EGVec1Dn = ", EGVec1Dn];
        Print["SolutionNewBase::EHIcoeff = ", EHIcoeff];
        Print["SolutionNewBase::EHI = ", EHI];
      ];
      (* Print["PPPv calc started..."]; *)

      PPPv = If[calcBS === True, II, PPPFull[Film, lambda, fita, n1], II];
      (*
    Print["PPPv calc ended..."];
    Print["PPPv = ",MatrixForm[N[PPPv]]];
    *)
      (*PPPv=SetAccuracy[PPPv,100];*)

      EHIv = EHI;
      EHRv = EGVec1Up.{{ehr1}, {ehr2}};
      EHTv = EGVec2Dn.{{eht1}, {eht2}};
      ssss = (PPPv.(EHIv + EHRv) - EHTv);
      (* ============================================== *)

      (*
    Print["ssss = ",MatrixForm[Chop[N[ssss]]]];
    Print["ssss (full) = ",ssss];
    *)

      varLst = {ehr1, ehr2, eht1, eht2};
      coeff[q_, s_] := Coefficient[ssss[[q, 1]], varLst[[s]]];
      free[q_] := (-ssss[[q, 1]] /. ehr1 -> 0 /. ehr2 -> 0 /. eht1 -> 0 /. eht2 -> 0);
      coeffTbl = Table[coeff[i, j], {i, 4}, {j, 4}];cfm = Inverse[coeffTbl];
      freeTbl = Table[free[i], {i, 4}, {j, 1}];
      (* Print["coeffTbl = ",MatrixForm[Chop[N[coeffTbl]]]]; *)

      If[useSolve === True,
        (
          If[UseQuietSolveValue,
            (
              ehrtsol = (Quiet[Solve[ssss == 0, {ehr1, ehr2, eht1, eht2}]])[[1]];
            ),
            (
              ehrtsol = (Solve[ssss == 0, {ehr1, ehr2, eht1, eht2}])[[1]];
            )
          ];
        ),
        (
          s1 = ssss[[1, 1]];
          s2 = ssss[[2, 1]];
          s3 = ssss[[3, 1]];
          s4 = ssss[[4, 1]];
          (* Print["s1 = ",N[s1],", s2 = ",N[s2],", s3 = ",N[s3],", s4 = ",N[s4]]; *)
          sol = cfm.freeTbl;
          ehrtsol = {ehr1 -> sol[[1, 1]], ehr2 -> sol[[2, 1]], eht1 -> sol[[3, 1]], eht2 -> sol[[4, 1]]};
        )
      ];

      (* PCDILEVELALL; PCDILEVELDETAILED; PCDILEVELMEDIUM; PCDILEVELSHORT; *)
      If[pdi == True && pdil >= PCDILEVELALL,
        Print["SolutionNewBase::ehrtsol (NEW) = ", N[ehrtsol]];
      ];

      (* ============================================== *)
      EHRcoeff = (Clear[ehr1, ehr2];({{ehr1}, {ehr2}} /. ehrtsol));
      EHTcoeff = (Clear[eht1, eht2];({{eht1}, {eht2}} /. ehrtsol));
      EHR = EGVec1Up.EHRcoeff;
      EHT = EGVec2Dn.EHTcoeff;

      (* PCDILEVELALL; PCDILEVELDETAILED; PCDILEVELMEDIUM; PCDILEVELSHORT; *)
      If[pdi == True && pdil >= PCDILEVELALL,
        Print["SolutionNewBase::EHI = ", N[EHI], ", Abs[EHI] = ", N[Abs[EHI]]];
        Print["SolutionNewBase::EHR = ", N[EHR], " Abs[EHR] = ", N[Abs[EHR]]];
        Print["SolutionNewBase::EHT = ", N[EHT], " Abs[EHT] = ", N[Abs[EHT]]];
      ];

      If[outPPPm === 1, h2 = MediaSubstrateThickness[Media];
      PPPm = PPP[MediaLowerEpsilon[Media], MediaLowerMu[Media], MediaLowerRo[Media], MediaLowerRoT[Media], lambda, fita, n1, h2];
      (*
    Print["h2 = ",N[h2]];
    Print["MediaLowerEpsilon = ",MatrixForm[N[MediaLowerEpsilon[Media]]]];
    Print["PPPm = ",MatrixForm[Chop[N[PPPm]]]];
    *)
      EHT = PPPm.EHT;
      ];

      If[outPPPm === -1, h2 = MediaSubstrateThickness[Media];
      PPPm = PPP[MediaUpperEpsilon[Media], MediaUpperMu[Media], MediaUpperRo[Media], MediaUpperRoT[Media], lambda, fita, n1, -h2];
      (*
    Print["h2 = ",h2];
    Print["MediaUpperEpsilon = ",MatrixForm[N[MediaUpperEpsilon[Media]]]];
    Print["PPPm = ",MatrixForm[Chop[N[PPPm]]]];
    *)
      EHR = PPPm.EHR;
      ];

      (* PCDILEVELALL; PCDILEVELDETAILED; PCDILEVELMEDIUM; PCDILEVELSHORT; *)
      If[pdi == True && pdil >= PCDILEVELALL,
        Print["SolutionNewBase::EHR final = ", N[EHR], ", Abs[EHR] final = ", Abs[N[EHR]]];
        Print["SolutionNewBase::EHT final = ", N[EHT], ", Abs[EHT] final = ", Abs[N[EHT]]];
      ];

      ehrule = {Ex0[z] -> EHI[[1, 1]], Ey0[z] -> EHI[[3, 1]], Hx0[z] -> -EHI[[4, 1]], Hy0[z] -> EHI[[2, 1]]};
      EHIFull = {EHI[[1, 1]], EHI[[3, 1]], ((Ez0[z] /. ehirule) /. ehrule), -EHI[[4, 1]], EHI[[2, 1]], ((Hz0[z] /. ehirule) /. ehrule), True};
      ehrule = ({Ex0[z] -> EHT[[1, 1]], Ey0[z] -> EHT[[3, 1]], Hx0[z] -> -EHT[[4, 1]], Hy0[z] -> EHT[[2, 1]]});
      EHTFull = {EHT[[1, 1]], EHT[[3, 1]], ((Ez0[z] /. ehtrule) /. ehrule), -EHT[[4, 1]], EHT[[2, 1]], ((Hz0[z] /. ehtrule) /. ehrule), True};
      ehrule = ({Ex0[z] -> EHR[[1, 1]], Ey0[z] -> EHR[[3, 1]], Hx0[z] -> -EHR[[4, 1]], Hy0[z] -> EHR[[2, 1]]});
      EHRFull = {EHR[[1, 1]], EHR[[3, 1]], ((Ez0[z] /. ehrrule) /. ehrule), -EHR[[4, 1]], EHR[[2, 1]], ((Hz0[z] /. ehrrule) /. ehrule), False};

      (* PCDILEVELALL; PCDILEVELDETAILED; PCDILEVELMEDIUM; PCDILEVELSHORT; *)
      If[pdi == True && pdil >= PCDILEVELALL,
        Print["EHIFull = ", Chop[N[EHIFull]]];
        Print["EHTFull = ", Chop[N[EHTFull]]];
        Print["EHRFull = ", Chop[N[EHRFull]]];
      ];

      If[calcDlt === True && len > 0,
        FilmLayer = Film[[1]];
        eps = FilmLayerEpsilon[FilmLayer];
        mu = FilmLayerMu[FilmLayer];
        ro = FilmLayerRo[FilmLayer];
        rotr = FilmLayerRoT[FilmLayer];
        delta = (MMM[eps, mu, ro, rotr, lambda, fita, n1])[[1]];
      ];

      egvf1 = JoinRight[EGVec1Up, EGVec1Dn];
      egvf2 = JoinRight[EGVec2Up, EGVec2Dn];
      egvf1Tr = Transpose[egvf1];
      egvf2Tr = Transpose[egvf2];
      ehrule = ({Ex0[z] -> EHR[[1, 1]], Ey0[z] -> EHR[[3, 1]], Hx0[z] -> -EHR[[4, 1]], Hy0[z] -> EHR[[2, 1]]});
      tmp = {egvf2[[1, 1]], egvf2[[3, 1]], (Ez0[z]), -egvf2[[4, 1]], egvf2[[2, 1]], (Hz0[z]), True};
      (*  Print["tmp = ",tmp]; *)

      EHTFullEG1 = GetEHFull[egvf2Tr[[1]], ehtrule, True];
      EHTFullEG2 = GetEHFull[egvf2Tr[[2]], ehtrule, True];
      EHTFullEG3 = GetEHFull[egvf2Tr[[3]], ehtrule, True];
      EHTFullEG4 = GetEHFull[egvf2Tr[[4]], ehtrule, True];
      (*EHTFullEG1={egvf2[[1,1]],egvf2[[3,1]],((Ez0[z]/.ehtrule)/.ehrule),-egvf2[[4,1]],egvf2[[2,1]],((Hz0[z]/.ehtrule)/.ehrule),True};
    EHTFullEG2={egvf2[[1,2]],egvf2[[3,2]],((Ez0[z]/.ehtrule)/.ehrule),-egvf2[[4,2]],egvf2[[2,2]],((Hz0[z]/.ehtrule)/.ehrule),True};
    EHTFullEG3={egvf2[[1,3]],egvf2[[3,3]],((Ez0[z]/.ehtrule)/.ehrule),-egvf2[[4,3]],egvf2[[2,3]],((Hz0[z]/.ehtrule)/.ehrule),True};
    EHTFullEG4={egvf2[[1,4]],egvf2[[3,4]],((Ez0[z]/.ehtrule)/.ehrule),-egvf2[[4,4]],egvf2[[2,4]],((Hz0[z]/.ehtrule)/.ehrule),True};*)

      egs1 = {Flatten[{EGVal1Up, EGVal1Dn}], egvf1};
      egs2 = {Flatten[{EGVal2Up, EGVal2Dn}], egvf2, EHTFullEG1, EHTFullEG2, EHTFullEG3, EHTFullEG4};

      (*
    Print["fita = ",Chop[N[fita/Degree]]];
    Print["egs1 = ",Chop[N[egs1]]];Print["egs2 = ",Chop[N[egs2]]];
    *)

      retval = {EHIFull, EHRFull, EHTFull, PPPv, delta, Media, IncidentLight, opts, MMM1[[1]], MMM2[[1]], coeffTbl, freeTbl, egs1, egs2};

      (* PCDILEVELALL; PCDILEVELDETAILED; PCDILEVELMEDIUM; PCDILEVELSHORT; *)
      If[pdi == True && pdil >= PCDILEVELALL,
        Print["SolutionNewBase::retval = ", retval];
      ];

      If[pdi == True ,
        Print["SolutionNewBase::end ================================================="];
        Print["   "];
      ];
      Return[retval];
    ];
(* ============================================== *)
GetEHFull[eh_, ehStdRule_, dwn_] :=
    Module[{ehf, ehrule (* ,Ex0,Ey0,Ez0,Hx0,Hy0,Hy0*)},
      ehrule = ({Ex0[z] -> eh[[1]], Ey0[z] -> eh[[3]], Hx0[z] -> -eh[[4]], Hy0[z] -> eh[[2]]});
      ehf = {eh[[1]], eh[[3]], ((Ez0[z] /. ehStdRule /. ehrule)), -eh[[4]], eh[[2]], ((Hz0[z] /. ehStdRule /. ehrule)), dwn};
      Return[ehf]
    ];
(* ============================================== *)
PoyntingS[ehFld_, fldIdx_] :=
    Module[{retval, eFld, hFld, pntgS},
      eFld = {ehFld[[1]], ehFld[[2]], ehFld[[3]]};
      hFld = {ehFld[[4]], ehFld[[5]], ehFld[[6]]};
      pntgS = Re[Cross[eFld, Conjugate[hFld]]];
      retval = pntgS[[fldIdx]];
      Return[retval];
    ];
(* ============================================== *)
GetEfromEH[eh_] := Module[{}, Return[{{eh[[1]]}, {eh[[2]]}, {eh[[3]]}}];];
GetHfromEH[eh_] := Module[{}, Return[{{eh[[4]]}, {eh[[5]]}, {eh[[6]]}}];];
(* ============================================== *)
GetSolIncidentLight[sol_] := Module[{}, Return[sol[[1]]];];
GetSolReflectedLight[sol_] := Module[{}, Return[sol[[2]]];];
GetSolTransmittedLight[sol_] := Module[{}, Return[sol[[3]]];];
GetSolPPP[sol_] := Module[{}, Return[sol[[4]]];];
GetSolDelta[sol_] := Module[{}, Return[sol[[5]]];];
GetSolMedia[sol_] := Module[{}, Return[sol[[6]]];];
GetSolIncidentLightInfo[sol_] := Module[{}, Return[sol[[7]]];];
GetSolOptions[sol_] := Module[{}, Return[sol[[8]]];];
GetSolM1[sol_] := Module[{}, Return[sol[[9]]];];
GetSolM2[sol_] := Module[{}, Return[sol[[10]]];];
GetSolCoeff[sol_] := Module[{}, Return[sol[[11]]];];
GetSolFreeTerm[sol_] := Module[{}, Return[sol[[12]]];];
GetSolEGSys1[sol_] := Module[{}, Return[sol[[13]]];];
GetSolEGSys2[sol_] := Module[{}, Return[sol[[14]]];];
GetSolEHTEG[sol_, idx_] := Module[{egs}, egs = GetSolEGSys2[sol];Return[egs[[idx + 2]]]];
(* ============================================== *)
GetSolBeta0Sol[sol_] :=
    Module[{solRet},
      solRet = Indeterminate;
      If[Length[sol] >= 15, solRet = sol[[15, 1]]];
      Return[solRet];
    ];
(* ============================================== *)
GetSolBeta90Sol[sol_] :=
    Module[{solRet},
      solRet = Indeterminate;
      If[Length[sol] >= 15, solRet = sol[[15, 2]]];
      Return[solRet];
    ];
(* ============================================== *)
GetSolIncidentLightE[sol_] :=
    Module[{retVal},
    (* Print["GetSolIncidentLightE..."]; *)
      retVal = GetEfromEH[GetSolIncidentLight[sol]];
      (* Print["GetSolIncidentLightE = ", retVal // MatrixForm]; *)
      Return[retVal];
    ];
(* ============================================== *)
GetSolReflectedLightE[sol_] := Module[{}, Return[GetEfromEH[GetSolReflectedLight[sol]]];];
GetSolTransmittedLightE[sol_] := Module[{}, Return[GetEfromEH[GetSolTransmittedLight[sol]]];];

GetSolIncidentLightH[sol_] := Module[{}, Return[GetHfromEH[GetSolIncidentLight[sol]]];];
GetSolReflectedLightH[sol_] := Module[{}, Return[GetHfromEH[GetSolReflectedLight[sol]]];];
GetSolTransmittedLightH[sol_] := Module[{}, Return[GetHfromEH[GetSolTransmittedLight[sol]]];];
(* ============================================== *)
GetSolIncidentLightD[sol_] :=
    Module[{retVal, EHfield, Efield, Dfield, Hfield, Bfield, media, eps, mu, ro, roT},
    (* Print["GetSolIncidentLightD..."]; *)
      media = GetSolMedia[sol];

      EHfield = GetSolIncidentLight[sol];
      eps = MediaUpperEpsilon[media];
      mu = MediaUpperMu[media];
      ro = MediaUpperRo[media];
      roT = MediaUpperRoT[media];

      Efield = GetEfromEH[EHfield];
      Hfield = GetHfromEH[EHfield];
      Dfield = eps . Efield + ro . Hfield;
      Bfield = roT . Efield + mu . Hfield;

      Return[Dfield];
    ];
(* ============================================== *)
GetSolReflectedLightD[sol_] :=
    Module[{retVal, EHfield, Efield, Dfield, Hfield, Bfield, media, eps, mu, ro, roT},
    (* Print["GetSolReflectedLightD..."]; *)
      media = GetSolMedia[sol];

      EHfield = GetSolReflectedLight[sol];
      eps = MediaUpperEpsilon[media];
      mu = MediaUpperMu[media];
      ro = MediaUpperRo[media];
      roT = MediaUpperRoT[media];

      Efield = GetEfromEH[EHfield];
      Hfield = GetHfromEH[EHfield];
      Dfield = eps . Efield + ro . Hfield;
      Bfield = roT . Efield + mu . Hfield;

      Return[Dfield];
    ];
(* ============================================== *)
GetSolTransmittedLightD[sol_] :=
    Module[{retVal, EHfield, Efield, Dfield, Hfield, Bfield, media, eps, mu, ro, roT},
    (* Print["GetSolTransmittedLightD..."]; *)
      media = GetSolMedia[sol];

      EHfield = GetSolTransmittedLight[sol];
      eps = MediaLowerEpsilon[media];
      mu = MediaLowerMu[media];
      ro = MediaLowerRo[media];
      roT = MediaLowerRoT[media];

      Efield = GetEfromEH[EHfield];
      Hfield = GetHfromEH[EHfield];
      Dfield = eps . Efield + ro . Hfield;
      Bfield = roT . Efield + mu . Hfield;

      Return[Dfield];
    ];
(* ============================================== *)
GetSolIncidentLightB[sol_] :=
    Module[{retVal, EHfield, Efield, Dfield, Hfield, Bfield, media, eps, mu, ro, roT},
    (* Print["GetSolIncidentLightB..."]; *)
      media = GetSolMedia[sol];

      EHfield = GetSolIncidentLight[sol];
      eps = MediaUpperEpsilon[media];
      mu = MediaUpperMu[media];
      ro = MediaUpperRo[media];
      roT = MediaUpperRoT[media];

      Efield = GetEfromEH[EHfield];
      Hfield = GetHfromEH[EHfield];
      Dfield = eps . Efield + ro . Hfield;
      Bfield = roT . Efield + mu . Hfield;

      Return[Bfield];
    ];
(* ============================================== *)
GetSolReflectedLightB[sol_] :=
    Module[{retVal, EHfield, Efield, Dfield, Hfield, Bfield, media, eps, mu, ro, roT},
    (* Print["GetSolReflectedLightB..."]; *)
      media = GetSolMedia[sol];

      EHfield = GetSolReflectedLight[sol];
      eps = MediaUpperEpsilon[media];
      mu = MediaUpperMu[media];
      ro = MediaUpperRo[media];
      roT = MediaUpperRoT[media];

      Efield = GetEfromEH[EHfield];
      Hfield = GetHfromEH[EHfield];
      Dfield = eps . Efield + ro . Hfield;
      Bfield = roT . Efield + mu . Hfield;

      Return[Bfield];
    ];
(* ============================================== *)
GetSolTransmittedLightB[sol_] :=
    Module[{retVal, EHfield, Efield, Dfield, Hfield, Bfield, media, eps, mu, ro, roT},
    (* Print["GetSolTransmittedLightB..."]; *)
      media = GetSolMedia[sol];

      EHfield = GetSolTransmittedLight[sol];
      eps = MediaLowerEpsilon[media];
      mu = MediaLowerMu[media];
      ro = MediaLowerRo[media];
      roT = MediaLowerRoT[media];

      Efield = GetEfromEH[EHfield];
      Hfield = GetHfromEH[EHfield];
      Dfield = eps . Efield + ro . Hfield;
      Bfield = roT . Efield + mu . Hfield;

      Return[Bfield];
    ];
(* ============================================== *)
(* retval={EHIFull,EHRFull,EHTFull,PPPv,delta,Media,IncidentLight,opts,MMM1[[1]],MMM2[[1]],coeffTbl,freeTbl,egs1,egs2}; *)
SolutionCombine[ehi : {_, _, _, _, _, _, _}, ehr : {_, _, _, _, _, _, _}, eht : {_, _, _, _, _, _, _}, ppp_, delta_, Media_, IncidentLight_, opts_] :=
    Module[{},
      Return[{ehi, ehr, eht, ppp, delta, Media, IncidentLight, opts, IdentityMatrix[4], IdentityMatrix[4]}];
    ];
(* ============================================== *)
SolutionCombine[ehi : {_, _, _, _, _, _, _}, ehr : {_, _, _, _, _, _, _}, eht : {_, _, _, _, _, _, _}, ppp_, delta_, Media_, IncidentLight_, opts_, mmm11_, mmm21_, coeffTbl_, freeTbl_, egs1_, egs2_] :=
    Module[{},
      Return[{ehi, ehr, eht, ppp, delta, Media, IncidentLight, opts, mmm11, mmm21, coeffTbl, freeTbl, egs1, egs2}];
    ];
(* ============================================== *)
SolutionCombine[ehi : {_, _, _, _, _, _, _}, ehr : {_, _, _, _, _, _, _}, eht : {_, _, _, _, _, _, _}, ppp_, delta_, Media_, IncidentLight_, opts_, mmm11_, mmm21_, coeffTbl_, freeTbl_, egs1_, egs2_, solBeta0_, solBeta90_] :=
    Module[{},
      Return[{ehi, ehr, eht, ppp, delta, Media, IncidentLight, opts, mmm11, mmm21, coeffTbl, freeTbl, egs1, egs2, {solBeta0, solBeta90}}];
    ];
(* ============================================== *)
EHFlip[eh : {_, _, _, _, _, _, _}] :=
    Module[{ehRet},
      ehRet = {eh[[1]], -eh[[2]], -eh[[3]], eh[[4]], -eh[[5]], -eh[[6]], Not[eh[[7]]]};
      Return[ehRet];
    ];
(* ============================================== *)
SubstanceGetThickness[subst_] := If[SubstanceQ[subst], ObjectGetContent[subst][[1]], Indeterminate, Indeterminate];
SubstanceGetRotationAngles[subst_] := If[SubstanceQ[subst], ObjectGetContent[subst][[2]], Indeterminate, Indeterminate];
SubstanceGetEpsilon[subst_] := If[SubstanceQ[subst], ObjectGetContent[subst][[3]], Indeterminate, Indeterminate];
SubstanceGetMu[subst_] := If[SubstanceQ[subst], ObjectGetContent[subst][[4]], Indeterminate, Indeterminate];
SubstanceGetRho[subst_] := If[SubstanceQ[subst], ObjectGetContent[subst][[5]], Indeterminate, Indeterminate];
(* ============================================== *)
CreateSemiInfiniteMediaFromN[refrInd_] := CreateSubstanceFromN[SemiInfiniteMediaClassName, Infinity, refrInd];

CreateSemiInfiniteMedia[epsilon_?OpticalTensorQ] :=
    Module[{subst, fi, theta, psi, rotationAngles},
      fi = {0, 0, 1, Subscript["φ", "l"], Degree};
      theta = {0, 0, 1, Subscript["θ", "l"], Degree};
      psi = {0, 0, 1, Subscript["ψ", "l"], Degree};
      rotationAngles = {fi, theta, psi};
      subst = CreateSubstance[SemiInfiniteMediaClassName, Infinity, rotationAngles, epsilon, muMstandard, roMstandard];
      Return[subst];
    ];

CreateSemiInfiniteMedia[rotationAngles_?OpticalTensorQ, epsilon_?MatrixQ] :=
    Module[{subst},
      subst = CreateSubstance[SemiInfiniteMediaClassName, Infinity, rotationAngles, epsilon, muMstandard, roMstandard];
      Return[subst];
    ];

CreateSemiInfiniteMedia[rotationAngles_?MatrixQ, epsilon_?OpticalTensorQ, mu_?OpticalTensorQ] :=
    Module[{subst},
      subst = CreateSubstance[SemiInfiniteMediaClassName, Infinity, rotationAngles, epsilon, mu, roMstandard];
      Return[subst];
    ];

CreateSemiInfiniteMedia[rotationAngles_?MatrixQ, epsilon_?OpticalTensorQ, mu_?OpticalTensorQ, rho_?OpticalTensorQ] :=
    Module[{subst},
      subst = CreateSubstance[SemiInfiniteMediaClassName, Infinity, rotationAngles, epsilon, mu, rho];
      Return[subst];
    ];
(* ============================================== *)
CreateSemiInfiniteMediaFromN[___] :=
    Module[{},
      Print["CreateSemiInfiniteMediaFromN::Invalid parameters."];
      Print["Correct usage: CreateSemiInfiniteMediaFromN[refrInd_]"];
      Abort[];
    ];
(* ============================================== *)
CreateSemiInfiniteMedia[___] :=
    Module[{},
      Print["CreateSemiInfiniteMedia::Invalid parameters."];
      Print["Correct usages:"];
      Print["    CreateSemiInfiniteMedia[rotationAngles_?MatrixQ, epsilon_?MatrixQ]"];
      Print["    CreateSemiInfiniteMedia[rotationAngles_?MatrixQ, epsilon_?MatrixQ, mu_?MatrixQ]"];
      Print["    CreateSemiInfiniteMedia[rotationAngles_?MatrixQ, epsilon_?MatrixQ, mu_?MatrixQ, rho_?MatrixQ]"];
      Abort[];
    ];
(* ============================================== *)
CreateSubstanceFromN[substanceType_?StringQ, thickness_, refrInd_] :=
    Module[{subst, fi, theta, psi, rotationAngles, strIdx, eps},
      strIdx = ToString[SubstanceIdx];
      fi = {0, 0, 1, Subscript["\[CurlyPhi]", strIdx], Degree};
      theta = {0, 0, 1, Subscript["\[Theta]", strIdx], Degree};
      psi = {0, 0, 1, Subscript["\[Psi]", strIdx], Degree};
      rotationAngles = {fi, theta, psi};
      eps = EpsilonFromN[refrInd];
      subst = CreateSubstance[substanceType, thickness, rotationAngles, eps, muMstandard, roMstandard];
      Return[subst];
    ] /; MemberQ[SubstanceClassList, substanceType];
(* ============================================== *)
CreateSubstanceFromN[___] :=
    Module[{},
      Print["CreateSubstanceFromN::Invalid parameters."];
      Print["Correct usage: CreateSubstanceFromN[substanceType_?StringQ, thickness_, refrInd_] where MemberQ[SubstanceClassList, substanceType]."];
      Abort[];
    ];
(* ============================================== *)
CreateSubstance[substanceType_?StringQ, thickness_, rotationAngles_?RotationAnglesQ, epsilon_?OpticalTensorQ, mu_?OpticalTensorQ, rho_?OpticalTensorQ] :=
    Module[{subst},
      subst = {substanceType, {thickness, rotationAngles, epsilon, mu, rho}};
      SubstanceIdx++;
      Return[subst];
    ] /; MemberQ[SubstanceClassList, substanceType];
(* ============================================== *)
CreateSubstance[___] :=
    Module[{},
      Print["CreateSubstance::Invalid parameters."];
      Print["Correct usage: CreateSubstance[substanceType_?StringQ, thickness_, rotationAngles_?RotationAnglesQ, epsilon_?OpticalTensorQ, mu_?OpticalTensorQ, rho_?OpticalTensorQ] where MemberQ[SubstanceClassList, substanceType]."];
      Abort[];
    ];
(* ============================================== *)
(* /;((!ListQ[thickness]) || (ListQ[thickness] && Length[thickness]\[Equal]5)); *)
(* ============================================== *)
CreateThickPlateFromN[thickness_, refrInd_] :=
    CreateSubstanceFromN[ThickPlateClassName, thickness, refrInd];

CreateThickPlate[thickness_, rotationAngles_?RotationAnglesQ, epsilon_?OpticalTensorQ] :=
    CreateSubstance[ThickPlateClassName, thickness, rotationAngles, epsilon, muMstandard, roMstandard];

CreateThickPlate[thickness_, rotationAngles_?RotationAnglesQ, epsilon_?OpticalTensorQ, mu_?OpticalTensorQ] :=
    CreateSubstance[ThickPlateClassName, thickness, rotationAngles, epsilon, mu, roMstandard];

CreateThickPlate[thickness_, rotationAngles_?RotationAnglesQ, epsilon_?OpticalTensorQ, mu_?OpticalTensorQ, rho_?OpticalTensorQ] :=
    CreateSubstance[ThickPlateClassName, thickness, rotationAngles, epsilon, mu, rho];
(* ============================================== *)
CreateThickPlateFromN[___] := Module[{},
  Print["CreateThickPlateFromN::Invalid parameters."];
  Print["Correct usage: CreateThickPlateFromN[thickness_, refrInd_]"];
  Abort[];
];
(* ============================================== *)
CreateThickPlate[___] :=
    Module[{},
      Print["CreateThickPlate::Invalid parameters."];
      Print["Correct usages:"];
      Print["    CreateThickPlate[thickness_, rotationAngles_?RotationAnglesQ, epsilon_?OpticalTensorQ]"];
      Print["    CreateThickPlate[thickness_, rotationAngles_?RotationAnglesQ, epsilon_?OpticalTensorQ, mu_?OpticalTensorQ]"];
      Print["    CreateThickPlate[thickness_, rotationAngles_?RotationAnglesQ, epsilon_?OpticalTensorQ, mu_?OpticalTensorQ, rho_?OpticalTensorQ]"];
      Abort[];
    ];
(* ============================================== *)
CreateFilmFromN[thickness_, refrInd_] := CreateSubstanceFromN[FilmClassName, thickness, refrInd];
CreateFilm[thickness_, rotationAngles_?RotationAnglesQ, epsilon_?OpticalTensorQ] := CreateSubstance[FilmClassName, thickness, rotationAngles, epsilon, muMstandard, roMstandard];
CreateFilm[thickness_, rotationAngles_?RotationAnglesQ, epsilon_?OpticalTensorQ, mu_?OpticalTensorQ] := CreateSubstance[FilmClassName, thickness, rotationAngles, epsilon, mu, roMstandard];
CreateFilm[thickness_, rotationAngles_?RotationAnglesQ, epsilon_?OpticalTensorQ, mu_?OpticalTensorQ, rho_?OpticalTensorQ] := CreateSubstance[FilmClassName, thickness, rotationAngles, epsilon, mu, rho];
(* ============================================== *)
CreateFilmFromN[___] :=
    Module[{},
      Print["CreateFilmFromN::Invalid parameters."];
      Print["Correct usage: CreateFilmFromN[thickness_, refrInd_]"];
      Abort[];
    ];
(* ============================================== *)
CreateFilm[___] :=
    Module[{},
      Print["CreateFilm::Invalid parameters."];
      Print["Correct usages:"];
      Print["    CreateFilm[thickness_, rotationAngles_?RotationAnglesQ, epsilon_?OpticalTensorQ]"];
      Print["    CreateFilm[thickness_, rotationAngles_?RotationAnglesQ, epsilon_?OpticalTensorQ, mu_?OpticalTensorQ]"];
      Print["    CreateFilm[thickness_, rotationAngles_?RotationAnglesQ, epsilon_?OpticalTensorQ, mu_?OpticalTensorQ, rho_?OpticalTensorQ]"];
      Abort[];
    ];
(* ============================================== *)
CreateIncidentRay[nUpper_?NumericQ, lambda_?VariableQ, fita_?VariableQ] :=
    CreateIncidentRay[nUpper, lambda, fita, {0, 0, 90, "\[Beta]", Degree}];

CreateIncidentRay[nUpper_?NumericQ, lambda_?VariableQ, fita_?VariableQ, beta_?VariableQ] :=
    CreateIncidentRay[nUpper, lambda, fita, beta, {0, 0, 1, "e"}];

CreateIncidentRay[nUpper_?NumericQ, lambda_?VariableQ, fita_?VariableQ, beta_?VariableQ, ellipt_?VariableQ] :=
    Module[{light, incidentLight, amplitude},
      amplitude = 1;
      incidentLight = IncidentLightNew[lambda, fita, beta, nUpper, amplitude, ellipt];
      light = {IncidentRayClassName, {incidentLight}};
      Return[light];
    ];
(* ============================================== *)
CreateIncidentRay[___] :=
    Module[{},
      Print["CreateIncidentRay::Invalid parameters."];
      Print["Correct usages:"];
      Print["    CreateIncidentRay[nUpper_?NumericQ, lambda_?VariableQ, fita_?VariableQ]"];
      Print["    CreateIncidentRay[nUpper_?NumericQ, lambda_?VariableQ, fita_?VariableQ, beta_?VariableQ]"];
      Print["    CreateIncidentRay[nUpper_?NumericQ, lambda_?VariableQ, fita_?VariableQ, beta_?VariableQ, ellipt_?VariableQ]"];
      Abort[];
    ];
(* ============================================== *)
IncidentRayGetLight[ray_] := If[IncidentRayQ[ray], ObjectGetContent[ray][[1]], Indeterminate, Indeterminate];
(* ============================================== *)
ValidateLayeredMedia[layers_?SubstanceListQ] :=
    Module[{retVal, len, ii},
    (* Print["ValidateLayeredMedia::Starting..."]; *)
      retVal = False;
      len = Length[layers];

      (* The last "layer" must be semi-infinite media *)
      If[!SemiInfiniteMediaQ[layers[[len]]],
        (
          Print["ValidateLayeredMedia::Last substance in the list must be semi-infinite media."];
          Abort[];
        )
      ];

      Do[
        (
          If[SemiInfiniteMediaQ[layers[[ii]]],
            (
              Print["ValidateLayeredMedia::All substances except the last one must be either thin films or thick plate."];
              Abort[];
            )
          ];
        ), {ii, 1, len - 1}
      ];

      Do[
        (
          If[ThickPlateQ[layers[[ii]]],
            (
              Print["ValidateLayeredMedia::Thick plate must be next to the last \"layer\"."];
              Abort[];
            )
          ];
        ), {ii, 1, len - 2}
      ];

      retVal = True;

      (*
      Print["ValidateLayeredMedia::retVal = ", retVal];
      Print["ValidateLayeredMedia::Completed."];
      *)

      Print["---"];
      Return[retVal];
    ];
(* ============================================== *)
LayeredSystemGetMedia[subst_] := If[LayeredSystemQ[subst], ObjectGetContent[subst][[1]], Indeterminate, Indeterminate];
LayeredSystemGetVarList[subst_] := If[LayeredSystemQ[subst], ObjectGetContent[subst][[2]], Indeterminate, Indeterminate];
LayeredSystemGetExtraOptions[subst_] := If[LayeredSystemQ[subst], ObjectGetContent[subst][[3]], Indeterminate, Indeterminate];
(* ============================================== *)
CreateLayeredSystem[incidentRay_?IncidentRayQ, gamma_?VariableQ, mediaSequence__?SubstanceQ] :=
    Module[{layers, media, film, len, ii, incidentLight, maxFilmLen, useThickPlate, thickPlate, thickness, eps, mu, rho, layer, sys, varList, lambda, fita, beta, amplitude, ellipt, nUpper, fi, theta, psi, fi1, theta1, psi1, thk1, layerSubstance, lowerSubstance, epsLower, muLower, roLower, eValLower, nLower, nOut, epsSubstr, muSubstr, roSubstr, eValSubstr, nSubstr, extraOptions},

      layers = {mediaSequence};

      (* Print["CreateLayeredSystem::layers = ", layers]; *)

      If[!ValidateLayeredMedia[layers],
        (
          Print["CreateLayeredSystem::Invalid media."];
          Return[Indeterminate];
        )
      ];

      extraOptions = {};
      incidentLight = IncidentRayGetLight[incidentRay];
      lambda = IncidentLightLambda[incidentLight];
      fita = IncidentLightFita[incidentLight];
      beta = IncidentLightBeta[incidentLight];
      amplitude = IncidentLighAmplitude[incidentLight];
      ellipt = IncidentLightEllipticity[incidentLight];
      nUpper = IncidentLighRefrIndex[incidentLight];

      len = Length[layers];
      useThickPlate = False;
      maxFilmLen = len - 1;

      lowerSubstance = layers[[len]];

      (* Print["CreateLayeredSystem::lowerSubstance = ", lowerSubstance]; *)

      {fi, theta, psi} = SubstanceGetRotationAngles[lowerSubstance];
      epsLower = SubstanceGetEpsilon[lowerSubstance];
      muLower = SubstanceGetMu[lowerSubstance];
      roLower = SubstanceGetRho[lowerSubstance];
      eValLower = Eigenvalues[epsLower . muLower];
      nLower = Sqrt[eValLower[[1]]];

      (* Print["CreateLayeredSystem::eValLower = ", eValLower // MatrixForm, ", nLower = ", nLower]; *)

      If[len >= 2,
        (
          If[ThickPlateQ[layers[[len - 1]]],
            (
              useThickPlate = True;
              thickPlate = layers[[len - 1]];
              maxFilmLen--;
            )
          ];
        )
      ];

      film = FilmNew[];

      (* Print["CreateLayeredSystem::lambda = ", lambda, ", fita = ", fita, ", beta = ", beta, ", gamma = ", gamma, ", ellipt = ", ellipt, ", fi = ", fi, ", theta = ", theta, ", psi = ", psi]; *)

      varList = VarListNew[{lambda, fita, beta, gamma, ellipt}, {fi, theta, psi}];

      Do[
        (
        (* Print["CreateLayeredSystem::ii = ", ii]; *)

          layer = layers[[ii]];
          layerSubstance = ObjectGetContent[layer];

          (* Print["CreateLayeredSystem::layer = ", layer]; *)
          (* Print["CreateLayeredSystem::layerSubstance = ", layerSubstance]; *)

          thickness = 0;

          eps = SubstanceGetEpsilon[layer];
          mu = SubstanceGetMu[layer];
          rho = SubstanceGetRho[layer];

          {fi1, theta1, psi1} = SubstanceGetRotationAngles[layer];
          thk1 = SubstanceGetThickness[layer];

          (* Print["CreateLayeredSystem::eps = ", eps // MatrixForm, ", mu = ", mu// MatrixForm, ", rho = ", rho// MatrixForm,", fi1 = ", fi1, ", theta1 = ", theta1, ", psi1 = ", psi1, ", thk1 = ", thk1]; *)
          (* Print["..."]; *)

          VarListAddLayer[varList, {fi1, theta1, psi1, thk1}];
          FilmAddLayer[film, FilmLayerNew[thickness, eps, mu, rho]];
        ), {ii, 1, maxFilmLen}
      ];

      If[!useThickPlate,
        (
          nOut = 1;
          thickness = 0;
          media = MediaNew[nUpper, nLower, gamma, film, "Layered System", nOut, thickness, epsLower, muLower, roLower];
          extraOptions = Join[extraOptions, {UseThickLastLayer -> False}];
        ),
        (
          epsSubstr = SubstanceGetEpsilon[thickPlate];
          muSubstr = SubstanceGetMu[thickPlate];
          roSubstr = SubstanceGetRho[thickPlate];
          thickness = SubstanceGetThickness[thickPlate];

          eValSubstr = Eigenvalues[epsSubstr . muSubstr];
          nSubstr = Sqrt[eValSubstr[[1]]];

          (* Print["CreateLayeredSystem::eValSubstr = ", eValSubstr // MatrixForm, ", nSubstr = ", nSubstr]; *)

          media = MediaNew[nUpper, nSubstr, gamma, film, "", nLower, thickness, epsSubstr, muSubstr, roSubstr];
          extraOptions = Join[extraOptions, {UseThickLastLayer -> True}];
        )
      ];

      sys = {LayeredSystemClassName, {media, varList, extraOptions}};
      Return[sys];
    ];
(* ============================================== *)
CreateLayeredSystem[___] :=
    Module[{},
      Print["CreateLayeredSystem::Invalid parameters."];
      Print["Correct usage: CreateLayeredSystem[incidentRay_?IncidentRayQ,gamma_?VariableQ,mediaSequence__?SubstanceQ]"];
      Abort[];
    ];
(* ============================================== *)