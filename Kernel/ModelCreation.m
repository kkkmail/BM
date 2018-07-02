(* ============================================== *)
(* :Summary: This module defines some commonly used Berreman Matrix transformations. *)
(* ============================================== *)
(* :Author: Konstantin K. Konstantinov *)
(* :Email: konstantin.k.konstantinov@gmail.com *)
(* :License type: GPL v3 or any later version, see http://www.gnu.org/licenses/ *)
(* :Copyright: K^3, 2001 - 2018 *)
(* :Version: Revision: 6.04.001, Date: 2018/07/02 *)
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
Options[ModelCreation] =
    {
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
CreateThickPlateFromN[___] :=
    Module[{},
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
CreateFilmFromN[thickness_, refrInd_] :=
    CreateSubstanceFromN[FilmClassName, thickness, refrInd];

CreateFilm[thickness_, rotationAngles_?RotationAnglesQ, epsilon_?OpticalTensorQ] :=
    CreateSubstance[FilmClassName, thickness, rotationAngles, epsilon, muMstandard, roMstandard];

CreateFilm[thickness_, rotationAngles_?RotationAnglesQ, epsilon_?OpticalTensorQ, mu_?OpticalTensorQ] :=
    CreateSubstance[FilmClassName, thickness, rotationAngles, epsilon, mu, roMstandard];

CreateFilm[thickness_, rotationAngles_?RotationAnglesQ, epsilon_?OpticalTensorQ, mu_?OpticalTensorQ, rho_?OpticalTensorQ] :=
    CreateSubstance[FilmClassName, thickness, rotationAngles, epsilon, mu, rho];
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
LayeredSystemGetUpperRefrIndex[subst_] := If[LayeredSystemQ[subst], ObjectGetContent[subst][[3]], Indeterminate, Indeterminate];
LayeredSystemGetExtraOptions[subst_] := If[LayeredSystemQ[subst], ObjectGetContent[subst][[4]], Indeterminate, Indeterminate];
(* ============================================== *)
CreateLayeredSystem[incidentRay_?IncidentRayQ, gamma_?VariableQ, mediaSequence__?SubstanceQ] :=
    Module[{layers, media, film, len, ii, incidentLight, maxFilmLen, useThickPlate, thickPlate, thickness, eps, mu, rho, layer, sys, varList, lambda, fita, beta, amplitude, ellipt, fi, theta, psi, fi1, theta1, psi1, thk1, layerSubstance, lowerSubstance, epsLower, muLower, roLower, eValLower, epsSubstr, muSubstr, roSubstr, eValSubstr, extraOptions, nUpper},

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
      amplitude = IncidentLightAmplitude[incidentLight];
      ellipt = IncidentLightEllipticity[incidentLight];
      nUpper = IncidentLightUpperRefrIndex[incidentLight];

      len = Length[layers];
      useThickPlate = False;
      maxFilmLen = len - 1;

      lowerSubstance = layers[[len]];

      (* Print["CreateLayeredSystem::lowerSubstance = ", lowerSubstance]; *)

      {fi, theta, psi} = SubstanceGetRotationAngles[lowerSubstance];
      epsLower = SubstanceGetEpsilon[lowerSubstance];
      muLower = SubstanceGetMu[lowerSubstance];
      roLower = SubstanceGetRho[lowerSubstance];

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
          thickness = 0;
          media = MediaNew[gamma, film, "Layered System", thickness, epsLower, muLower, roLower, epsMstandard];
          extraOptions = Join[extraOptions, {UseThickLastLayer -> False}];
        ),
        (
          epsSubstr = SubstanceGetEpsilon[thickPlate];
          muSubstr = SubstanceGetMu[thickPlate];
          roSubstr = SubstanceGetRho[thickPlate];
          thickness = SubstanceGetThickness[thickPlate];

          media = MediaNew[gamma, film, "", thickness, epsSubstr, muSubstr, roSubstr, epsLower, muLower, roLower];
          extraOptions = Join[extraOptions, {UseThickLastLayer -> True}];
        )
      ];

      sys = {LayeredSystemClassName, {media, varList, nUpper, extraOptions}};
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