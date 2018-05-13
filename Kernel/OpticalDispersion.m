(* ============================================== *)
(* :Summary: This module defines various dispersion related functions. *)
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
Options[OpticalDispersion] = {OpticalDispersionVersion -> 6.03};
(* ============================================== *)
refrIndexSquared[lambda_, kCoeff_, lambdaNull_] := 1 + kCoeff * lambda^2 / (lambda^2 - lambdaNull^2);
sigmaAbsorption[lambdaMidPoint_, lambdaHalfWidth_] := (lambdaHalfWidth - lambdaMidPoint) / Log[2];
absorptionCoeff[lambda_, kAbsorption_, lambdaMidPoint_, lambdaHalfWidth_] :=
    kAbsorption * Exp[-(lambda - lambdaMidPoint)^2 / sigmaAbsorption[lambdaMidPoint, lambdaHalfWidth]^2];

refrIndex[lambda_, kCoeff_, lambdaNull_, kAbsorption_, lambdaMidPoint_, lambdaHalfWidth_] :=
    Sqrt[refrIndexSquared[lambda, kCoeff, lambdaNull]] + I * absorptionCoeff[lambda, kAbsorption, lambdaMidPoint, lambdaHalfWidth];

gyration11Func[lambda_, refrIndAverageFunc_, a2Coeff_, a3Coeff_, lambda2Coeff_] :=
    I * (lambda * refrIndAverageFunc[lambda] * ((a2Coeff / (lambda^2 - lambda2Coeff^2)) + (a3Coeff * lambda^2 / (lambda^2 - lambda2Coeff^2)^2)));

gyration33Func[lambda_, refrIndAverageFunc_, a1Coeff_, lambda1Coeff_] :=
    I * (lambda * refrIndAverageFunc[lambda] * a1Coeff / (lambda^2 - lambda1Coeff^2));
(* ============================================== *)
eps$Vacuum = IdentityMatrix[3];
(* ============================================== *)
(* La3Ga5SiO14 *)
(* 0.4 mkm < lambda < 1.0 mkm *)

refrIndex$La3Ga5SiO14$Ordinary[lambda_] :=
    refrIndex[lambda, 2.4981088, 0.12978841 mkm, 0.5 * 10^-4, 0.28 * mkm, 0.3 * mkm];

refrIndex$La3Ga5SiO14$ExtraOrdinary[lambda_] :=
    refrIndex[lambda, 2.5408145, 0.12914765 mkm, 1.0 * 10^-4, 0.28 * mkm, 0.3 * mkm];

refrIndex$La3Ga5SiO14$Average[lambda_] :=
    (Re[refrIndex$La3Ga5SiO14$Ordinary[lambda]] + Re[refrIndex$La3Ga5SiO14$ExtraOrdinary[lambda]]) / 2;

g11$La3Ga5SiO14[lambda_] :=
    gyration11Func[lambda, refrIndex$La3Ga5SiO14$Average, 0.6106 * 10^-11, 0.6278 * 10^-11, 0.156 mkm];

g33$La3Ga5SiO14[lambda_] :=
    gyration33Func[lambda, refrIndex$La3Ga5SiO14$Average, 0.6072 * 10^-11, 0.198 mkm];

eps$La3Ga5SiO14[lambda_] := Module[{nVal1, nVal2, nVal3, epsRet},
  nVal1 = refrIndex$La3Ga5SiO14$Ordinary[lambda];
  nVal2 = refrIndex$La3Ga5SiO14$Ordinary[lambda];
  nVal3 = refrIndex$La3Ga5SiO14$ExtraOrdinary[lambda];
  epsRet = EpsilonFromN[nVal1, nVal2, nVal3];
  Return[N[epsRet]];
];

rho$La3Ga5SiO14[lambda_] := Module[{nVal1, nVal2, nVal3, rhoRet},
  rhoRet = DiagonalMatrix[{g11$La3Ga5SiO14[lambda], 0, g33$La3Ga5SiO14[lambda]}];
  Return[N[rhoRet]];
];

(* ============================================== *)
(* Si *)
refrIndex$Si[lambda_] := 3 + I * 0.2;

eps$Si[lambda_] :=
    Module[{epsRet},
      epsRet = EpsilonFromN[refrIndex$Si[lambda]];
      Return[N[epsRet]];
    ];
(* ============================================== *)
