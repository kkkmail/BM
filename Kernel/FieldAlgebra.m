(* ============================================== *)
(* :Summary:This module defines some commonly used Field transformations. *)
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
Options[FieldAlgebra] = {FieldAlgebraVersion -> 6.03, FunctionName -> "Unknown", FunctionDescription -> "Unknown", NonAverageable -> False, NonAverageableIndex -> 1, UseAnalyzer -> False, AbsoluteAzimuth -> True, AzimuthRangeType -> 0, AzimuthShiftType -> 1, PrintFunctionDebugInfo -> False, PrintFunctionDebugInfoLevel -> 0, SetEllipticitySignR -> True, UseZAxisForEllipticityR -> False, SetEllipticitySignT -> True, UseZAxisForEllipticityT -> False, SetEllipticitySignI -> True, UseZAxisForEllipticityI -> False, EllipticityCutOffForAzimyth -> 0.9999, AveragingFuncCall -> None, RequiresCalculateBeta0and90 -> False};
(* ============================================== *)
FAAZTOLERANCE = 10^-6;
PVECTORTOLERANCE = 10^-6;
ZEROIFULLBASECUTOFF = 10^-12;
(* ============================================== *)
Options[IFull] = {FunctionName -> "I", FunctionDescription -> "Full intensity of Incident light."};
Options[RFull] = {FunctionName -> "R", FunctionDescription -> "Full intensity of Reflected light."};
Options[TFull] = {FunctionName -> "T", FunctionDescription -> "Full intensity of Transmitted light."};
(* ============================================== *)
Options[Ix] = {FunctionName -> Subscript["I", "p"], FunctionDescription -> "Intensity of Incident light going into X component."};
Options[Iy] = {FunctionName -> Subscript["I", "s"], FunctionDescription -> "Intensity of Incident light going into Y component."};
Options[Rx] = {FunctionName -> Subscript["R", "p"], FunctionDescription -> "Intensity of Reflected light going into X component."};
Options[Ry] = {FunctionName -> Subscript["R", "s"], FunctionDescription -> "Intensity of Reflected light going into Y component."};
Options[Tx] = {FunctionName -> Subscript["T", "p"], FunctionDescription -> "Intensity of Transmitted light going into X component."};
Options[Ty] = {FunctionName -> Subscript["T", "s"], FunctionDescription -> "Intensity of Transmitted light going into Y component."};
(* ============================================== *)
Options[Xit] = {FunctionName -> Subscript["\[Chi]", "t"], FunctionDescription -> "Azimuth of Transmitted light (new).", NonAverageable -> True, NonAverageableIndex -> 2};
Options[CosXit] = {FunctionName -> Cos[Subscript["\[Chi]", "t"]], FunctionDescription -> "Cosine of Azimuth of Transmitted light (new).", NonAverageable -> True, NonAverageableIndex -> 2};
Options[Sin2Xit] = {FunctionName -> Sin[2 Subscript["\[Chi]", "t"]], FunctionDescription -> "Sine of 2 x Azimuth of Transmitted light (new).", NonAverageable -> True, NonAverageableIndex -> 2};
Options[XitDegree] = {FunctionName -> Subscript["\[Chi]", "t"], FunctionDescription -> "Azimuth of Transmitted light in Degrees (new).", NonAverageable -> True, NonAverageableIndex -> 2};
Options[Elt] = {FunctionName -> Subscript["e", "t"], FunctionDescription -> "Ellipticity of Transmitted light (new).", NonAverageable -> True, NonAverageableIndex -> 2};
(* ============================================== *)
(* TODO-check NonAverageableIndex *)
Options[Xii] = {FunctionName -> Subscript["\[Chi]", "i"], FunctionDescription -> "Azimuth of Incident light (new).", NonAverageable -> True, NonAverageableIndex -> 1};
Options[CosXii] = {FunctionName -> Cos[Subscript["\[Chi]", "i"]], FunctionDescription -> "Cosine of Azimuth of Incident light (new).", NonAverageable -> True, NonAverageableIndex -> 1};
Options[Sin2Xii] = {FunctionName -> Sin[2 Subscript["\[Chi]", "i"]], FunctionDescription -> "Sine of 2 x Azimuth of Incident light (new).", NonAverageable -> True, NonAverageableIndex -> 1};
Options[XiiDegree] = {FunctionName -> Subscript["\[Chi]", "i"], FunctionDescription -> "Azimuth of Incident light in Degrees (new).", NonAverageable -> True, NonAverageableIndex -> 1};
Options[Eli] = {FunctionName -> Subscript["e", "i"], FunctionDescription -> "Ellipticity of Incident light (new).", NonAverageable -> True, NonAverageableIndex -> 1};
(* ============================================== *)
Options[Xir] = {FunctionName -> Subscript["\[Chi]", "r"], FunctionDescription -> "Azimuth of Reflected light (new).", NonAverageable -> True, NonAverageableIndex -> 1};
Options[CosXir] = {FunctionName -> Cos[Subscript["\[Chi]", "r"]], FunctionDescription -> "Cosine of Azimuth of Reflected light (new).", NonAverageable -> True, NonAverageableIndex -> 1};
Options[XirDegree] = {FunctionName -> Subscript["\[Chi]", "r"], FunctionDescription -> "Azimuth of Reflected light in Degrees (new).", NonAverageable -> True, NonAverageableIndex -> 1};
Options[Sin2Xir] = {FunctionName -> Sin[2 Subscript["\[Chi]", "r"]], FunctionDescription -> "Sine of 2 x Azimuth of Reflected light (new).", NonAverageable -> True, NonAverageableIndex -> 1};
Options[Elr] = {FunctionName -> Subscript["e", "r"], FunctionDescription -> "Ellipticity of Reflected light (new).", NonAverageable -> True, NonAverageableIndex -> 1};
(* ============================================== *)
Options[RAnalyzer] = {FunctionName -> "RAnalyzer", FunctionDescription -> "Intensity of Reflected light passed through analyzer.", UseAnalyzer -> True};
Options[TAnalyzer] = {FunctionName -> "TAnalyzer", FunctionDescription -> "Intensity of Transmitted light passed through analyzer.", UseAnalyzer -> True};
Options[DetM1] = {FunctionName -> "Det[M1]", FunctionDescription -> "Determinant of Matrix M of the upper media.", NonAverageable -> True, NonAverageableIndex -> 1};
Options[DetM2] = {FunctionName -> "Det[M2]", FunctionDescription -> "Determinant of Matrix M of the lower media.", NonAverageable -> True, NonAverageableIndex -> 1};
Options[LDetM2] = {FunctionName -> "Log[Det[M2]]", FunctionDescription -> "Log of Determinant of Matrix M of the lower media.", NonAverageable -> True, NonAverageableIndex -> 1};
Options[DetPPP] = {FunctionName -> "Det[PPP]", FunctionDescription -> "Det[PPP].", NonAverageable -> True, NonAverageableIndex -> 1};
Options[DetCoeff] = {FunctionName -> "Det[Coeff]", FunctionDescription -> "Det[Coeff].", NonAverageable -> True, NonAverageableIndex -> 1};
Options[ReDetCoeff] = {FunctionName -> "Re[Det[Coeff]]", FunctionDescription -> "Re[Det[Coeff]].", NonAverageable -> True, NonAverageableIndex -> 1};
Options[ImDetCoeff] = {FunctionName -> "Im[Det[Coeff]]", FunctionDescription -> "Im[Det[Coeff]].", NonAverageable -> True, NonAverageableIndex -> 1};
Options[RxAccuracy] = {FunctionName -> "RxAccuracy", FunctionDescription -> "Accuracy of Rx.", NonAverageable -> True, NonAverageableIndex -> 1};
Options[DetM2Accuracy] = {FunctionName -> "DetM2Accuracy", FunctionDescription -> "Accuracy of Det[M2].", NonAverageable -> True, NonAverageableIndex -> 1};
Options[DetCoeffAccuracy] = {FunctionName -> "DetCoeffAccuracy", FunctionDescription -> "Accuracy of Det[Coeff].", NonAverageable -> True, NonAverageableIndex -> 1};
Options[M2EValRe] = {FunctionName -> "Re[EVal[M2]]", FunctionDescription -> "Re of EigenValues of Matrix M of the lower media.", NonAverageable -> True, NonAverageableIndex -> 1};
Options[M2EValIm] = {FunctionName -> "Im[EVal[M2]]", FunctionDescription -> "Im of EigenValues of Matrix M of the lower media.", NonAverageable -> True, NonAverageableIndex -> 1};
Options[PoyntingI] = {FunctionName -> "PoyntingI", FunctionDescription -> "Poynting vector component of Incident light."};
Options[PoyntingIFull] = {FunctionName -> "PoyntingIFull", FunctionDescription -> "Value of full Poynting Vector of incident light."};
Options[PoyntingR] = {FunctionName -> "PoyntingR", FunctionDescription -> "Poynting vector component of Reflected light."};
Options[PoyntingRFull] = {FunctionName -> "PoyntingRFull", FunctionDescription -> "Value of full Poynting Vector of reflected light."};
Options[PoyntingT] = {FunctionName -> "PoyntingT", FunctionDescription -> "Poynting vector component of Transmitted light."};
Options[PoyntingTFull] = {FunctionName -> "PoyntingTFull", FunctionDescription -> "Value of full Poynting Vector of transmitted light."};
Options[PoyntingRTFull] = {FunctionName -> "PoyntingRTFull", FunctionDescription -> "PoyntingRTFull."};
Options[PoyntingIRTzDiff] = {FunctionName -> "PoyntingIRTzDiff", FunctionDescription -> "Sum of z components of Poynting Vector of incident, reflected and transmitted light."};
Options[PoyntingVector2] = {FunctionName -> "PoyntingVector2", FunctionDescription -> "Poynting vector component of eigen vector of the lower media."};
Options[PoyntingXYPhase2] = {FunctionName -> "PoyntingXYPhase2", FunctionDescription -> "Poynting vector component phase (azimuth) in xy plane of eigen vector of the lower media."};
Options[PoyntingXY2] = {FunctionName -> "PoyntingXY2", FunctionDescription -> "Poynting vector xy component of eigen vector of the lower media."};
Options[ReEGE2] = {FunctionName -> "ReEGE2", FunctionDescription -> "Real part of Electric field of eigen vector of the lower media."};
Options[ImEGE2] = {FunctionName -> "ImEGE2", FunctionDescription -> "Imaginary part of Electric field of eigen vector of the lower media."};
Options[ReEGH2] = {FunctionName -> "ReEGH2", FunctionDescription -> "Real part of Magnetic field of eigen vector of the lower media."};
Options[ImEGH2] = {FunctionName -> "ImEGH2", FunctionDescription -> "Imaginary part of Magnetic field of eigen vector of the lower media."};
Options[LRFull] = {FunctionName -> "Log[RFull]", FunctionDescription -> "Logarithm of full intensity of Reflected light."};
Options[LTFull] = {FunctionName -> "Log[TFull]", FunctionDescription -> "Logarithm of full intensity of Transmitted light."};
Options[LRx] = {FunctionName -> "Log[Rx]", FunctionDescription -> "Logarithm of Intensity of Reflected light going into X component."};
Options[LRy] = {FunctionName -> "Log[Ry]", FunctionDescription -> "Logarithm of Intensity of Reflected light going into Y component."};
Options[LTx] = {FunctionName -> "Log[Tx]", FunctionDescription -> "Logarithm of Intensity of Transmitted light going into X component."};
Options[LTy] = {FunctionName -> "Log[Ty]", FunctionDescription -> "Logarithm of Intensity of Transmitted light going into Y component."};
Options[LnRFull] = {FunctionName -> "Ln[RFull]", FunctionDescription -> "
    Natural logarithm of full intensity of Reflected light."};
Options[LnTFull] = {FunctionName -> "Ln[TFull]", FunctionDescription -> "Natural logarithm of full intensity of Transmitted light."};
Options[LnRx] = {FunctionName -> "Ln[Rx]", FunctionDescription -> "Natural
    logarithm of Intensity of Reflected light going into X component."};
Options[LnRy] = {FunctionName -> "Ln[Ry]", FunctionDescription -> "Natural logarithm of Intensity of Reflected light going into Y component."};
Options[LnTx] = {FunctionName -> "Ln[
      Tx]", FunctionDescription -> "Natural logarithm of
        Intensity of Transmitted light going into X component."};
Options[LnTy] = {FunctionName -> "Ln[Ty]", FunctionDescription -> "Natural logarithm of Intensity of Transmitted light going into Y component."};
Options[EpsComponent] = {FunctionName -> "EpsComponent", FunctionDescription -> "Component of Matrix Epsilon.", NonAverageable -> True, NonAverageableIndex -> 1};
Options[MuComponent] = {FunctionName -> "MuComponent", FunctionDescription -> "Component of Matrix Mu.", NonAverageable -> True, NonAverageableIndex -> 1};
Options[RoComponent] = {FunctionName -> "RoComponent", FunctionDescription -> "Component of Matrix Ro.", NonAverageable -> True, NonAverageableIndex -> 1};
Options[XitEG] = {FunctionName -> Subscript["\[Chi]", "t"], FunctionDescription -> "Azimuth of the eigen vector of the lower media.", NonAverageable -> True, NonAverageableIndex -> 2};
Options[CosXitEG] = {FunctionName -> Cos[Subscript["\[Chi]", "t"]], FunctionDescription -> "Cosine of the eigen vector of the lower media.", NonAverageable -> True, NonAverageableIndex -> 2};
Options[Sin2XitEG] = {FunctionName -> Sin[2 Subscript["\[Chi]", "t"]], FunctionDescription -> "Sine of 2 x Azimuth of the eigen vector of the lower media.", NonAverageable -> True, NonAverageableIndex -> 2};
Options[XitEGDegree] = {FunctionName -> Subscript["\[Chi]", "t"], FunctionDescription -> "Azimuth of the eigen vector of the lower media in Degrees.", NonAverageable -> True, NonAverageableIndex -> 2};
Options[EltEG] = {FunctionName -> Subscript["e", "t"], FunctionDescription -> "Ellipticity of the eigen vector of the lower media.", NonAverageable -> True, NonAverageableIndex -> 2};
Options[EI] = {FunctionName -> "EI", FunctionDescription -> "Component of E of incident light."};
Options[ER] = {FunctionName -> "ER", FunctionDescription -> "Component of E of reflected light."};
Options[ET] = {FunctionName -> "ET", FunctionDescription -> "Component of E of transmitted light."};
(* ============================================== *)
Options[PsiPP] = {FunctionName -> Subscript["\[Psi]", "pp"], FunctionDescription -> "Psi pp.", NonAverageable -> True, NonAverageableIndex -> 1, RequiresCalculateBeta0and90 -> True};
Options[PsiPS] = {FunctionName -> Subscript["\[Psi]", "ps"], FunctionDescription -> "Psi ps.", NonAverageable -> True, NonAverageableIndex -> 1, RequiresCalculateBeta0and90 -> True};
Options[PsiSP] = {FunctionName -> Subscript["\[Psi]", "sp"], FunctionDescription -> "Psi sp.", NonAverageable -> True, NonAverageableIndex -> 1, RequiresCalculateBeta0and90 -> True};

Options[DeltaPP] = {FunctionName -> Subscript["\[CapitalDelta]", "pp"], FunctionDescription -> "Delta pp.", NonAverageable -> True, NonAverageableIndex -> 1, RequiresCalculateBeta0and90 -> True};
Options[DeltaPS] = {FunctionName -> Subscript["\[CapitalDelta]", "ps"], FunctionDescription -> "Delta ps.", NonAverageable -> True, NonAverageableIndex -> 1, RequiresCalculateBeta0and90 -> True};
Options[DeltaSP] = {FunctionName -> Subscript["\[CapitalDelta]", "sp"], FunctionDescription -> "Delta sp.", NonAverageable -> True, NonAverageableIndex -> 1, RequiresCalculateBeta0and90 -> True};
(* ============================================== *)
Options[PsiPPDegree] = {FunctionName -> Subscript["\[Psi]", "pp"], FunctionDescription -> "Psi pp in degrees.", NonAverageable -> True, NonAverageableIndex -> 1, RequiresCalculateBeta0and90 -> True};
Options[PsiPSDegree] = {FunctionName -> Subscript["\[Psi]", "ps"], FunctionDescription -> "Psi ps in degrees.", NonAverageable -> True, NonAverageableIndex -> 1, RequiresCalculateBeta0and90 -> True};
Options[PsiSPDegree] = {FunctionName -> Subscript["\[Psi]", "sp"], FunctionDescription -> "Psi sp in degrees.", NonAverageable -> True, NonAverageableIndex -> 1, RequiresCalculateBeta0and90 -> True};

Options[DeltaPPDegree] = {FunctionName -> Subscript["\[CapitalDelta]", "pp"], FunctionDescription -> "Delta pp in degrees.", NonAverageable -> True, NonAverageableIndex -> 1, RequiresCalculateBeta0and90 -> True};
Options[DeltaPSDegree] = {FunctionName -> Subscript["\[CapitalDelta]", "ps"], FunctionDescription -> "Delta ps in degrees.", NonAverageable -> True, NonAverageableIndex -> 1, RequiresCalculateBeta0and90 -> True};
Options[DeltaSPDegree] = {FunctionName -> Subscript["\[CapitalDelta]", "sp"], FunctionDescription -> "Delta sp in degrees.", NonAverageable -> True, NonAverageableIndex -> 1, RequiresCalculateBeta0and90 -> True}
(* ============================================== *)
Options[MuellerMatrixR] = {FunctionName -> Subscript["M", "R"], FunctionDescription -> "Mueller Matrix R.", NonAverageable -> True, NonAverageableIndex -> 1, RequiresCalculateBeta0and90 -> True};
(* ============================================== *)
Options[StokesVectorR] = {FunctionName -> Subscript["S", "R"], FunctionDescription -> "Stokes Vector R.", NonAverageable -> False, NonAverageableIndex -> 1};

Options[StokesVectorT] = {FunctionName -> Subscript["S", "T"], FunctionDescription -> "Stokes Vector T.", NonAverageable -> False, NonAverageableIndex -> 1};

Options[StokesVectorI] = {FunctionName -> Subscript["S", "I"], FunctionDescription -> "Stokes Vector I.", NonAverageable -> False, NonAverageableIndex -> 1};
(* ============================================== *)
Options[StokesVectorAllR] = {FunctionName -> Subscript["S", "R"], FunctionDescription -> "Stokes Vector R.", NonAverageable -> False, NonAverageableIndex -> 1};

Options[StokesVectorAllT] = {FunctionName -> Subscript["S", "T"], FunctionDescription -> "Stokes Vector T.", NonAverageable -> False, NonAverageableIndex -> 1};

Options[StokesVectorAllI] = {FunctionName -> Subscript["S", "I"], FunctionDescription -> "Stokes Vector I.", NonAverageable -> False, NonAverageableIndex -> 1};
(* ============================================== *)
Options[StokesGammaR] = {FunctionName -> Subscript["\[Gamma]", "R"], FunctionDescription -> "\[Gamma] of Stokes Vector R.", NonAverageable -> False, NonAverageableIndex -> 1, AveragingFuncCall -> StokesVectorAllR};
Options[StokesGammaDegreeR] = {FunctionName -> Subscript["\[Gamma]", "R"] FromCharacterCode[176], FunctionDescription -> "\[Gamma] of Stokes Vector R (in degrees).", NonAverageable -> False, NonAverageableIndex -> 1, AveragingFuncCall -> StokesVectorAllR};
(* ============================================== *)
Options[StokesChiR] = {FunctionName -> Subscript["\[Chi]", "R"], FunctionDescription -> "\[Chi] of Stokes Vector R.", NonAverageable -> False, NonAverageableIndex -> 1, AveragingFuncCall -> StokesVectorAllR};
Options[StokesChiDegreeR] = {FunctionName -> Subscript["\[Chi]", "R"] FromCharacterCode[176], FunctionDescription -> "\[Chi] of Stokes Vector R (in degrees).", NonAverageable -> False, NonAverageableIndex -> 1, AveragingFuncCall -> StokesVectorAllR};
(* ============================================== *)
Options[StokesPolarizedR] = {FunctionName -> Subscript["p", "R"], FunctionDescription -> "p of Stokes Vector R.", NonAverageable -> False, NonAverageableIndex -> 1, AveragingFuncCall -> StokesVectorAllR};
(* ============================================== *)
FunctionNameString[HoldPattern[EI[xyz_]]] := Module[{retval}, retval = "EI[" <> ToString[If[xyz === 1, "x", If[xyz === 2, "y", If[xyz === 3, "z"]]]] <> "]";
Return[retval];];

FunctionNameString[HoldPattern[ER[xyz_]]] := Module[{retval}, retval = "ER[" <> ToString[If[xyz === 1, "x", If[xyz === 2, "y", If[xyz === 3, "z"]]]] <> "]";
Return[retval];];

FunctionNameString[HoldPattern[ET[xyz_]]] := Module[{retval}, retval = "ET[" <> ToString[If[xyz === 1, "x", If[xyz === 2, "y", If[xyz === 3, "z"]]]] <> "]";
Return[retval];];

FunctionNameString[HoldPattern[PoyntingI[xyz_]]] := Module[{retval}, retval = "SI[" <> ToString[If[xyz === 1, "x", If[xyz === 2, "y", If[xyz === 3, "z"]]]] <> "]";
Return[retval];];

FunctionNameString[HoldPattern[PoyntingR[xyz_]]] := Module[{retval}, retval = "SR[" <> ToString[If[xyz === 1, "x", If[xyz === 2, "y", If[xyz === 3, "z"]]]] <> "]";
Return[retval];];

FunctionNameString[HoldPattern[PoyntingT[xyz_]]] := Module[{retval}, retval = "ST[" <> ToString[If[xyz === 1, "x", If[xyz === 2, "y", If[xyz === 3, "z"]]]] <> "]";
Return[retval];];

FunctionNameString[HoldPattern[PoyntingVector2[evi_, xyz_]]] := Module[{retval}, retval = "S2[" <> ToString[evi] <> "," <> ToString[If[xyz === 1, "x", If[xyz === 2, "y", If[xyz === 3, "z"]]]] <> "]";
Return[retval];];

FunctionNameString[HoldPattern[PoyntingXY2[evi_]]] := Module[{retval}, retval = "Sxy2[" <> ToString[evi] <> "]";
Return[retval];];

FunctionNameString[HoldPattern[PoyntingXYPhase2[evi_]]] := Module[{retval}, retval = "SxyPhase2[" <> ToString[evi] <> "]";
Return[retval];];

FunctionNameString[HoldPattern[M2EValRe[evi_]]] := Module[{retval}, retval = "Re[Eval(M2)[" <> ToString[evi] <> "]]";
Return[retval];];

FunctionNameString[HoldPattern[M2EValIm[evi_]]] := Module[{retval}, retval = "Im[Eval(M2)[" <> ToString[evi] <> "]]";
Return[retval];];

FunctionNameString[HoldPattern[ReEGE2[evi_, xyz_]]] := Module[{retval}, retval = "Re[E2[" <> ToString[evi] <> "," <> ToString[If[xyz === 1, "x", If[xyz === 2, "y", If[xyz === 3, "z"]]]] <> "]]";
Return[retval];];

FunctionNameString[HoldPattern[ImEGE2[evi_, xyz_]]] := Module[{retval}, retval = "Im[E2[" <> ToString[evi] <> "," <> ToString[If[xyz === 1, "x", If[xyz === 2, "y", If[xyz === 3, "z"]]]] <> "]]";
Return[retval];];

FunctionNameString[HoldPattern[ReEGH2[evi_, xyz_]]] := Module[{retval}, retval = "Re[H2[" <> ToString[evi] <> "," <> ToString[If[xyz === 1, "x", If[xyz === 2, "y", If[xyz === 3, "z"]]]] <> "]]";
Return[retval];];

FunctionNameString[HoldPattern[ImEGH2[evi_, xyz_]]] := Module[{retval}, retval = "Im[H2[" <> ToString[evi] <> "," <> ToString[If[xyz === 1, "x", If[xyz === 2, "y", If[xyz === 3, "z"]]]] <> "]]";
Return[retval];];

FunctionNameString[HoldPattern[EpsComponent[UpDown_, ReIm_, xyzRow_, xyzCol_]]] := Module[{retval, hdr}, hdr = "Eps";
retval = If[UpDown > 0, "Layer " <> ToString[UpDown] <> ": ", ""] <> If[ReIm === 1, "Re", "Im"] <> "[" <> hdr <> If[UpDown === -1, "1", If[UpDown === -2, "2", ""]] <> "[" <> ToString[If[xyzRow === 1, "x", If[xyzRow === 2, "y", If[xyzRow === 3, "z"]]]] <> "," <> ToString[If[xyzCol === 1, "x", If[xyzCol === 2, "y", If[xyzCol === 3, "z"]]]] <> "]]";
Return[retval];];

FunctionNameString[HoldPattern[MuComponent[UpDown_, ReIm_, xyzRow_, xyzCol_]]] := Module[{retval, hdr}, hdr = "Mu";
retval = If[UpDown > 0, "Layer " <> ToString[UpDown] <> ": ", ""] <> If[ReIm === 1, "Re", "Im"] <> "[" <> hdr <> If[UpDown === -1, "1", If[UpDown === -2, "2", ""]] <> "[" <> ToString[If[xyzRow === 1, "x", If[xyzRow === 2, "y", If[xyzRow === 3, "z"]]]] <> "," <> ToString[If[xyzCol === 1, "x", If[xyzCol === 2, "y", If[xyzCol === 3, "z"]]]] <> "]]";
Return[retval];];

FunctionNameString[HoldPattern[RoComponent[UpDown_, ReIm_, xyzRow_, xyzCol_]]] := Module[{retval, hdr}, hdr = "Ro";
retval = If[UpDown > 0, "Layer " <> ToString[UpDown] <> ": ", ""] <> If[ReIm === 1, "Re", "Im"] <> "[" <> hdr <> If[UpDown === -1, "1", If[UpDown === -2, "2", ""]] <> "[" <> ToString[If[xyzRow === 1, "x", If[xyzRow === 2, "y", If[xyzRow === 3, "z"]]]] <> "," <> ToString[If[xyzCol === 1, "x", If[xyzCol === 2, "y", If[xyzCol === 3, "z"]]]] <> "]]";
Return[retval];];

FunctionNameString[HoldPattern[XitEG[evi_]]] := Module[{retval}, (*retval=ToString[Subscript["\[Chi]","t"]]<>"["<>ToString[evi]<>"]";*)retval = "\[Chi]t" <> "[" <> ToString[evi] <> "]";
Return[retval];];
FunctionNameString[HoldPattern[CosXitEG[evi_]]] := Module[{retval}, (*retval=ToString[Cos[Subscript["\[Chi]","t"]]]<>"["<>ToString[evi]<>"]";*)retval = "Cos[\[Chi]t]" <> "[" <> ToString[evi] <> "]";
Return[retval];];
FunctionNameString[HoldPattern[Sin2XitEG[evi_]]] := Module[{retval}, (*retval=ToString[Sin[2 Subscript["\[Chi]","t"]]]<>"["<>ToString[evi]<>"]";*)retval = "Sin[2 \[Chi]t]" <> "[" <> ToString[evi] <> "]";
Return[retval];];
FunctionNameString[HoldPattern[XitEGDegree[evi_]]] := Module[{retval}, (*retval=ToString[Subscript["\[Chi]","t"]]<>"["<>ToString[evi]<>"]";*)retval = "\[Chi]t" <> "[" <> ToString[evi] <> "]";
Return[retval];];
FunctionNameString[HoldPattern[EltEG[evi_]]] := Module[{retval}, (*retval=ToString[Subscript["e","t"]]<>"["<>ToString[evi]<>"]";*)retval = "et" <> "[" <> ToString[evi] <> "]";
Return[retval];];

(*
FunctionNameString[HoldPattern[StokesVectorR[idx_]]]:=Module[{retval},retval="S["<>ToString[idx-1]<>", R]";
Return[retval];];
FunctionNameString[HoldPattern[StokesVectorI[idx_]]]:=Module[{retval},retval="S["<>ToString[idx-1]<>", I]";
Return[retval];];
*)

FunctionNameString[HoldPattern[StokesVectorI[idx_]]] := Module[{retval}, retval = Subscript["S", "I," <> ToString[idx - 1]];
Return[retval];];
FunctionNameString[HoldPattern[StokesVectorR[idx_]]] := Module[{retval}, retval = Subscript["S", "R," <> ToString[idx - 1]];
Return[retval];];
FunctionNameString[HoldPattern[StokesVectorT[idx_]]] := Module[{retval}, retval = Subscript["S", "T," <> ToString[idx - 1]];
Return[retval];];
(* ============================================== *)
GetFunctionName[func_] := Module[{strFuncName, opts, hd, lst}, opts = Options[func];
lst = Apply[List, func];
(*Print["opts = ",opts];
Print["func = ",func];
Print["lst = ",lst];*)strFuncName = If[Head[lst] === Head[{}], FunctionNameString[func], FunctionName /. opts /. Options[FieldAlgebra]];
(*Print["strFuncName = ",strFuncName];*)Return[strFuncName];];
SetAttributes[GetFunctionName, Listable];

(* ============================================== *)
GetFunctionDesc[func_] := Module[{strFuncDesc, opts, hd, lst, fnelhlp1, fnelhlp2, ueFuncListHlp1}, lst = Apply[List, func];
fnelhlp1 = Apply[List, func];
fnelhlp2 = Head[func];
ueFuncListHlp1 = If[Head[fnelhlp1] === Head[{}], fnelhlp2, fnelhlp1];
(*Print["ueFuncListHlp1 = ",ueFuncListHlp1];*)opts = Options[ueFuncListHlp1];
strFuncDesc = FunctionDescription /. opts /. Options[FieldAlgebra];
Return[strFuncDesc];];
SetAttributes[GetFunctionDesc, Listable];
(* ============================================== *)
GetFunctionOptions[func_] := Module[{opts, hd, lst, fnelhlp1, fnelhlp2, ueFuncListHlp1}, lst = Apply[List, func];
fnelhlp1 = Apply[List, func];
fnelhlp2 = Head[func];
ueFuncListHlp1 = If[Head[fnelhlp1] === Head[{}], fnelhlp2, fnelhlp1];
opts = Options[ueFuncListHlp1];
(*Print["opts = ",opts];*)Return[opts];];
SetAttributes[GetFunctionOptions, Listable];
(* ============================================== *)
PoyntingVector[ehField : {_, _, _, _, _, _}] := Module[{retVal, eField, hField}, eField = Table[ehField[[iii]], {iii, 1, 3}];
hField = Table[ehField[[iii + 3]], {iii, 1, 3}];
retVal = PoyntingVector[eField, hField];
Return[retVal];];

PoyntingVector[ehField : {_, _, _, _, _, _, _}] := Module[{retVal, eField, hField}, eField = Table[ehField[[iii]], {iii, 1, 3}];
hField = Table[ehField[[iii + 3]], {iii, 1, 3}];
retVal = PoyntingVector[eField, hField];
Return[retVal];];

PoyntingVector[eField : {_, _, _}, hField : {_, _, _}] := Module[{retVal}, retVal = Re[Flatten[eField]\[Cross]Flatten[Conjugate[hField]]];
Return[retVal];];
(* ============================================== *)
IFullBase[FullSol_] :=
    Module[{Efield, Dfield, Hfield, Bfield, retVal, ExBvec, ExHvec, ExH1vec, pdi, pdil, ignoreZeroI, zeroIFullReplVal, opts},
      opts = GetSolOptions[FullSol];
      pdi = PrintFunctionDebugInfo /. opts /. Options[FieldAlgebra];
      pdil = PrintFunctionDebugInfoLevel /. opts /. Options[FieldAlgebra];
      If[pdi == True, Print["IFullBase..."];
      Print["IFullBase::opts = ", opts];];
      ignoreZeroI = IgnoreZeroIFullBase /. opts /. Options[BerremanDirect];
      zeroIFullReplVal = ZeroIFullBaseReplaceValue /. opts /. Options[BerremanDirect];
      If[pdi == True, Print["IFullBase::ignoreZeroI = ", ignoreZeroI];
      Print["IFullBase::zeroIFullReplVal = ", zeroIFullReplVal];];
      Efield = GetSolIncidentLightE[FullSol];
      Dfield = GetSolIncidentLightD[FullSol];
      Hfield = GetSolIncidentLightH[FullSol];
      Bfield = GetSolIncidentLightB[FullSol];
      ExBvec = Flatten[Efield]\[Cross]Flatten[Conjugate[Bfield]];
      ExHvec = Flatten[Efield]\[Cross]Flatten[Conjugate[Hfield]];
      ExH1vec = Flatten[Efield]\[Cross]Flatten[Hfield];
      retVal = Re[ExHvec[[3]]];
      (*retVal=Re[ExH1vec[[3]]];*)If[ignoreZeroI == True && retVal < ZEROIFULLBASECUTOFF, If[pdi == True, Print["IFullBase:: Near zero retval is being replaced. Old retval = ", retVal];];
      retVal = zeroIFullReplVal;];

      (*
      Print["IFullBase::Efield = ", Efield];
      Print["IFullBase::Hfield = ", Hfield];
      Print["IFullBase::ExHvec = ", ExHvec];
      Print["IFullBase::retVal = ", retVal];
      *)

      If[pdi == True,
        (
          Print["E = ", Efield // MatrixForm, ", H = ", Hfield // MatrixForm, ", D = ", Dfield // MatrixForm, ", B = ", Bfield // MatrixForm];
          Print["E x B = ", ExBvec // MatrixForm, ", E x Conjugate[H] = ", ExHvec // MatrixForm, ", E x H = ", ExH1vec // MatrixForm];
          Print["retVal = ", retVal];
          Print["IFullBase completed."];
          Print["   "];
        )
      ];

      Return[retVal];
    ];
(* ============================================== *)
IFull[FullSol_] :=
    Module[{Efield, Dfield, Hfield, Bfield, retVal, ExBvec, ExHvec, Ibase, ExH1vec, opts, pdi, pdil, ExHvecRe, ExEHvecRe},
      opts = GetSolOptions[FullSol];
      pdi = PrintFunctionDebugInfo /. opts /. Options[FieldAlgebra];
      pdil = PrintFunctionDebugInfoLevel /. opts /. Options[FieldAlgebra];
      If[pdi == True, Print["IFull..."]];
      Ibase = IFullBase[FullSol];
      Efield = GetSolIncidentLightE[FullSol];
      Dfield = GetSolIncidentLightD[FullSol];
      Hfield = GetSolIncidentLightH[FullSol];
      Bfield = GetSolIncidentLightB[FullSol];
      ExBvec = Flatten[Efield]\[Cross]Flatten[Conjugate[Bfield]];
      ExHvec = Flatten[Efield]\[Cross]Flatten[Conjugate[Hfield]];
      ExH1vec = Flatten[Efield]\[Cross]Flatten[Hfield];
      ExHvecRe = Re[ExHvec];
      ExEHvecRe = Flatten[Efield].ExHvecRe;
      (*If[pdi\[Equal]True,Print["Ibase = ",Ibase]];*)
      retVal = If[Abs[Ibase] > 0, Re[ExHvec[[3]]] / Ibase, 0, 0];
      If[pdi == True, Print["retVal = ", retVal]];
      (*retVal=Re[ExH1vec[[3]]]/Ibase;*)If[pdi == True, (Print["E = ", Efield // MatrixForm, ", H = ", Hfield // MatrixForm, ", D = ", Dfield // MatrixForm, ", B = ", Bfield // MatrixForm];
      Print["E x B = ", ExBvec // MatrixForm, ", E x Conjugate[H] = ", ExHvec // MatrixForm, ", E x H = ", ExH1vec // MatrixForm];
      Print["E * Re[E x Conjugate[H]] = ", ExEHvecRe // MatrixForm, ", E * (E x Conjugate[H]) = ", (Flatten[Efield].ExHvec) // MatrixForm];
      Print["retVal = ", ToString[retVal]];
      Print["IFull completed."];
      Print["   "];)];
      Return[retVal];
    ];
(* ============================================== *)
RFull[FullSol_] :=
    Module[{Efield, Dfield, Hfield, Bfield, retVal, ExBvec, ExHvec, Ibase, ExH1vec, pdi, pdil, ExHvecRe, ExEHvecRe},
      opts = GetSolOptions[FullSol];
      pdi = PrintFunctionDebugInfo /. opts /. Options[FieldAlgebra];
      pdil = PrintFunctionDebugInfoLevel /. opts /. Options[FieldAlgebra];
      If[pdi == True, Print["RFull..."]];
      Ibase = IFullBase[FullSol];
      Efield = GetSolReflectedLightE[FullSol];
      Dfield = GetSolReflectedLightD[FullSol];
      Hfield = GetSolReflectedLightH[FullSol];
      Bfield = GetSolReflectedLightB[FullSol];
      ExBvec = Flatten[Efield]\[Cross]Flatten[Conjugate[Bfield]];
      ExHvec = Flatten[Efield]\[Cross]Flatten[Conjugate[Hfield]];
      ExH1vec = Flatten[Efield]\[Cross]Flatten[Hfield];
      ExHvecRe = Re[ExHvec];
      ExEHvecRe = Flatten[Efield].ExHvecRe;
      retVal = If[Abs[Ibase] > 0, Re[ExHvec[[3]]] / Ibase, 0, 0];
      (*retVal=Re[ExH1vec[[3]]]/Ibase;*)If[pdi == True, Print["E = ", Efield // MatrixForm, ", H = ", Hfield // MatrixForm, ", D = ", Dfield // MatrixForm, ", B = ", Bfield // MatrixForm];
      Print["E x B = ", ExBvec // MatrixForm, ", E x Conjugate[H] = ", ExHvec // MatrixForm, ", E x H = ", ExH1vec // MatrixForm];
      Print["E * Re[E x Conjugate[H]] = ", ExEHvecRe // MatrixForm, ", E * (E x Conjugate[H]) = ", (Flatten[Efield].ExHvec) // MatrixForm];
      Print["retVal = ", retVal];];
      Return[-retVal];
    ];
(* ============================================== *)
TFull[FullSol_] :=
    Module[{Efield, Dfield, Hfield, Bfield, retVal, ExBvec, ExHvec, Ibase, ExH1vec, pdi, pdil, ExHvecRe, ExEHvecRe},
      opts = GetSolOptions[FullSol];
      pdi = PrintFunctionDebugInfo /. opts /. Options[FieldAlgebra];
      pdil = PrintFunctionDebugInfoLevel /. opts /. Options[FieldAlgebra];
      If[pdi == True, Print["TFull..."]];
      Ibase = IFullBase[FullSol];
      Efield = GetSolTransmittedLightE[FullSol];
      Dfield = GetSolTransmittedLightD[FullSol];
      Hfield = GetSolTransmittedLightH[FullSol];
      Bfield = GetSolTransmittedLightB[FullSol];
      ExBvec = Flatten[Efield]\[Cross]Flatten[Conjugate[Bfield]];
      ExHvec = Flatten[Efield]\[Cross]Flatten[Conjugate[Hfield]];
      ExH1vec = Flatten[Efield]\[Cross]Flatten[Hfield];
      ExHvecRe = Re[ExHvec];
      ExEHvecRe = Flatten[Efield].ExHvecRe;
      retVal = If[Abs[Ibase] > 0, Re[ExHvec[[3]]] / Ibase, 0, 0];
      (*retVal=Re[ExH1vec[[3]]]/Ibase;*)If[pdi == True, Print["E = ", Efield // MatrixForm, ", H = ", Hfield // MatrixForm, ", D = ", Dfield // MatrixForm, ", B = ", Bfield // MatrixForm];
      Print["E x B = ", ExBvec // MatrixForm, ", E x Conjugate[H] = ", ExHvec // MatrixForm, ", E x H = ", ExH1vec // MatrixForm];
      Print["E * Re[E x Conjugate[H]] = ", ExEHvecRe // MatrixForm, ", E * (E x Conjugate[H]) = ", (Flatten[Efield].ExHvec) // MatrixForm];
      Print["retVal = ", retVal];];
      Return[retVal];
    ];
(* ============================================== *)
(*Rx[FullSol_]:=Module[{xxxx},Check[(Clear[xxxx];xxxx=GetSolReflectedLight[FullSol];(Abs[xxxx[[1]]]^2+Abs[xxxx[[3]]]^2)),0]];
Ry[FullSol_]:=Module[{xxxx},Check[(Clear[xxxx];GetSolReflectedLight[FullSol];(Abs[xxxx[[2]]]^2)),0]];*)

(* ============================================== *)
(*Tx[FullSol_]:=Module[{xxxx},Check[(Clear[xxxx];
xxxx=GetSolTransmittedLight[FullSol];(Abs[xxxx[[1]]]^2+Abs[xxxx[[3]]]^2)),0]];
Ty[FullSol_]:=Module[{xxxx},Check[(Clear[xxxx];xxxx=GetSolTransmittedLight[FullSol];(Abs[xxxx[[2]]]^2)),0]];*)
(* ============================================== *)
Ix[FullSol_] :=
    Module[{Efield, Dfield, Hfield, Bfield, retVal, ExBvec, ExHvec, Ibase, ExH1vec, opts, pdi, pdil, ExHvecRe, ExEHvecRe},
      opts = GetSolOptions[FullSol];
      pdi = PrintFunctionDebugInfo /. opts /. Options[FieldAlgebra];
      pdil = PrintFunctionDebugInfoLevel /. opts /. Options[FieldAlgebra];
      If[pdi == True, Print["Ix..."]];
      Ibase = IFullBase[FullSol];
      Efield = GetSolIncidentLightE[FullSol];
      Dfield = GetSolIncidentLightD[FullSol];
      Hfield = GetSolIncidentLightH[FullSol];
      Bfield = GetSolIncidentLightB[FullSol];
      Efield = Flatten[Efield];
      Dfield = Flatten[Dfield];
      Hfield = Flatten[Hfield];
      Bfield = Flatten[Bfield];
      Efield[[2]] = 0;
      Efield[[3]] = 0;
      Hfield[[1]] = 0;
      Hfield[[3]] = 0;
      ExBvec = Efield\[Cross]Conjugate[Bfield];
      ExHvec = Efield\[Cross]Conjugate[Hfield];
      ExH1vec = Efield\[Cross]Hfield;
      ExHvecRe = Re[ExHvec];
      ExEHvecRe = Efield.ExHvecRe;
      If[pdi == True, Print["E = ", Efield // MatrixForm, ", H = ", Hfield // MatrixForm, ", D = ", Dfield // MatrixForm, ", B = ", Bfield // MatrixForm];
      Print["E x B = ", ExBvec // MatrixForm, ", E x Conjugate[H] = ", ExHvec // MatrixForm, ", E x H = ", ExH1vec // MatrixForm];
      Print["E * Re[E x Conjugate[H]] = ", ExEHvecRe // MatrixForm, ", E * (E x Conjugate[H]) = ", (Flatten[Efield].ExHvec) // MatrixForm];];
      retVal = If[Abs[Ibase] > 0, Re[ExHvec[[3]]] / Ibase, 0, 0];
      (*retVal=Re[ExH1vec[[3]]]/Ibase;*)
      Return[retVal];
    ];
(* ============================================== *)
Rx[FullSol_] :=
    Module[{Efield, Dfield, Hfield, Bfield, retVal, ExBvec, ExHvec, Ibase, ExH1vec, pdi, pdil, ExHvecRe, ExEHvecRe},
      opts = GetSolOptions[FullSol];
      pdi = PrintFunctionDebugInfo /. opts /. Options[FieldAlgebra];
      pdil = PrintFunctionDebugInfoLevel /. opts /. Options[FieldAlgebra];
      If[pdi == True, Print["Rx..."]];
      Ibase = IFullBase[FullSol];
      Efield = GetSolReflectedLightE[FullSol];
      Dfield = GetSolReflectedLightD[FullSol];
      Hfield = GetSolReflectedLightH[FullSol];
      Bfield = GetSolReflectedLightB[FullSol];
      Efield = Flatten[Efield];
      Dfield = Flatten[Dfield];
      Hfield = Flatten[Hfield];
      Bfield = Flatten[Bfield];
      Efield[[2]] = 0;
      Efield[[3]] = 0;
      Hfield[[1]] = 0;
      Hfield[[3]] = 0;
      ExBvec = Efield\[Cross]Conjugate[Bfield];
      ExHvec = Efield\[Cross]Conjugate[Hfield];
      ExH1vec = Efield\[Cross]Hfield;
      ExHvecRe = Re[ExHvec];
      ExEHvecRe = Efield.ExHvecRe;
      If[pdi == True, Print["E = ", Efield // MatrixForm, ", H = ", Hfield // MatrixForm, ", D = ", Dfield // MatrixForm, ", B = ", Bfield // MatrixForm];
      Print["E x B = ", ExBvec // MatrixForm, ", E x Conjugate[H] = ", ExHvec // MatrixForm, ", E x H = ", ExH1vec // MatrixForm];
      Print["E * Re[E x Conjugate[H]] = ", ExEHvecRe // MatrixForm, ", E * (E x Conjugate[H]) = ", (Flatten[Efield].ExHvec) // MatrixForm];];
      retVal = If[Abs[Ibase] > 0, Re[ExHvec[[3]]] / Ibase, 0, 0];
      (*retVal=Re[ExH1vec[[3]]]/Ibase;*)
      Return[-retVal];
    ];
(* ============================================== *)
Tx[FullSol_] :=
    Module[{Efield, Dfield, Hfield, Bfield, retVal, ExBvec, ExHvec, Ibase, ExH1vec, pdi, pdil, ExHvecRe, ExEHvecRe},
      opts = GetSolOptions[FullSol];
      pdi = PrintFunctionDebugInfo /. opts /. Options[FieldAlgebra];
      pdil = PrintFunctionDebugInfoLevel /. opts /. Options[FieldAlgebra];
      If[pdi == True, Print["Tx..."]];
      Ibase = IFullBase[FullSol];
      Efield = GetSolTransmittedLightE[FullSol];
      Dfield = GetSolTransmittedLightD[FullSol];
      Hfield = GetSolTransmittedLightH[FullSol];
      Bfield = GetSolTransmittedLightB[FullSol];
      Efield = Flatten[Efield];
      Dfield = Flatten[Dfield];
      Hfield = Flatten[Hfield];
      Bfield = Flatten[Bfield];
      Efield[[2]] = 0;
      Efield[[3]] = 0;
      Hfield[[1]] = 0;
      Hfield[[3]] = 0;
      ExBvec = Efield\[Cross]Conjugate[Bfield];
      ExHvec = Efield\[Cross]Conjugate[Hfield];
      ExH1vec = Efield\[Cross]Hfield;
      ExHvecRe = Re[ExHvec];
      ExEHvecRe = Efield.ExHvecRe;

      If[pdi == True,
        Print["E = ", Efield // MatrixForm, ", H = ", Hfield // MatrixForm, ", D = ", Dfield // MatrixForm, ", B = ", Bfield // MatrixForm];
        Print["E x B = ", ExBvec // MatrixForm, ", E x Conjugate[H] = ", ExHvec // MatrixForm, ", E x H = ", ExH1vec // MatrixForm];
        Print["E * Re[E x Conjugate[H]] = ", ExEHvecRe // MatrixForm, ", E * (E x Conjugate[H]) = ", (Flatten[Efield].ExHvec) // MatrixForm];
      ];

      retVal = If[Abs[Ibase] > 0, Re[ExHvec[[3]]] / Ibase, 0, 0];

      (*
      Print["TX::Ibase = ", Ibase];
      Print["TX::ExHvec = ", ExHvec];
      Print["TX::Efield = ", Efield];
      Print["TX::Hfield = ", Hfield];
      Print["TX::retVal = ", retVal];
      *)

      (*retVal=Re[ExH1vec[[3]]]/Ibase;*)
      Return[retVal];
    ];
(* ============================================== *)Iy[FullSol_] := Module[{Efield, Dfield, Hfield, Bfield, retVal, ExBvec, ExHvec, Ibase, ExH1vec, opts, pdi, pdil, ExHvecRe, ExEHvecRe}, opts = GetSolOptions[FullSol];
pdi = PrintFunctionDebugInfo /. opts /. Options[FieldAlgebra];
pdil = PrintFunctionDebugInfoLevel /. opts /. Options[FieldAlgebra];
If[pdi == True, Print["Iy..."]];
Ibase = IFullBase[FullSol];
Efield = GetSolIncidentLightE[FullSol];
Dfield = GetSolIncidentLightD[FullSol];
Hfield = GetSolIncidentLightH[FullSol];
Bfield = GetSolIncidentLightB[FullSol];
Efield = Flatten[Efield];
Dfield = Flatten[Dfield];
Hfield = Flatten[Hfield];
Bfield = Flatten[Bfield];
Efield[[1]] = 0;
Efield[[3]] = 0;
Hfield[[2]] = 0;
Hfield[[3]] = 0;
ExBvec = Efield\[Cross]Conjugate[Bfield];
ExHvec = Efield\[Cross]Conjugate[Hfield];
ExH1vec = Efield\[Cross]Hfield;
ExHvecRe = Re[ExHvec];
ExEHvecRe = Efield.ExHvecRe;
If[pdi == True, Print["E = ", Efield // MatrixForm, ", H = ", Hfield // MatrixForm, ", D = ", Dfield // MatrixForm, ", B = ", Bfield // MatrixForm];
Print["E x B = ", ExBvec // MatrixForm, ", E x Conjugate[H] = ", ExHvec // MatrixForm, ", E x H = ", ExH1vec // MatrixForm];
Print["E * Re[E x Conjugate[H]] = ", ExEHvecRe // MatrixForm, ", E * (E x Conjugate[H]) = ", (Flatten[Efield].ExHvec) // MatrixForm];];
retVal = If[Abs[Ibase] > 0, Re[ExHvec[[3]]] / Ibase, 0, 0];
(*retVal=Re[ExH1vec[[3]]]/Ibase;*)Return[retVal];];
(* ============================================== *)
Ry[FullSol_] :=
    Module[{Efield, Dfield, Hfield, Bfield, retVal, ExBvec, ExHvec, Ibase, ExH1vec, pdi, pdil, ExHvecRe, ExEHvecRe},
      opts = GetSolOptions[FullSol];
      pdi = PrintFunctionDebugInfo /. opts /. Options[FieldAlgebra];
      pdil = PrintFunctionDebugInfoLevel /. opts /. Options[FieldAlgebra];
      If[pdi == True, Print["Ry..."]];
      Ibase = IFullBase[FullSol];
      Efield = GetSolReflectedLightE[FullSol];
      Dfield = GetSolReflectedLightD[FullSol];
      Hfield = GetSolReflectedLightH[FullSol];
      Bfield = GetSolReflectedLightB[FullSol];
      Efield = Flatten[Efield];
      Dfield = Flatten[Dfield];
      Hfield = Flatten[Hfield];
      Bfield = Flatten[Bfield];
      Efield[[1]] = 0;
      Efield[[3]] = 0;
      Hfield[[2]] = 0;
      Hfield[[3]] = 0;
      ExBvec = Efield\[Cross]Conjugate[Bfield];
      ExHvec = Efield\[Cross]Conjugate[Hfield];
      ExH1vec = Efield\[Cross]Hfield;
      ExHvecRe = Re[ExHvec];
      ExEHvecRe = Efield.ExHvecRe;
      If[pdi == True, Print["E = ", Efield // MatrixForm, ", H = ", Hfield // MatrixForm, ", D = ", Dfield // MatrixForm, ", B = ", Bfield // MatrixForm];
      Print["E x B = ", ExBvec // MatrixForm, ", E x Conjugate[H] = ", ExHvec // MatrixForm, ", E x H = ", ExH1vec // MatrixForm];
      Print["E * Re[E x Conjugate[H]] = ", ExEHvecRe // MatrixForm, ", E * (E x Conjugate[H]) = ", (Flatten[Efield].ExHvec) // MatrixForm];];
      retVal = If[Abs[Ibase] > 0, Re[ExHvec[[3]]] / Ibase, 0, 0];
      (*retVal=Re[ExH1vec[[3]]]/Ibase;*)
      Return[-retVal];
    ];
(* ============================================== *)
Ty[FullSol_] :=
    Module[{Efield, Dfield, Hfield, Bfield, retVal, ExBvec, ExHvec, Ibase, ExH1vec, pdi, pdil, ExHvecRe, ExEHvecRe},
      opts = GetSolOptions[FullSol];
      pdi = PrintFunctionDebugInfo /. opts /. Options[FieldAlgebra];
      pdil = PrintFunctionDebugInfoLevel /. opts /. Options[FieldAlgebra];
      If[pdi == True, Print["Ty..."]];
      Ibase = IFullBase[FullSol];
      Efield = GetSolTransmittedLightE[FullSol];
      Dfield = GetSolTransmittedLightD[FullSol];
      Hfield = GetSolTransmittedLightH[FullSol];
      Bfield = GetSolTransmittedLightB[FullSol];
      Efield = Flatten[Efield];
      Dfield = Flatten[Dfield];
      Hfield = Flatten[Hfield];
      Bfield = Flatten[Bfield];
      Efield[[1]] = 0;
      Efield[[3]] = 0;
      Hfield[[2]] = 0;
      Hfield[[3]] = 0;
      ExBvec = Efield\[Cross]Conjugate[Bfield];
      ExHvec = Efield\[Cross]Conjugate[Hfield];
      ExH1vec = Efield\[Cross]Hfield;
      ExHvecRe = Re[ExHvec];
      ExEHvecRe = Efield.ExHvecRe;
      If[pdi == True, Print["E = ", Efield // MatrixForm, ", H = ", Hfield // MatrixForm, ", D = ", Dfield // MatrixForm, ", B = ", Bfield // MatrixForm];
      Print["E x B = ", ExBvec // MatrixForm, ", E x Conjugate[H] = ", ExHvec // MatrixForm, ", E x H = ", ExH1vec // MatrixForm];
      Print["E * Re[E x Conjugate[H]] = ", ExEHvecRe // MatrixForm, ", E * (E x Conjugate[H]) = ", (Flatten[Efield].ExHvec) // MatrixForm];];
      retVal = If[Abs[Ibase] > 0, Re[ExHvec[[3]]] / Ibase, 0, 0];
      (*retVal=Re[ExH1vec[[3]]]/Ibase;*)
      Return[retVal];
    ];
(* ============================================== *)
pVector[vec : {_, _, _}] :=
    Module[{retval},
      retval = If[(I * vec.Conjugate[vec]) == 0, {0, 0, 0}, -Re[Cross[vec, Conjugate[vec]] / (I * vec.Conjugate[vec])];];
      Return[retval];
    ];
(* ============================================== *)
pVectorElliplicity[vec : {_, _, _}] :=
    Module[{retval, pVec, pVecAbs, elt, eltSol, eltVal},
      pVec = pVector[vec];
      pVecAbs = Sqrt[Abs[pVec.Conjugate[pVec]]];
      If[pVecAbs != 0, eltSol = NSolve[pVecAbs == 2 * elt / (elt^2 + 1), elt], eltSol = {elt -> 0};];
      eltVal = Table[Re[elt /. eltSol], {iii, 1, Length[eltSol]}];
      retval = Min[eltVal];
      Return[retval];
    ];
(* ============================================== *)
qVector[vec : {_, _, _}] :=
    Module[{retval},
      If[(vec.Conjugate[vec]) == 0 || (vec.vec) == 0,
        retval = {0, 0, 0},
        retval = Re[(Abs[vec.vec] / (vec.Conjugate[vec])) * (Re[vec / Sqrt[vec.vec]] / Sqrt[Re[vec / Sqrt[vec.vec]].Re[vec / Sqrt[vec.vec]]])]
      ];
      Return[retval];
    ];
(* ============================================== *)
FieldEllipticity[ehField : {_, _, _, _, _, _}, setSign_, useZAxis_, direction_] :=
    Module[{retval, eField, hField},
      eField = Table[ehField[[iii]], {iii, 1, 3}];
      hField = Table[ehField[[iii + 3]], {iii, 1, 3}];
      retval = FieldEllipticity[eField, hField, setSign, useZAxis, direction];
      Return[retval];
    ];

FieldEllipticity[ehField : {_, _, _, _, _, _, _}, setSign_, useZAxis_, direction_] :=
    Module[{retval, eField, hField},
      eField = Table[ehField[[iii]], {iii, 1, 3}];
      hField = Table[ehField[[iii + 3]], {iii, 1, 3}];
      retval = FieldEllipticity[eField, hField, setSign, useZAxis, direction];
      Return[retval];
    ];

FieldEllipticity[eField : {_, _, _}, hField : {_, _, _}, setSign_, useZAxis_, direction_] :=
    Module[{retval, pVec, qVec, pVecElct, poyntVec, pVecXpountVec},
      pVec = pVector[eField];
      pVecElct = pVectorElliplicity[eField];
      qVec = qVector[eField];
      poyntVec = If[useZAxis == True, If[direction == True, {0, 0, 1}, {0, 0, -1}, {0, 0, 1}], PoyntingVector[eField, hField], PoyntingVector[eField, hField]];
      pVecXpountVec = pVec.poyntVec;
      retval = If[pVecXpountVec >= 0 || setSign == False, pVecElct, -pVecElct, pVecElct];
      (*Print["FieldEllipticity: pVec = ",pVec,", poyntVec =",poyntVec,", pVecXpountVec = ",pVecXpountVec,", retval = ",retval];*)
      Return[retval];
    ];
(* ============================================== *)
FieldAzimuth[ehField : {_, _, _, _, _, _}, setSign_, useZAxis_, direction_] :=
    Module[{retval, eField, hField},
      eField = Table[ehField[[iii]], {iii, 1, 3}];
      hField = Table[ehField[[iii + 3]], {iii, 1, 3}];
      retval = FieldAzimuth[eField, hField, setSign, useZAxis, direction];
      Return[retval];
    ];

FieldAzimuth[ehField : {_, _, _, _, _, _}, setSign_, useZAxis_, direction_, elpCutOff_] :=
    Module[{retval, eField, hField},
      eField = Table[ehField[[iii]], {iii, 1, 3}];
      hField = Table[ehField[[iii + 3]], {iii, 1, 3}];
      retval = FieldAzimuth[eField, hField, setSign, useZAxis, direction, elpCutOff];
      Return[retval];
    ];

FieldAzimuth[ehField : {_, _, _, _, _, _, _}, setSign_, useZAxis_, direction_] :=
    Module[{retval, eField, hField},
      eField = Table[ehField[[iii]], {iii, 1, 3}];
      hField = Table[ehField[[iii + 3]], {iii, 1, 3}];
      retval = FieldAzimuth[eField, hField, setSign, useZAxis, direction];
      Return[retval];
    ];

FieldAzimuth[ehField : {_, _, _, _, _, _, _}, setSign_, useZAxis_, direction_, elpCutOff_] :=
    Module[{retval, eField, hField},
      eField = Table[ehField[[iii]], {iii, 1, 3}];
      hField = Table[ehField[[iii + 3]], {iii, 1, 3}];
      retval = FieldAzimuth[eField, hField, setSign, useZAxis, direction, elpCutOff];
      Return[retval];
    ];

FieldAzimuth[eField : {_, _, _}, hField : {_, _, _}, setSign_, useZAxis_, direction_] :=
    Module[{elpCutOff, retval},
      elpCutOff = EllipticityCutOffForAzimyth /. Options[FieldAlgebra];
      retval = FieldAzimuth[eField, hField, setSign, useZAxis, direction, elpCutOff];
      Return[retval];
    ];

FieldAzimuth[eField : {_, _, _}, hField : {_, _, _}, setSign_, useZAxis_, direction_, elpCutOff_] :=
    Module[{retval, pVec, qVec, poyntVec, pVecDpountVec, eyVec, epVec, eqVec, eaVec, aVec, cosFi, rVec, erVec, eaVecDeqVec, fiAngle, elpCutOffVar, fieldElp},
    (*Print["   "];*)
      elpCutOffVar = If[Re[elpCutOff] < 0, 2, Re[elpCutOff], 2];
      fieldElp = FieldEllipticity[eField, hField, setSign, useZAxis, direction];
      If[fieldElp > elpCutOffVar,
      (*When ellipticily is too close to 1 we cannot determine the azimyth precisely.So we use cut off to return 0 in that case.*)
        retval = 0,
        (
          eyVec = If[direction == True, {0, 1, 0}, {0, -1, 0}, {0, 1, 0}];

          pVec = pVector[eField];
          poyntVec = PoyntingVector[eField, hField];
          pVecDpountVec = pVec.poyntVec;

          pVec = If[pVecDpountVec < 0, -pVec, pVec, pVec];

          (*Print["FieldAzimuth:: eField = ",eField,", pVec = ",pVec,", poyntVec = ",poyntVec];*)

          If[pVec.pVec < PVECTORTOLERANCE, pVec = poyntVec];

          epVec = If[pVec.pVec > 0, pVec / Sqrt[pVec.pVec], {0, 0, 0}];
          qVec = qVector[eField];
          eqVec = If[qVec.qVec > 0, qVec / Sqrt[qVec.qVec], {0, 0, 0}];
          rVec = Cross[epVec, eqVec];
          erVec = If[rVec.rVec > 0, rVec / Sqrt[rVec.rVec], {0, 0, 0}];
          aVec = Cross[eyVec, pVec];
          eaVec = If[aVec.aVec > 0, aVec / Sqrt[aVec.aVec], {0, 0, 0}];
          (*Print["FieldAzimuth:: epVec = ",epVec,", eqVec = ",eqVec,", erVec = ",erVec,", eaVec = ",eaVec];*)
          eaVecDeqVec = eaVec.eqVec;
          eaVec = If[eaVecDeqVec < 0, -eaVec, eaVec, eaVec];
          (*Print["FieldAzimuth:: eaVecXeqVec = ",eaVecDeqVec,", new eaVec = ",eaVec];*)
          eaVecDeqVec = eaVec.eqVec;
          cosFi = Max[Min[eaVecDeqVec, 1], -1];
          fiAngle = Re[ArcCos[cosFi]];
          retval = If[(Cross[eaVec, eqVec].Cross[erVec, eqVec]) > 0, -fiAngle, fiAngle, fiAngle];
        ),
        retval = 0
      ];
      (*Print["FieldAzimuth:: eaVecDeqVec = ",eaVecDeqVec,", fiAngle = ",fiAngle,", cosFi = ",cosFi,", retval = ",retval];*)
      Return[retval];

    ];
(* ============================================== *)
Elt[FullSol_] :=
    Module[{ehField, retval, opts, setElctSgn, useZAxis},
      opts = GetSolOptions[FullSol];
      setElctSgn = SetEllipticitySignT /. opts /. Options[FieldAlgebra];
      useZAxis = UseZAxisForEllipticityT /. opts /. Options[FieldAlgebra];
      ehField = GetSolTransmittedLight[FullSol];
      retval = FieldEllipticity[ehField, setElctSgn, useZAxis, True];
      Return[retval];
    ];

Xit[FullSol_] :=
    Module[{ehField, retval, opts, setElctSgn, useZAxis, elpCutOff},
      opts = GetSolOptions[FullSol];
      setElctSgn = SetEllipticitySignT /. opts /. Options[FieldAlgebra];
      useZAxis = UseZAxisForEllipticityT /. opts /. Options[FieldAlgebra];
      elpCutOff = EllipticityCutOffForAzimyth /. Options[FieldAlgebra];
      ehField = GetSolTransmittedLight[FullSol];
      retval = FieldAzimuth[ehField, setElctSgn, useZAxis, True, elpCutOff];
      Return[retval];
    ];

CosXit[FullSol_] := Cos[Xit[FullSol]];
SinXit[FullSol_] := Sin[Xit[FullSol]];
Sin2Xit[FullSol_] := (2 * SinXit[FullSol] * CosXit[FullSol]);
XitDegree[FullSol_] := (Xit[FullSol] / Degree);
(* ============================================== *)
Eli[FullSol_] := Module[{ehField, retval, opts, setElctSgn, useZAxis}, opts = GetSolOptions[FullSol];
setElctSgn = SetEllipticitySignI /. opts /. Options[FieldAlgebra];
useZAxis = UseZAxisForEllipticityI /. opts /. Options[FieldAlgebra];
ehField = GetSolIncidentLight[FullSol];
retval = FieldEllipticity[ehField, setElctSgn, useZAxis, True];
Return[retval];];

Xii[FullSol_] := Module[{ehField, retval, opts, setElctSgn, useZAxis, elpCutOff}, opts = GetSolOptions[FullSol];
setElctSgn = SetEllipticitySignI /. opts /. Options[FieldAlgebra];
useZAxis = UseZAxisForEllipticityI /. opts /. Options[FieldAlgebra];
elpCutOff = EllipticityCutOffForAzimyth /. Options[FieldAlgebra];
ehField = GetSolIncidentLight[FullSol];
retval = FieldAzimuth[ehField, setElctSgn, useZAxis, True, elpCutOff];
Return[retval];];

CosXii[FullSol_] := Cos[Xii[FullSol]];
SinXii[FullSol_] := Sin[Xii[FullSol]];
Sin2Xii[FullSol_] := (2 * SinXii[FullSol] * CosXii[FullSol]);
XiiDegree[FullSol_] := (Xii[FullSol] / Degree);
(* ============================================== *)
Elr[FullSol_] := Module[{ehField, retval, opts, setElctSgn, useZAxis}, opts = GetSolOptions[FullSol];
setElctSgn = SetEllipticitySignR /. opts /. Options[FieldAlgebra];
useZAxis = UseZAxisForEllipticityR /. opts /. Options[FieldAlgebra];
ehField = GetSolReflectedLight[FullSol];
retval = FieldEllipticity[ehField, setElctSgn, useZAxis, False];
Return[retval];];

Xir[FullSol_] := Module[{ehField, retval, opts, setElctSgn, useZAxis, elpCutOff}, opts = GetSolOptions[FullSol];
setElctSgn = SetEllipticitySignR /. opts /. Options[FieldAlgebra];
useZAxis = UseZAxisForEllipticityR /. opts /. Options[FieldAlgebra];
elpCutOff = EllipticityCutOffForAzimyth /. Options[FieldAlgebra];
ehField = GetSolReflectedLight[FullSol];
retval = FieldAzimuth[ehField, setElctSgn, useZAxis, False, elpCutOff];
Return[retval];];

CosXir[FullSol_] := Cos[Xir[FullSol]];
SinXir[FullSol_] := Sin[Xir[FullSol]];
Sin2Xir[FullSol_] := (2 * SinXir[FullSol] * CosXir[FullSol]);
XirDegree[FullSol_] := (Xir[FullSol] / Degree);
(* ============================================== *)
RAnalyzer[FullSol_] := Module[{anInf, fita, ex, ey, ehFull, exy, anAngle, anParAmpl, anCrossAmpl, rotm, anTrf, exyOut, retval}, anInf = SolutionGetReflectedAnalyzerInfo[FullSol];
anAngle = AnalyzerInfoGetAngle[anInf];
anParAmpl = AnalyzerInfoGetParallelAmplitude[anInf];
anCrossAmpl = AnalyzerInfoGetCrossedAmplitude[anInf];
fita = IncidentLightFita[GetSolIncidentLightInfo[FullSol]];
ehFull = FullSol[[2]];ex = Check[ehFull[[1]] / Cos[fita], ehFull[[3]]];
ey = ehFull[[2]];exy = {{ex}, {ey}};rotm = RotationMatrix2D[anAngle];
anTrf = {{anParAmpl, 0}, {0, anCrossAmpl}};exyOut = anTrf.rotm.exy;
retval = Abs[exyOut[[1, 1]]]^2 + Abs[exyOut[[2, 1]]]^2;
(*Print["exy = ",N[Chop[exy]],", exyOut = ",N[Chop[exyOut]],", RAnalyzer = ",N[retval],", Original = ",N[Abs[exy[[1,1]]]^2+Abs[exy[[2,1]]]^2],", anAngle = ",anAngle];*)Return[retval];];
(* ============================================== *)
TAnalyzer[FullSol_] := Module[{anInf, fita, ex, ey, ehFull, exy, anAngle, anParAmpl, anCrossAmpl, rotm, anTrf, exyOut, retval, opts, n1, n2, utll}, anInf = SolutionGetTransmittedAnalyzerInfo[FullSol];
opts = GetSolOptions[FullSol];
anAngle = AnalyzerInfoGetAngle[anInf];
anParAmpl = AnalyzerInfoGetParallelAmplitude[anInf];
anCrossAmpl = AnalyzerInfoGetCrossedAmplitude[anInf];
utll = UseThickLastLayer /. opts /. Options[BerremanDirect];
n1 = MediaUpperRefractionIndex[GetSolMedia[FullSol]];
n2 = MediaLowerRefractionIndex[GetSolMedia[FullSol]];
If[utll === True, n2 = MediaOutRefractionIndex[GetSolMedia[FullSol]]];
fita = PsiAngle[IncidentLightFita[GetSolIncidentLightInfo[FullSol]], n1, n2];
ehFull = FullSol[[3]];ex = Check[ehFull[[1]] / Cos[fita], ehFull[[3]]];
ey = ehFull[[2]];exy = {{ex}, {ey}};rotm = RotationMatrix2D[anAngle];
anTrf = {{anParAmpl, 0}, {0, anCrossAmpl}};exyOut = anTrf.rotm.exy;
retval = Abs[exyOut[[1, 1]]]^2 + Abs[exyOut[[2, 1]]]^2;
(*Print["exy = ",N[Chop[exy]],", exyOut = ",N[Chop[exyOut]],", TAnalyzer = ",N[retval],", Original = ",N[Abs[exy[[1,1]]]^2+Abs[exy[[2,1]]]^2],", anAngle = ",anAngle];*)Return[retval];];

DetM2[FullSol_] := Module[{retval}, retval = Det[GetSolM2[FullSol]];Return[retval];];
DetPPP[FullSol_] := Module[{retval}, retval = Det[GetSolPPP[FullSol]];Return[retval];];
DetCoeff[FullSol_] := Module[{retval}, retval = Abs[Det[GetSolCoeff[FullSol]]];
Return[retval];];
ReDetCoeff[FullSol_] := Module[{retval}, retval = Abs[Re[Det[GetSolCoeff[FullSol]]]];
Return[retval];];
ImDetCoeff[FullSol_] := Module[{retval}, retval = Abs[Im[Det[GetSolCoeff[FullSol]]]];
Return[retval];];

(* ============================================== *)
LRFull[FullSol_] := Module[{xxxx, retval}, Clear[xxxx];xxxx = N[RFull[FullSol]];
retval = If[xxxx > 0, Log[10, xxxx], 0];Return[retval];];
LRx[FullSol_] := Module[{xxxx, retval}, Clear[xxxx];xxxx = N[Rx[FullSol]];
retval = If[xxxx > 0, Log[10, xxxx], 0];Return[retval];];
LRy[FullSol_] := Module[{xxxx, retval}, Clear[xxxx];xxxx = N[Ry[FullSol]];
retval = If[xxxx > 0, Log[10, xxxx], 0];Return[retval];];

LTFull[FullSol_] := Module[{xxxx, retval}, Clear[xxxx];xxxx = N[TFull[FullSol]];
retval = If[xxxx > 0, Log[10, xxxx], 0];Return[retval];];
LTx[FullSol_] := Module[{xxxx, retval}, Clear[xxxx];xxxx = N[Tx[FullSol]];
retval = If[xxxx > 0, Log[10, xxxx], 0];Return[retval];];
LTy[FullSol_] := Module[{xxxx, retval}, Clear[xxxx];xxxx = N[Ty[FullSol]];
retval = If[xxxx > 0, Log[10, xxxx], 0];Return[retval];];
(* ============================================== *)
LnRFull[FullSol_] := Module[{xxxx, retval}, Clear[xxxx];
xxxx = N[RFull[FullSol]];retval = If[xxxx > 0, Log[xxxx], 0];Return[retval];];
LnRx[FullSol_] := Module[{xxxx, retval}, Clear[xxxx];
xxxx = N[Rx[FullSol]];retval = If[xxxx > 0, Log[xxxx], 0];Return[retval];];
LnRy[FullSol_] := Module[{xxxx, retval}, Clear[xxxx];xxxx = N[Ry[FullSol]];retval = If[xxxx > 0, Log[xxxx], 0];Return[retval];];

LnTFull[FullSol_] := Module[{xxxx, retval}, Clear[xxxx];xxxx = N[TFull[FullSol]];retval = If[xxxx > 0, Log[xxxx], 0];Return[retval];];
LnTx[FullSol_] := Module[{xxxx, retval}, Clear[xxxx];xxxx = N[Tx[FullSol]];retval = If[xxxx > 0, Log[xxxx], 0];Return[retval];];
LnTy[FullSol_] := Module[{xxxx, retval}, Clear[xxxx];xxxx = N[Ty[FullSol]];retval = If[xxxx > 0, Log[xxxx], 0];Return[retval];];
(* ============================================== *)
RxAccuracy[FullSol_] := Module[{}, Return[Accuracy[Rx[FullSol]]];];
DetM2Accuracy[FullSol_] := Module[{}, Return[Accuracy[MMatrix2[FullSol]]];];
DetCoeffAccuracy[FullSol_] := Module[{}, Return[Accuracy[DetCoeff[FullSol]]];];
(* ============================================== *)
LDetM2[FullSol_] := Module[{xxxx, retval}, Clear[xxxx];
xxxx = N[Abs[MMatrix2[FullSol]]];
retval = If[xxxx > 0, Log[10, xxxx], 0];
Return[retval];];
(* ============================================== *)
M2EValRe[evi_, FullSol_] := Module[{retval, opts, evIdx, egsys, optsFunc}, evIdx = Max[Min[evi, 4], 1];
egsys = GetSolEGSys2[FullSol];
retval = Re[N[egsys[[1, evIdx]]]];
Return[retval];];
(* ============================================== *)
M2EValIm[evi_, FullSol_] := Module[{retval, opts, evIdx, egsys, optsFunc}, evIdx = Max[Min[evi, 4], 1];
egsys = GetSolEGSys2[FullSol];
retval = Im[N[egsys[[1, evIdx]]]];
Return[retval];];
(* ============================================== *)
ReEGE2[evi_, xyz_, FullSol_] := Module[{retval, evIdx, ehFld, eFld, hFld, fldIdx, pntgS, ehMult}, evIdx = Max[Min[evi, 4], 1];
fldIdx = Max[Min[xyz, 3], 1];
ehFld = GetSolEHTEG[FullSol, evIdx];
eFld = {ehFld[[1]], ehFld[[2]], ehFld[[3]]};
hFld = {ehFld[[4]], ehFld[[5]], ehFld[[6]]};
ehMult = If[Re[eFld[[3]]] >= 0, -1, 1, 1];
retval = N[Re[ehMult * eFld[[fldIdx]]]];
Return[retval];];
(* ============================================== *)
ImEGE2[evi_, xyz_, FullSol_] := Module[{retval, evIdx, ehFld, eFld, hFld, fldIdx, pntgS, ehMult}, evIdx = Max[Min[evi, 4], 1];
fldIdx = Max[Min[xyz, 3], 1];
ehFld = GetSolEHTEG[FullSol, evIdx];
eFld = {ehFld[[1]], ehFld[[2]], ehFld[[3]]};
hFld = {ehFld[[4]], ehFld[[5]], ehFld[[6]]};
ehMult = If[Re[eFld[[3]]] >= 0, -1, 1, 1];
retval = N[Im[ehMult * eFld[[fldIdx]]]];
Return[retval];];
(* ============================================== *)
ReEGH2[evi_, xyz_, FullSol_] := Module[{retval, evIdx, ehFld, eFld, hFld, fldIdx, pntgS, ehMult}, evIdx = Max[Min[evi, 4], 1];
fldIdx = Max[Min[xyz, 3], 1];
ehFld = GetSolEHTEG[FullSol, evIdx];
eFld = {ehFld[[1]], ehFld[[2]], ehFld[[3]]};
hFld = {ehFld[[4]], ehFld[[5]], ehFld[[6]]};
ehMult = If[Re[eFld[[3]]] >= 0, -1, 1, 1];
retval = N[Re[ehMult * hFld[[fldIdx]]]];
Return[retval];];
(* ============================================== *)
ImEGH2[evi_, xyz_, FullSol_] := Module[{retval, evIdx, ehFld, eFld, hFld, fldIdx, pntgS, ehMult}, evIdx = Max[Min[evi, 4], 1];
fldIdx = Max[Min[xyz, 3], 1];
ehFld = GetSolEHTEG[FullSol, evIdx];
eFld = {ehFld[[1]], ehFld[[2]], ehFld[[3]]};
hFld = {ehFld[[4]], ehFld[[5]], ehFld[[6]]};
ehMult = If[Re[eFld[[3]]] >= 0, -1, 1, 1];
retval = N[Im[ehMult * hFld[[fldIdx]]]];
Return[retval];];
(* ============================================== *)
PoyntingVector2[evi_, xyz_, FullSol_] := Module[{retval, evIdx, ehFld, eFld, hFld, fldIdx, pntgS}, evIdx = Max[Min[evi, 4], 1];
fldIdx = Max[Min[xyz, 3], 1];
ehFld = GetSolEHTEG[FullSol, evIdx];
eFld = {ehFld[[1]], ehFld[[2]], ehFld[[3]]};
hFld = {ehFld[[4]], ehFld[[5]], ehFld[[6]]};
pntgS = N[Re[Cross[eFld, Conjugate[hFld]]]];
retval = pntgS[[fldIdx]];
(*Print["eFld = ",N[eFld]];Print["hFld = ",N[hFld]];
Print["pntgS = ",N[pntgS]];Print["retval = ",N[retval]];*)Return[retval];];
(* ============================================== *)
PoyntingI[xyz_, FullSol_] := Module[{retval, evIdx, ehFld, eFld, hFld, fldIdx, pntgS}, ehFld = GetSolIncidentLight[FullSol];
fldIdx = Max[Min[xyz, 3], 1];
eFld = {ehFld[[1]], ehFld[[2]], ehFld[[3]]};
hFld = {ehFld[[4]], ehFld[[5]], ehFld[[6]]};
pntgS = N[Re[Cross[eFld, Conjugate[hFld]]]];retval = pntgS[[fldIdx]];
Return[retval];];
(* ============================================== *)
PoyntingIFull[FullSol_] := Module[{retval, evIdx, ehFld, eFld, hFld, fldIdx, pntgS}, ehFld = GetSolIncidentLight[FullSol];
eFld = {ehFld[[1]], ehFld[[2]], ehFld[[3]]};
hFld = {ehFld[[4]], ehFld[[5]], ehFld[[6]]};
pntgS = N[Re[Cross[eFld, Conjugate[hFld]]]];
retval = Sqrt[pntgS[[1]]^2 + pntgS[[2]]^2 + pntgS[[3]]^2];
Return[retval];];
(* ============================================== *)
PoyntingR[xyz_, FullSol_] := Module[{retval, evIdx, ehFld, eFld, hFld, fldIdx, pntgS}, ehFld = GetSolReflectedLight[FullSol];
fldIdx = Max[Min[xyz, 3], 1];
eFld = {ehFld[[1]], ehFld[[2]], ehFld[[3]]};
hFld = {ehFld[[4]], ehFld[[5]], ehFld[[6]]};
pntgS = N[Re[Cross[eFld, Conjugate[hFld]]]];retval = pntgS[[fldIdx]];
Return[retval];];
(* ============================================== *)
PoyntingRFull[FullSol_] := Module[{retval, evIdx, ehFld, eFld, hFld, fldIdx, pntgS}, ehFld = GetSolReflectedLight[FullSol];
eFld = {ehFld[[1]], ehFld[[2]], ehFld[[3]]};
hFld = {ehFld[[4]], ehFld[[5]], ehFld[[6]]};
pntgS = N[Re[Cross[eFld, Conjugate[hFld]]]];
retval = Sqrt[pntgS[[1]]^2 + pntgS[[2]]^2 + pntgS[[3]]^2];
Return[retval];];
(* ============================================== *)
PoyntingT[xyz_, FullSol_] := Module[{retval, evIdx, ehFld, eFld, hFld, fldIdx, pntgS}, ehFld = GetSolTransmittedLight[FullSol];
fldIdx = Max[Min[xyz, 3], 1];
eFld = {ehFld[[1]], ehFld[[2]], ehFld[[3]]};
hFld = {ehFld[[4]], ehFld[[5]], ehFld[[6]]};
pntgS = N[Re[Cross[eFld, Conjugate[hFld]]]];
retval = pntgS[[fldIdx]];
Return[retval];];
(* ============================================== *)
PoyntingTFull[FullSol_] := Module[{retval, evIdx, ehFld, eFld, hFld, fldIdx, pntgS}, ehFld = GetSolTransmittedLight[FullSol];
eFld = {ehFld[[1]], ehFld[[2]], ehFld[[3]]};
hFld = {ehFld[[4]], ehFld[[5]], ehFld[[6]]};
pntgS = N[Re[Cross[eFld, Conjugate[hFld]]]];
retval = Sqrt[pntgS[[1]]^2 + pntgS[[2]]^2 + pntgS[[3]]^2];
Return[retval];];
(* ============================================== *)
PoyntingRTFull[FullSol_] := Module[{retval}, retval = PoyntingTFull[FullSol] + PoyntingRFull[FullSol];
Return[retval];];
(* ============================================== *)
PoyntingIRTzDiff[FullSol_] := Module[{retval}, retval = PoyntingI[3, FullSol] - (PoyntingT[3, FullSol] - PoyntingR[3, FullSol]);
Return[retval];];
(* ============================================== *)
PoyntingXY2[evi_, FullSol_] := Module[{retval, pntgX, pntgY}, pntgX = PoyntingVector2[evi, 1, FullSol];
pntgY = PoyntingVector2[evi, 2, FullSol];
retval = Sqrt[pntgX^2 + pntgY^2];
Return[retval];];
(* ============================================== *)
PoyntingXYPhase2[evi_, FullSol_] := Module[{retval, pntgX, pntgY, fi, ro, sol, sol1, pntgXY}, pntgX = PoyntingVector2[evi, 1, FullSol];
pntgY = PoyntingVector2[evi, 2, FullSol];
pntgXY = PoyntingXY2[evi, FullSol];
sol = NSolve[{pntgX - ro * Sin[fi] == 0, pntgY - ro * Cos[fi] == 0, Sign[ro] * ro - ro == 0}, {fi, ro}];
sol1 = If[Head[sol] === Head[{}], sol, {{ro -> 0, fi -> 0}}];
(*Print["Head[sol] = ",Head[sol]];
Print["sol = ",sol];
Print["sol1 = ",sol1];*)(*retval=(180/Pi)*If[pntgX\[GreaterEqual]0,If[pntgY===0,Pi/2,ArcTan[pntgX/pntgY]],If[pntgY\[GreaterEqual]0,Pi-If[pntgY===0,Pi/2,ArcTan[pntgX/pntgY],-Pi+If[pntgY===0,Pi/2,ArcTan[pntgX/pntgY]]]]];*)retval = (180 / Pi) * (fi /. sol1[[1]]);
Return[retval];];

(* ============================================== *)
EI[xyz_, FullSol_] := Module[{retval, evIdx, ehFld, eFld, hFld, fldIdx}, ehFld = GetSolIncidentLight[FullSol];
fldIdx = Max[Min[xyz, 3], 1];
eFld = {ehFld[[1]], ehFld[[2]], ehFld[[3]]};
hFld = {ehFld[[4]], ehFld[[5]], ehFld[[6]]};
retval = eFld[[fldIdx]];
Return[retval];];
(* ============================================== *)
ER[xyz_, FullSol_] := Module[{retval, evIdx, ehFld, eFld, hFld, fldIdx}, ehFld = GetSolReflectedLight[FullSol];
fldIdx = Max[Min[xyz, 3], 1];
eFld = {ehFld[[1]], ehFld[[2]], ehFld[[3]]};
hFld = {ehFld[[4]], ehFld[[5]], ehFld[[6]]};
retval = eFld[[fldIdx]];
Return[retval];];
(* ============================================== *)
ET[xyz_, FullSol_] := Module[{retval, evIdx, ehFld, eFld, hFld, fldIdx}, ehFld = GetSolTransmittedLight[FullSol];
fldIdx = Max[Min[xyz, 3], 1];
eFld = {ehFld[[1]], ehFld[[2]], ehFld[[3]]};
hFld = {ehFld[[4]], ehFld[[5]], ehFld[[6]]};
retval = eFld[[fldIdx]];
Return[retval];];

(* ============================================== *)
EpsComponent[UpDown_, ReIm_, xyzRow_, xyzCol_, FullSol_] := Module[{retval, mtrx, row, col, Media, Film, len, mrtx}, Media = GetSolMedia[FullSol];
row = Max[Min[xyzRow, 3], 1];
col = Max[Min[xyzCol, 3], 1];
Film = MediaFilm[Media];
len = MediaFilmLength[Media];
If[UpDown > 0 && UpDown <= len, mrtx = FilmLayerEpsilon[Film[[UpDown]]], If[UpDown === -1, mrtx = MediaUpperEpsilon[Media], mrtx = MediaLowerEpsilon[Media]];];retval = N[If[ReIm === 1, Re[mrtx[[row, col]]], Im[mrtx[[row, col]]]]];
Return[retval];];
(* ============================================== *)
MuComponent[UpDown_, ReIm_, xyzRow_, xyzCol_, FullSol_] := Module[{retval, mtrx, row, col, Media, len, Film, Film, len, mrtx}, Media = GetSolMedia[FullSol];
row = Max[Min[xyzRow, 3], 1];
col = Max[Min[xyzCol, 3], 1];
Film = MediaFilm[Media];
len = MediaFilmLength[Media];
If[UpDown > 0 && UpDown <= len, mrtx = FilmLayerMu[Film[[UpDown]]], If[UpDown === -1, mrtx = MediaUpperMu[Media], mrtx = MediaLowerMu[Media]];];
retval = N[If[ReIm === 1, Re[mrtx[[row, col]]], Im[mrtx[[row, col]]]]];
Return[retval];];
(* ============================================== *)
RoComponent[UpDown_, ReIm_, xyzRow_, xyzCol_, FullSol_] := Module[{retval, mtrx, row, col, Media, Film, len, mrtx}, Media = GetSolMedia[FullSol];
row = Max[Min[xyzRow, 3], 1];
col = Max[Min[xyzCol, 3], 1];
Film = MediaFilm[Media];
len = MediaFilmLength[Media];
If[UpDown > 0 && UpDown <= len, mrtx = FilmLayerRo[Film[[UpDown]]], If[UpDown === -1, mrtx = MediaUpperRo[Media], mrtx = MediaLowerRo[Media]];];
retval = N[If[ReIm === 1, Re[mrtx[[row, col]]], Im[mrtx[[row, col]]]]];
Return[retval];];
(* ============================================== *)
XitEGBase[evi_, FullSol_] := Module[{retval, evIdx, ehField, opts, setElctSgn, useZAxis, elpCutOff, pdi, pdil}, opts = GetSolOptions[FullSol];
pdi = PrintFunctionDebugInfo /. opts /. Options[FieldAlgebra];
pdil = PrintFunctionDebugInfoLevel /. opts /. Options[FieldAlgebra];
If[pdi == True, Print["XitEG::Starting..."];
Print["XitEG::evi = ", evi];
Print["XitEG::FullSol = ", FullSol];];
setElctSgn = SetEllipticitySignT /. opts /. Options[FieldAlgebra];
useZAxis = UseZAxisForEllipticityT /. opts /. Options[FieldAlgebra];
elpCutOff = EllipticityCutOffForAzimyth /. Options[FieldAlgebra];
evIdx = Max[Min[evi, 4], 1];
ehField = GetSolEHTEG[FullSol, evIdx];
retval = FieldAzimuth[ehField, setElctSgn, useZAxis, True, elpCutOff];
Return[retval];];

XitEGDegreeBase[evi_, FullSol_] := (XitEGBase[evi, FullSol] / Degree);
CosXitEG[evi_, FullSol_] := Cos[XitEGBase[evi, FullSol]];
SinXitEG[evi_, FullSol_] := Sin[XitEGBase[evi, FullSol]];
Sin2XitEG[evi_, FullSol_] := (2 * SinXitEG[evi, FullSol] * CosXitEG[evi, FullSol]);
XitEG[evi_, FullSol_] := (ArcSin[Sin2XitEG[evi, FullSol]] / 2);
XitEGDegree[evi_, FullSol_] := (XitEG[evi, FullSol] / Degree);

EltEG[evi_, FullSol_] := Module[{evIdx, ehField, retval, opts, setElctSgn, useZAxis}, opts = GetSolOptions[FullSol];
setElctSgn = SetEllipticitySignT /. opts /. Options[FieldAlgebra];
useZAxis = UseZAxisForEllipticityT /. opts /. Options[FieldAlgebra];
evIdx = Max[Min[evi, 4], 1];
ehField = GetSolEHTEG[FullSol, evIdx];
retval = FieldEllipticity[ehField, setElctSgn, useZAxis, True];
Return[retval];];

(* ============================================== *)
(*s\[Equal]y,p\[Equal]x (after rotation to get rid of z)*)
MuellerMatrix[rSS_, rPP_, rPS_, rSP_] := Re[{{(rPP * Conjugate[rPP] + rPS * Conjugate[rPS] + rSP * Conjugate[rSP] + rSS * Conjugate[rSS]) / 2, (rPP * Conjugate[rPP] + rPS * Conjugate[rPS] - rSP * Conjugate[rSP] - rSS * Conjugate[rSS]) / 2, (rSP * Conjugate[rPP] + rSS * Conjugate[rPS] + rPP * Conjugate[rSP] + rPS * Conjugate[rSS]) / 2, (I / 2) * (rSP * Conjugate[rPP] + rSS * Conjugate[rPS] - rPP * Conjugate[rSP] - rPS * Conjugate[rSS])}, {(rPP * Conjugate[rPP] - rPS * Conjugate[rPS] + rSP * Conjugate[rSP] - rSS * Conjugate[rSS]) / 2, (rPP * Conjugate[rPP] - rPS * Conjugate[rPS] - rSP * Conjugate[rSP] + rSS * Conjugate[rSS]) / 2, (rSP * Conjugate[rPP] - rSS * Conjugate[rPS] + rPP * Conjugate[rSP] - rPS * Conjugate[rSS]) / 2, (I / 2) * (rSP * Conjugate[rPP] - rSS * Conjugate[rPS] - rPP * Conjugate[rSP] + rPS * Conjugate[rSS])}, {(rPS * Conjugate[rPP] + rPP * Conjugate[rPS] + rSS * Conjugate[rSP] + rSP * Conjugate[rSS]) / 2, (rPS * Conjugate[rPP] + rPP * Conjugate[rPS] - rSS * Conjugate[rSP] - rSP * Conjugate[rSS]) / 2, (rSS * Conjugate[rPP] + rSP * Conjugate[rPS] + rPS * Conjugate[rSP] + rPP * Conjugate[rSS]) / 2, (I / 2) * (rSS * Conjugate[rPP] + rSP * Conjugate[rPS] - rPS * Conjugate[rSP] - rPP * Conjugate[rSS])}, {(I / 2) * (-(rPS * Conjugate[rPP]) + rPP * Conjugate[rPS] - rSS * Conjugate[rSP] + rSP * Conjugate[rSS]), (I / 2) * (-(rPS * Conjugate[rPP]) + rPP * Conjugate[rPS] + rSS * Conjugate[rSP] - rSP * Conjugate[rSS]), (I / 2) * (-(rSS * Conjugate[rPP]) + rSP * Conjugate[rPS] - rPS * Conjugate[rSP] + rPP * Conjugate[rSS]), (rSS * Conjugate[rPP] - rSP * Conjugate[rPS] - rPS * Conjugate[rSP] + rPP * Conjugate[rSS]) / 2}}];
(* ============================================== *)
CalculateEllipsometricData[FullSol_] := Module[{solBeta0, solBeta90, eBeta0fieldR, eBeta90fieldR, eBeta0fieldT, eBeta90fieldT, fita, retVal},

(*
Print["CalculateEllipsometricData::Starting..."];
Print["CalculateEllipsometricData::FullSol = ",FullSol];
*)

  fita = IncidentLightFita[GetSolIncidentLightInfo[FullSol]];

  solBeta0 = GetSolBeta0Sol[FullSol];
  solBeta90 = GetSolBeta90Sol[FullSol];

  (*
Print["CalculateEllipsometricData::solBeta0 = ",solBeta0];
Print["CalculateEllipsometricData::solBeta90 = ",solBeta90];
*)

  eBeta0fieldR = GetSolReflectedLightE[solBeta0];
  eBeta90fieldR = GetSolReflectedLightE[solBeta90];
  eBeta0fieldT = GetSolTransmittedLightE[solBeta0];
  eBeta90fieldT = GetSolTransmittedLightE[solBeta90];

  (*
Print["CalculateEllipsometricData::eBeta0fieldR = ",eBeta0fieldR];
Print["CalculateEllipsometricData::eBeta90fieldR = ",eBeta90fieldR];
Print["CalculateEllipsometricData::eBeta0fieldT = ",eBeta0fieldT];
Print["CalculateEllipsometricData::eBeta90fieldT = ",eBeta90fieldT];
*)

  retVal = CalculateEllipsometricData[fita, eBeta0fieldR, eBeta90fieldR, eBeta0fieldT, eBeta90fieldT];
  Return[retVal];
];

CalculateEllipsometricData[fita_, er0R : {_, _, _}, er90R : {_, _, _}, er0T : {_, _, _}, er90T : {_, _, _}] :=
    Module[{fi, fiRule, fiMinusRule, rot, rotI, rotR, ERpRot, ERps, ERpp, ERsRot, ERss, ERsp, rpp, rps, rsp, rss, ropp, rops, rosp, psipp, psips, psisp, deltapp, deltaps, deltasp, retVal, muellMtrR, muellMtrT},
      fiRule = {fi -> fita};
      fiMinusRule = {fi -> -fita};
      rot = {{Cos[fi], 0, Sin[fi]}, {0, 1, 0}, {-Sin[fi], 0, Cos[fi]}};
      rotI = rot /. fiMinusRule;
      rotR = rot /. fiRule;

      (*
      Print["rot = ",rot//MatrixForm];
      Print["rotI = ",rotI//MatrixForm];
      Print["rotR = ",rotR//MatrixForm];
      Print["er0R = ",er0R//MatrixForm];
      Print["er90R = ",er90R//MatrixForm];
      Print["er0T = ",er0T//MatrixForm];
      Print["er90T = ",er90T//MatrixForm];
      *)

      ERpRot = Flatten[(rotR.er0R)];
      ERps = ERpRot[[2]];
      ERpp = Sqrt[Abs[ERpRot[[1]]]^2 + Abs[ERpRot[[3]]]^2];
      ERsRot = Flatten[(rotR.er90R)];
      ERss = ERsRot[[2]];
      ERsp = Sqrt[Abs[ERsRot[[1]]]^2 + Abs[ERsRot[[3]]]^2];

      (*
      Print["ERpRot = ",ERpRot//MatrixForm];
      Print["ERsRot = ",ERsRot//MatrixForm];
      *)

      rpp = ERpp;
      rps = ERps;
      rsp = ERsp;
      rss = ERss;

      (*
      Print["rpp = ",rpp];
      Print["rps = ",rps];
      Print["rsp = ",rsp];
      Print["rss = ",rss];
      *)

      ropp = rpp / rss;
      rops = rps / rss;
      rosp = rsp / rss;
      psipp = ArcTan[Abs[ropp]];
      psips = ArcTan[Abs[rops]];
      psisp = ArcTan[Abs[rosp]];
      deltapp = -Arg[ropp];
      deltaps = -Arg[rops];
      deltasp = -Arg[rosp];


      muellMtrR = MuellerMatrix[rss, rpp, rps, rsp];
      muellMtrT = Table[Indeterminate, {ii, 1, 4}, {jj, 1, 4}];
      retVal = {psipp, deltapp, psips, deltaps, psisp, deltasp, rss, rpp, rps, rsp, muellMtrR, muellMtrT};

      (*
      Print["ERpRot = ",ERpRot];
      Print["ERps = ",ERps];
      Print["ERpp = ",ERpp];
      Print["..."];
      Print["ERsRot = ",ERsRot];
      Print["ERss = ",ERss];
      Print["ERsp = ",ERsp];
      Print["..."];
      Print["ropp = ",ropp];
      Print["rops = ",rops];
      Print["rosp = ",rosp];
      Print["..."];
      Print["psipp = ",psipp];
      Print["psips = ",psips];
      Print["psisp = ",psisp];
      Print["..."];
      Print["deltapp = ",deltapp];
      Print["deltaps = ",deltaps];
      Print["deltasp = ",deltasp];
      Print["..."];
      Print["muellMtrR = ",muellMtrR // MatrixForm];
      Print["..."];
      *)

      Return[retVal];
    ];
(* ============================================== *)
(*Function to get various elements from the list of ellipsometric data*)
EDataGetPsiPP[eld : {___}] := eld[[1]];
EDataGetDeltaPP[eld : {___}] := eld[[2]];
EDataGetPsiPS[eld : {___}] := eld[[3]];
EDataGetDeltaPS[eld : {___}] := eld[[4]];
EDataGetPsiSP[eld : {___}] := eld[[5]];
EDataGetDeltaSP[eld : {___}] := eld[[6]];

EDataGetRss[eld : {___}] := eld[[7]];
EDataGetRpp[eld : {___}] := eld[[8]];
EDataGetRps[eld : {___}] := eld[[9]];
EDataGetRsp[eld : {___}] := eld[[10]];
EDataGetMuellerMatrixR[eld : {___}] := eld[[11]];
EDataGetMuellerMatrixT[eld : {___}] := eld[[12]];

(* ============================================== *)

PsiPP[FullSol_] := Module[{eld, retVal}, eld = CalculateEllipsometricData[FullSol];
retVal = EDataGetPsiPP[eld];
Return[retVal];];

PsiPS[FullSol_] := Module[{eld, retVal}, eld = CalculateEllipsometricData[FullSol];
retVal = EDataGetPsiPS[eld];
Return[retVal];];

PsiSP[FullSol_] := Module[{eld, retVal},
  eld = CalculateEllipsometricData[FullSol];
  retVal = EDataGetPsiSP[eld];
  Return[retVal];
];

DeltaPP[FullSol_] := Module[{eld, retVal}, eld = CalculateEllipsometricData[FullSol];
retVal = EDataGetDeltaPP[eld];
Return[retVal];];

DeltaPS[FullSol_] := Module[{eld, retVal}, eld = CalculateEllipsometricData[FullSol];
retVal = EDataGetDeltaPS[eld];
Return[retVal];];

DeltaSP[FullSol_] := Module[{eld, retVal}, eld = CalculateEllipsometricData[FullSol];
retVal = EDataGetDeltaSP[eld];
Return[retVal];];

(* ============================================== *)
PsiPPDegree[FullSol_] := (PsiPP[FullSol] / Degree);
PsiPSDegree[FullSol_] := (PsiPS[FullSol] / Degree);
PsiSPDegree[FullSol_] := (PsiSP[FullSol] / Degree);
DeltaPPDegree[FullSol_] := (DeltaPP[FullSol] / Degree);
DeltaPSDegree[FullSol_] := (DeltaPS[FullSol] / Degree);
DeltaSPDegree[FullSol_] := (DeltaSP[FullSol] / Degree);
(* ============================================== *)
MuellerMatrixR[FullSol_] := Module[{eld, retVal},
(* Print["MuellerMatrixR::Starting..."]; *)
  eld = CalculateEllipsometricData[FullSol];
  (*Print["MuellerMatrixR::eld = ",eld];*)
  retVal = EDataGetMuellerMatrixR[eld];
  (*Print["MuellerMatrixR::retVal = ",retVal];*)
  Return[retVal];
];
(* ============================================== *)
MuellerMatrixT[FullSol_] := Module[{eld, retVal},
  eld = CalculateEllipsometricData[FullSol];
  (*Print["MuellerMatrixT::eld = ",eld];*)
  retVal = EDataGetMuellerMatrixT[eld];
  (*Print["MuellerMatrixT::retVal = ",retVal];*)
  Return[retVal];];
(* ============================================== *)
CalculateStokesVector[eField : {_, _, _}, hField : {_, _, _}, inveseZcomp_?BooleanQ] := Module[{stokesVector, kVec, eY, eXp, AxBx, AxBy, AyBx, AyBy, AVec, BVec, eXpC},
  kVec = kVector[eField, hField, inveseZcomp];
  eY = {0, 1, 0};
  eXp = eY \[Cross] kVec;
  eXpC = eY \[Cross]Conjugate[ kVec];

  AVec = eField;
  BVec = Conjugate[eField];

  AxBx = (eXp . AVec) * (eXpC . BVec);
  AxBy = (eXp . AVec) * (eY . BVec);
  AyBx = (eY . AVec) * (eXpC . BVec);
  AyBy = (eY . AVec) * (eY . BVec);

  stokesVector = Re[{AxBx + AyBy, AxBx - AyBy, AxBy + AyBx, I * (AxBy - AyBx)}];

  (*
Print["CalculateStokesVector::fita = ", fita, ", eField = ", eField // MatrixForm, ", stokesVector = ", Chop[stokesVector] // MatrixForm];
*)

  Return[stokesVector];
];
(* ============================================== *)
kVector[eField : {_, _, _}, hField : {_, _, _}, inveseZcomp_?BooleanQ] := Module[{ehVec, ehAmpl, ehArg, kVec},
  ehVec = Cross[hField, eField];
  ehAmpl = Sqrt[Abs[ehVec.Conjugate[ehVec]]];
  ehArg = Arg[ehVec[[3]]];
  kVec = If[ehAmpl > 0, ehVec * Exp[-I * ehArg] / ehAmpl, {0, 0, 0}];
  If[inveseZcomp, kVec *= (-1)];

  Return[kVec];
];
(* ============================================== *)
StokesVectorAllR[FullSol_] := Module[{stokesVector, eField, hField},
  eField = Flatten[GetSolReflectedLightE[FullSol]];
  hField = Flatten[GetSolReflectedLightH[FullSol]];
  stokesVector = CalculateStokesVector[eField, hField, True];
  Return[stokesVector];
];
(* ============================================== *)
StokesVectorAllT[FullSol_] := Module[{stokesVector, eField, hField},
  eField = Flatten[GetSolTransmittedLightE[FullSol]];
  hField = Flatten[GetSolTransmittedLightH[FullSol]];
  stokesVector = CalculateStokesVector[eField, hField, False];
  Return[stokesVector];
];
(* ============================================== *)
StokesVectorAllI[FullSol_] := Module[{stokesVector, eField, hField},
  eField = Flatten[GetSolIncidentLightE[FullSol]];
  hField = Flatten[GetSolReflectedLightH[FullSol]];
  stokesVector = CalculateStokesVector[eField, hField, False];
  Return[stokesVector];
];
(* ============================================== *)
(* ============================================== *)
StokesVectorR[idx_, FullSol_] := StokesVectorAllR[FullSol][[idx]];
(* ============================================== *)
StokesVectorT[idx_, FullSol_] := StokesVectorAllT[FullSol][[idx]];
(* ============================================== *)
StokesVectorI[idx_, FullSol_] := StokesVectorAllI[FullSol][[idx]];
(* ============================================== *)
(* Amount of polarized light in Stokes vector *)
StokesPolarized[stokesVector_] := Module[{II, pp, MM, CC, SS},
  {II, MM, CC, SS} = Re[stokesVector];

  If[II^2 > (MM^2 + CC^2 + SS^2),
    (
      pp = If[II > 0, Sqrt[(MM^2 + CC^2 + SS^2) / II^2], 1, Indeterminate];
    ),
    (
      pp = 1;
    ),
    (
      pp = Indeterminate;
    )
  ];

  Return[pp];
] /; (VectorQ[stokesVector] && Length[stokesVector] == 4);
(* ============================================== *)
StokesDepolarized[stokesVector_] := (1 - StokesPolarized[stokesVector]) /; (VectorQ[stokesVector] && Length[stokesVector] == 4);
(* ============================================== *)
(* Azimuth of elliptic polarization of Stokes vector *)
StokesGamma[stokesVector_] := Module[{Ip, pp, MM, CC, SS , gamma},
  MM = stokesVector[[2]];
  CC = stokesVector[[3]];
  SS = stokesVector[[4]];
  Ip = Sqrt[MM^2 + CC^2 + SS^2];
  gamma = ArcSin[SS / Ip] / 2;

  Return[gamma];
] /; (VectorQ[stokesVector] && Length[stokesVector] == 4);
(* ============================================== *)
StokesGammaDegree[stokesVector : _] := (StokesGamma[stokesVector] / Degree) /; (VectorQ[stokesVector] && Length[stokesVector] == 4);
(* ============================================== *)
(* stokesVector is expected to be for polarized light (no unpolarized component) *)
StokesChi[stokesVector_] := Module[{Ip, pp, MM, CC, SS , chi, sin2Gamma, cos2Gamma},
  MM = stokesVector[[2]];
  CC = stokesVector[[3]];
  SS = stokesVector[[4]];

  Ip = Sqrt[MM^2 + CC^2 + SS^2];
  sin2Gamma = If[Ip > 0, SS / Ip, 0, Indeterminate];
  cos2Gamma = Sqrt[1 - sin2Gamma^2];
  chi = If[cos2Gamma > 0, If[(CC^2 + SS^2) > 0, ArcTan[CC, SS], 0, Indeterminate] / 2, 0, Indeterminate];

  Return[chi];
] /; (VectorQ[stokesVector] && Length[stokesVector] == 4);
(* ============================================== *)
StokesChiDegree[stokesVector_] := (StokesChi[stokesVector] / Degree) /; (VectorQ[stokesVector] && Length[stokesVector] == 4);
(* ============================================== *)
StokesGammaR[stokesVector_] := Module[{retVal},
  retVal = StokesGamma[stokesVector];
  (*
Print["StokesGammaR::stokesVector = ", stokesVector];
Print["StokesGammaR::retVal = ", retVal];
*)
  Return[retVal];
] /; (VectorQ[stokesVector] && Length[stokesVector] == 4);
(* ============================================== *)
StokesGammaDegreeR[stokesVector_] := Module[{retVal},
  retVal = StokesGammaDegree[stokesVector];
  (*
Print["StokesGammaDegreeR::stokesVector = ", stokesVector];
Print["StokesGammaDegreeR::retVal = ", retVal];
*)
  Return[retVal];
] /; (VectorQ[stokesVector] && Length[stokesVector] == 4);
(* ============================================== *)
StokesGammaR[FullSol_] := Module[{retVal, stokesVector},
  stokesVector = StokesVectorAllR[FullSol];
  retVal = StokesGamma[stokesVector];
  (*
Print["StokesGammaR[FullSol]::stokesVector = ", stokesVector];
Print["StokesGammaR[FullSol]::retVal = ", retVal];
*)
  Return[retVal];
];
(* ============================================== *)
StokesGammaDegreeR[FullSol_] := Module[{retVal, stokesVector},
  stokesVector = StokesVectorAllR[FullSol];
  retVal = StokesGammaDegree[stokesVector];
  (*
Print["StokesGammaDegreeR[FullSol]::stokesVector = ", stokesVector];
Print["StokesGammaDegree[RFullSol]::retVal = ", retVal];
*)
  Return[retVal];
];
(* ============================================== *)
StokesChiR[stokesVector_] := Module[{retVal},
  retVal = StokesChi[stokesVector];
  (*
Print["StokesChiR::stokesVector = ", stokesVector];
Print["StokesChiR::retVal = ", retVal];
*)
  Return[retVal];
] /; (VectorQ[stokesVector] && Length[stokesVector] == 4);
(* ============================================== *)
StokesChiDegreeR[stokesVector_] := Module[{retVal},
  retVal = StokesChiDegree[stokesVector];
  (*
Print["StokesChiDegreeR::stokesVector = ", stokesVector];
Print["StokesChiDegreeR::retVal = ", retVal];
*)
  Return[retVal];
] /; (VectorQ[stokesVector] && Length[stokesVector] == 4);
(* ============================================== *)
StokesChiR[FullSol_] := Module[{retVal, stokesVector},
  stokesVector = StokesVectorAllR[FullSol];
  retVal = StokesChi[stokesVector];
  (*
Print["StokesChiR[FullSol]::stokesVector = ", stokesVector];
Print["StokesChiR[FullSol]::retVal = ", retVal];
*)
  Return[retVal];
];
(* ============================================== *)
StokesChiDegreeR[FullSol_] := Module[{retVal, stokesVector},
  stokesVector = StokesVectorAllR[FullSol];
  retVal = StokesChiDegree[stokesVector];
  (*
Print["StokesChiDegreeR[FullSol]::stokesVector = ", stokesVector];
Print["StokesChiDegreeR[RFullSol]::retVal = ", retVal];
*)
  Return[retVal];
];
(* ============================================== *)
StokesPolarizedR[stokesVector_] := Module[{retVal},
  retVal = StokesPolarized[stokesVector];
  (*
Print["StokesPolarizedR::stokesVector = ", stokesVector];
Print["StokesPolarizedR::retVal = ", retVal];
*)
  Return[retVal];
] /; (VectorQ[stokesVector] && Length[stokesVector] == 4);
(* ============================================== *)
StokesPolarizedR[FullSol_] := Module[{retVal, stokesVector},
  stokesVector = StokesVectorAllR[FullSol];
  retVal = StokesPolarized[stokesVector];
  (*
Print["StokesPolarizedR[FullSol]::stokesVector = ", stokesVector];
Print["StokesPolarizedR[FullSol]::retVal = ", retVal];
*)
  Return[retVal];
];
(* ============================================== *)

