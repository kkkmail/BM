(* ============================================== *)
(* :Summary: This module defines optical model functions logic. *)
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
Options[OpticalModelFunctions] =
    {
      OpticalModelFunctionsVersion -> 6.04
    };
(* ============================================== *)
(* Stokes vector for horizontal linear polarization *)
StokesVectorInitLHP = {{1}, {1}, {0}, {0}};

(* Stokes vector for vertical linear polarization *)
StokesVectorInitLVP = {{1}, {-1}, {0}, {0}};

(* Stokes vector for linear polarization with +45 degrees rotation *)
StokesVectorInitLp45P = {{1}, {0}, {1}, {0}};

(* Stokes vector for linear polarization with -45 degrees rotation *)
StokesVectorInitLm45P = {{1}, {0}, {-1}, {0}};

(* Stokes vector for right circular polarization *)
StokesVectorInitRCP = {{1}, {0}, {0}, {1}};

(* Stokes vector for left circular polarization *)
StokesVectorInitLCP = {{1}, {0}, {0}, {-1}};

(* Stokes vector for unpolarized light *)
StokesVectorInitUnpolarized = {{1}, {0}, {0}, {0}};
(* ============================================== *)
Options[StokesVectorS0] = {FunctionName -> Subscript["S", "0"], FunctionDescription -> "Stokes vectror: 0 component."};
StokesVectorS0[mullerMatrix_?MatrixQ] :=
    Module[{stVecOut},
      stVecOut = mullerMatrix . StokesVectorInitLHP;
      Return[stVecOut[[1, 1]]];
    ];
(* ============================================== *)
Options[StokesVectorS1] = {FunctionName -> Subscript["S", "1"], FunctionDescription -> "Stokes vectror: 1 component."};
StokesVectorS1[mullerMatrix_?MatrixQ] :=
    Module[{stVecOut},
      stVecOut = mullerMatrix . StokesVectorInitLHP;
      Return[stVecOut[[2, 1]]];
    ];
(* ============================================== *)
Options[StokesVectorS2] = {FunctionName -> Subscript["S", "2"], FunctionDescription -> "Stokes vectror: 2 component."};
StokesVectorS2[mullerMatrix_?MatrixQ] :=
    Module[{stVecOut},
      stVecOut = mullerMatrix . StokesVectorInitLHP;
      Return[stVecOut[[3, 1]]];
    ];
(* ============================================== *)
Options[StokesVectorS3] = {FunctionName -> Subscript["S", "3"], FunctionDescription -> "Stokes vectror: 3 component."};
StokesVectorS3[mullerMatrix_?MatrixQ] :=
    Module[{stVecOut},
      stVecOut = mullerMatrix . StokesVectorInitLHP;
      Return[stVecOut[[4, 1]]];
    ];
(* ============================================== *)