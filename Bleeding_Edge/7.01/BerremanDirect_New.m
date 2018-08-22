(* ============================================== *)
(* :Summary: This module extends basic Berreman Matrix transformations. *)
(* ============================================== *)
(* :Author: Konstantin K. Konstantinov *)
(* :Email: konstantin.k.konstantinov@gmail.com *)
(* :License type: GPL v3 or any later version, see http://www.gnu.org/licenses/ *)
(* :Copyright: K^3, 2001 - 2018 *)
(* :Version: Revision: 7.01.001, Date: 2018/07/02 *)
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
BDAVGTYPESUMOFTWOPOINTS = 0;
BDAVGTYPEUSEAVGPERIODS = 1;
BDAVGTYPEUSEAVGPERIODSEXACTLAMBDA = 2;
BDAVGTYPESERIES = 100;
BDAVGTYPESTD = BDAVGTYPESERIES;

BDIMAGESIZE = 640;

BDPLTTEXTOPTS =
    {
      FontFamily -> "Courier",
      FontSize -> 18,
      FontWeight -> "Bold"
    };
(* ============================================== *)
Options[BerremanDirect] =
    {
      BerremanDirectVersion -> 7.01,
      RotateAll -> True,
      ConsecutiveRotation -> True,
      PrintTimeEstimate -> False,
      PrintCalculationProgress -> False,
      PrintCalculationDetails -> False,
      UseThickLastLayer -> False,
      NoOfAveragingPoints -> 5,
      AveragingPeriods -> 1,
      AveragingType -> BDAVGTYPESERIES,
      Amplitude -> 1,
      TransmittedAnalyzerAngle -> 0,
      TransmittedAnalyzerParallelAmplitude -> 1,
      TransmittedAnalyzerCrossedAmplitude -> 0,
      ReflectedAnalyzerAngle -> 0,
      ReflectedAnalyzerParallelAmplitude -> 1,
      ReflectedAnalyzerCrossedAmplitude -> 0,
      AnalyzerAngleAbsoluteValue -> False,
      UseAnalyzer -> False,
      UseMakeFileName -> False,
      EigenValueIndex -> 1,
      UsePlotLabel3D -> True,
      UseChop -> True,
      ChopTolerance -> 10^-7,
      ChopPrecision -> 6,
      PrintFunctionDescription -> True,
      IgnoreZeroIFullBase -> True,
      ZeroIFullBaseReplaceValue -> 1,
      Plot3DType -> Plot3D,
      BDPerformCalculations -> True,
      BDPlotFigures -> True,
      PlotOptions3D -> {PlotPoints -> 25, PlotRange -> All},
      PlotOptions2D -> {PlotPoints -> 25, PlotRange -> All, MaxRecursion -> 3}
    };

(*AveragingType-Wavelength averaging:0-sum of two points/2,1-averaging by NoOfAveragingPoints& using AveragingPeriods,2-same as 1,but lambda is exactly as supplied*)
(* ============================================== *)