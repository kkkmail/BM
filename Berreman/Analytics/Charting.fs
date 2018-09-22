namespace Analytics

open FSharp.Collections.ParallelSeq
open System.Numerics
open MathNet.Numerics.LinearAlgebra
open Berreman.MathNetNumericsMath
open Berreman.MatrixExp

open Berreman.Constants
open Berreman.Fields
open Berreman.BerremanMatrix
open Berreman.Geometry
open Berreman.MaterialProperties
open Berreman.Media
open Berreman.Solvers
open Berreman.FieldFunctions
open OpticalProperties.Standard
open Berreman
open OpticalProperties
open Analytics.StandardSystems

open Standard

open Analytics.Variables

//open FSharp.Charting
open FSharp.Plotly
//open XPlot.Plotly

module Charting =
    let plot (fn : List<OpticalFunction>) (f : FixedInfo) (x : RangedVariable) =
        let data = calculate f x
        let getFuncData (e : OpticalFunction) = data |> Array.map (fun (v, s) -> (v, s.func e))

         //FSharp.Plotly
        Chart.Combine (fn |> List.map (fun e -> Chart.Line(getFuncData e, Name = e.info.fullName)))
        |> Chart.withX_AxisStyle(x.name, MinMax = (x.plotMinValue, x.plotMaxValue))
        |> Chart.Show

    //let plot1() = plot fn f1 incidenceAngleRange
    //let plot2() = plot fn f2 ellipticityRange

    let fn = [R; T]
