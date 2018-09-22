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

    let plot (f : FixedInfo) (x : RangedVariable) (fn : List<OpticalFunction>) =
        let data = calculate f x
        let getFuncData (e : OpticalFunction) = data |> Array.map (fun (v, s) -> (v, s.func e))

         //FSharp.Plotly
        Chart.Combine (fn |> List.map (fun e -> Chart.Line(getFuncData e, Name = e.info.fullName)))
        |> Chart.withX_AxisStyle(x.name, MinMax = (x.plotMinValue, x.plotMaxValue))
        |> Chart.Show


    let mapFun (data : #seq<#seq<double * double * EmFieldSystem>>) (fn : OpticalFunction) = 
        data
        |> Seq.map (fun r -> r |> Seq.map (fun (_, _, e) -> e.func fn) |> Array.ofSeq)
        |> Array.ofSeq


    let plot3D (f : FixedInfo) (x : RangedVariable) (y : RangedVariable) (fn : List<OpticalFunction>) =
        let xVal = x.plotPoints
        let yVal = y.plotPoints
        let data = calculate3D f x y

        let plotFun e = 
            let zVal = mapFun data e

            // kk:20180922 The axes are somehow mysteriouly reversed. Here we swap X with Y for both the data and the names.
            Chart.Surface(zVal, yVal, xVal, Opacity = 0.7, Contours = Contours.initXyz(Show = true), Name = e.info.name)
            |> Chart.withX_AxisStyle(y.name)
            |> Chart.withY_AxisStyle(x.name)
            |> Chart.withZ_AxisStyle(e.info.name)
            |> Chart.Show

        fn |> List.map (fun e -> plotFun e)
