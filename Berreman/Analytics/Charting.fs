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


    let mapFun (fn : OpticalFunction) (data : #seq<#seq<double * double * EmFieldSystem>>) = 
        data
        |> Seq.map (fun r -> r |> Seq.map (fun (_, _, e) -> e.func fn) |> Array.ofSeq)
        |> Array.ofSeq


    let plot3D (fn : OpticalFunction) (f : FixedInfo) (x : RangedVariable) (y : RangedVariable) =
        let xRange = [ x.plotMinValue; x.plotMaxValue ]
        let yRange = [ y.plotMinValue; y.plotMaxValue ]
        let data = calculate3D f x y
        let zVal = mapFun fn data

        //Chart.Surface(zVal, xRange, yRange)
        //|> Chart.Show

        Chart.Surface(zVal)
        |> Chart.withX_AxisStyle(x.name, MinMax = (x.plotMinValue, x.plotMaxValue))
        |> Chart.withY_AxisStyle(y.name, MinMax = (y.plotMinValue, y.plotMaxValue))
        |> Chart.withZ_AxisStyle(fn.info.name)
        |> Chart.Show


    //let plotAbc3D (f : OpticalFunction) (g : Ellipticity -> Angle -> EmFieldSystem) =
    //    let subData (Ellipticity e) = [| for i in 0..89 -> (float i |> Angle.degree |> g (Ellipticity e)).func f |]
    //    let data = 
    //        [| for i in 0..100 -> i |]
    //        |> PSeq.map (fun i -> (((float i) / 100.0) |> Ellipticity.create|> subData))
    //        |> Array.ofSeq

    //    let getFuncData (e : OpticalFunction) = data |> Array.map (fun x -> x |> Array.map (fun y -> y))

    //    Chart.Surface(data)
    //    |> Chart.withX_AxisStyle("$\Phi$", MinMax = (0.0, 90.0))
    //    |> Chart.withY_AxisStyle("e", MinMax = (0.0, 1.0))
    //    |> Chart.withZ_AxisStyle(f.info.fullName)
    //    |> Chart.Show
