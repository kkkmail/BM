namespace Analytics

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

//open FSharp.Charting
open FSharp.Plotly

module Charting = 

    //let gePlotData() : (float * float)[] =
    //    [| for i in 1..100 -> (float i, float (i * i)) |]


    //let plotData () = 
    //    Chart.Line(gePlotData(), Name = "Test")
    //    //|> Chart.WithXAxis(Enabled = true, Title = (sprintf "%A" h.matchFunction), Max = maxVal, Min = h.interval.minValue)
    //    //|> Chart.WithYAxis(Enabled = true, Title = "Probability of failing match", Max = 1.0, Min = 0.0)
    //    //|> Chart.WithLegend(Docking = ChartTypes.Docking.Left, InsideArea = true, Title = h.description)

    //    |> Chart.WithXAxis(Enabled = true, Title = "X")
    //    |> Chart.WithYAxis(Enabled = true, Title = "Y")
    //    |> Chart.WithLegend(Docking = ChartTypes.Docking.Left, InsideArea = true, Title = "Legend")
    //    |> Chart.Show


    let system = 
        {
            upper = OpticalProperties.vacuum
            films = []
            lower = OpticalProperties.transparentGlass
        }


    let getLight e a = 
        {
            wavelength = WaveLength.nm 600.0
            refractionIndex = RefractionIndex.vacuum
            incidenceAngle = IncidenceAngle.create a
            polarization = Polarization.defaultValue
            ellipticity = e
        }


    let getSol a = 
        //printfn "a = %A" a
        let sol = BaseOpticalSystemSolver(system, getLight Ellipticity.defaultValue a)
        //printfn "sol = %A" sol
        sol.emSys.rp


    let getEmSys e a = 
        let sol = BaseOpticalSystemSolver(system, getLight e a)
        sol.emSys


    let gePlotData1() : (float * float)[] =
        [| for i in 0..89 -> (float i, (float i |> Angle.degree |> getSol)) |]


    let plotAbc (f : List<OpticalFunction>) (g : Angle -> EmFieldSystem) =
        let data = [| for i in 0..89 -> (float i, (float i |> Angle.degree |> g)) |]

        let getFuncData (e : OpticalFunction) = data |> Array.map (fun (x, s) -> (x, s.func e))

        //Chart.Combine (f |> List.map (fun e -> Chart.Line(getFuncData e, Name = e.info.fullName)))
        //|> Chart.WithXAxis(Enabled = true, Title = "incidence angle", Min = 0.0, Max = 90.0)
        //|> Chart.WithLegend(InsideArea = true) // Docking = ChartTypes.Docking.Left, Title = "Title"
        //|> Chart.Show

        Chart.Combine (f |> List.map (fun e -> Chart.Line(getFuncData e, Name = e.info.fullName)))
        |> Chart.withX_AxisStyle("incidence angle", MinMax = (0.0, 90.0))
        //|> Chart.WithXAxis(Enabled = true, Title = "incidence angle", Min = 0.0, Max = 90.0)
        //|> Chart.WithLegend(InsideArea = true) // Docking = ChartTypes.Docking.Left, Title = "Title"
        |> Chart.Show


    let plotAbc3D (f : OpticalFunction) (g : Ellipticity -> Angle -> EmFieldSystem) =
        //let subData (Ellipticity e) = [| for i in 0..89 -> (e, float i, (float i |> Angle.degree |> g (Ellipticity e))) |]
        //let data = [| for i in 0..100 -> (((float i) / 100.0) |> Ellipticity.create|> subData) |]

        let subData (Ellipticity e) = [| for i in 0..89 -> (float i |> Angle.degree |> g (Ellipticity e)).func f |]
        let data = [| for i in 0..100 -> (((float i) / 100.0) |> Ellipticity.create|> subData) |]

        let getFuncData (e : OpticalFunction) = data |> Array.map (fun x -> x |> Array.map (fun y -> y))

        Chart.Surface(data)
        |> Chart.Show


    let plot() = plotAbc [ Rp; Tp ] (getEmSys Ellipticity.defaultValue)
        //Chart.Line(gePlotData1())
        //|> Chart.WithXAxis(Enabled = true, Title = "incidence angle", Min = 0.0, Max = 90.0)
        //|> Chart.Show

    let plot3D() = plotAbc3D Rp getEmSys
