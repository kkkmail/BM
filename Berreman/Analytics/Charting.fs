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

open FSharp.Charting

module Charting = 

    let gePlotData() : (float * float)[] =
        [| for i in 1..100 -> (float i, float (i * i)) |]


    let plotData () = 
        Chart.Line(gePlotData(), Name = "Test")
        //|> Chart.WithXAxis(Enabled = true, Title = (sprintf "%A" h.matchFunction), Max = maxVal, Min = h.interval.minValue)
        //|> Chart.WithYAxis(Enabled = true, Title = "Probability of failing match", Max = 1.0, Min = 0.0)
        //|> Chart.WithLegend(Docking = ChartTypes.Docking.Left, InsideArea = true, Title = h.description)

        |> Chart.WithXAxis(Enabled = true, Title = "X")
        |> Chart.WithYAxis(Enabled = true, Title = "Y")
        |> Chart.WithLegend(Docking = ChartTypes.Docking.Left, InsideArea = true, Title = "Legend")
        |> Chart.Show


    let gePlotData1() : (float * float)[] =
        let system = 
            {
                upper = OpticalProperties.vacuum
                films = []
                lower = OpticalProperties.transparentGlass
            }

        let getLight a = 
            {
                wavelength = WaveLength.nm 600.0
                refractionIndex = RefractionIndex.vacuum
                incidenceAngle = IncidenceAngle.create a
                polarization = Polarization.defaultValue
                ellipticity = Ellipticity.defaultValue
            }

        let getSol a = 
            //printfn "a = %A" a
            let sol = BaseOpticalSystemSolver(system, getLight a)
            //printfn "sol = %A" sol
            sol.emSys.rp


        [| for i in 0..89 -> (float i, (float i |> Angle.degree |> getSol)) |]

    let plot() = 
        Chart.Line(gePlotData1())
        |> Chart.WithXAxis(Enabled = true, Title = "incidence angle", Min = 0.0, Max = 90.0)
        |> Chart.Show
