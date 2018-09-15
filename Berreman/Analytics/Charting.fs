﻿namespace Analytics

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

        let getSol a = BaseOpticalSystemSolver(system, getLight a)


        failwith ""

    let plot() = 

        failwith ""
