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

module Variables = 

    type RangedVariable<'T> = 
        {
            startValue : 'T
            endValue : 'T
            numberOfPoints : int
        }


    type Variable<'T> = 
        | Fixed of 'T
        | Range of RangedVariable<'T>

        static member createFixed t = Fixed t
        static member createRange s e = Range { startValue = s; endValue = e; numberOfPoints = 100 }


    //type AngleVariable = 
    //    {
    //        startAngle : Angle
    //        endAngle : Angle
    //        numberOfPoints : int
    //    }

    //    static member create s e = { startAngle = s; endAngle = e; numberOfPoints = 100 }
    //    static member defaultValue = AngleVariable.create (Angle.degree 0.) (Angle.degree 90.)
    //    static member defaultValueZeroToEightyNine = AngleVariable.create (Angle.degree 0.) (Angle.degree 89.)


    //type EllipticityVariable = 
    //    {
    //        startEllipticity : Ellipticity
    //        endEllipticity : Ellipticity
    //        numberOfPoints : int
    //    }

    //    static member create s e = { startEllipticity = s; endEllipticity = e; numberOfPoints = 100 }
    //    static member defaultValue = EllipticityVariable.create (Ellipticity.create 0.0) (Ellipticity.create 1.0)


    type Variable = 
        | IncidenceAngleVariable of Variable<IncidenceAngle>
        | PolarizationVariable of Variable<Polarization>
        | EllipticityVariable of Variable<Ellipticity>
        | WaveLengthVariable of Variable<WaveLength>


    type Variable1D = 
        {
            x : Variable
        }


    type Variable2D = 
        {
            x : Variable
            y : Variable
        }

    //type Variables = 
    //    | Variable1D of Variable1D
    //    | Variable2D of Variable2D


    let getWaveLength (l : IncidentLightInfo) (v : Variable) i = 
        match v with 
        | WaveLengthVariable (Fixed f) -> f
        | WaveLengthVariable (Range r) -> 
            let (WaveLength s) = r.startValue
            let (WaveLength e) = r.endValue
            s + (e - s) * (double i) |> WaveLength
        | _ -> l.wavelength


    let getIncidenceAngle (l : IncidentLightInfo) (v : Variable) i = 
        match v with 
        | IncidenceAngleVariable (Fixed f) -> f
        | IncidenceAngleVariable (Range r) -> 
            let (IncidenceAngle (Angle s)) = r.startValue
            let (IncidenceAngle (Angle e)) = r.endValue
            s + (e - s) * (double i) |> Angle |> IncidenceAngle
        | _ -> l.incidenceAngle


    let getPolarization (l : IncidentLightInfo) (v : Variable) i = 
        match v with 
        | PolarizationVariable (Fixed f) -> f
        | PolarizationVariable (Range r) -> 
            let (Polarization (Angle s)) = r.startValue
            let (Polarization (Angle e)) = r.endValue
            s + (e - s) * (double i) |> Angle |> Polarization
        | _ -> l.polarization


    let getEllipticity (l : IncidentLightInfo) (v : Variable) i = 
        match v with 
        |EllipticityVariable (Fixed f) -> f
        | EllipticityVariable (Range r) -> 
            let (Ellipticity s) = r.startValue
            let (Ellipticity e) = r.endValue
            s + (e - s) * (double i) |> Ellipticity
        | _ -> l.ellipticity


    let calculate (l : IncidentLightInfo) (o : OpticalSystem) (v : Variable1D) = 

        let getLight i = 
            {
                wavelength = getWaveLength l v.x i
                refractionIndex = l.refractionIndex
                incidenceAngle = getIncidenceAngle l v.x i
                polarization = getPolarization l v.x i
                ellipticity = getEllipticity l v.x i
            }

        let getEmSys i = 
            let sol = OpticalSystemSolver(o, getLight i)
            sol.emSys

        0

