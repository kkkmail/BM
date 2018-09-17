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

    type Range<'T> = 
        {
            startValue : 'T
            endValue : 'T
            numberOfPoints : int
        }

        static member create s e = { startValue = s; endValue = e; numberOfPoints = 100 }


    type RangedVariable = 
        | IncidenceAngleRange of Range<IncidenceAngle>
        | PolarizationRange of Range<Polarization>
        | EllipticityRange of Range<Ellipticity>
        | WaveLengthRange of Range<WaveLength>

        member this.length = 
            match this with 
            | IncidenceAngleRange v -> v.numberOfPoints
            | PolarizationRange v -> v.numberOfPoints
            | EllipticityRange v -> v.numberOfPoints
            | WaveLengthRange v -> v.numberOfPoints

        member this.name = 
            match this with 
            | IncidenceAngleRange _ -> "f"
            | PolarizationRange _ -> "p"
            | EllipticityRange _ -> "e"
            | WaveLengthRange _ -> "w (nm)"

        member this.value i = 
            match this with 
            | IncidenceAngleRange r -> 
                let (IncidenceAngle (Angle s)) = r.startValue
                let (IncidenceAngle (Angle e)) = r.endValue
                s + (e - s) * (double i) / (double r.numberOfPoints)
            | PolarizationRange r -> 
                let (Polarization (Angle s)) = r.startValue
                let (Polarization (Angle e)) = r.endValue
                s + (e - s) * (double i) / (double r.numberOfPoints)
            | EllipticityRange r -> 
                let (Ellipticity s) = r.startValue
                let (Ellipticity e) = r.endValue
                s + (e - s) * (double i) / (double r.numberOfPoints)
            | WaveLengthRange r -> 
                let (WaveLength s) = r.startValue
                let (WaveLength e) = r.endValue
                s + (e - s) * (double i) / (double r.numberOfPoints)

        member this.plotValue i = 
            match this with 
            | IncidenceAngleRange _ -> (this.value i) |> toDegree
            | PolarizationRange _ -> (this.value i) |> toDegree
            | EllipticityRange _ -> this.value i
            | WaveLengthRange _ -> (this.value i) |> toNanometers

        member this.plotMinValue = 
            match this with 
            | IncidenceAngleRange r -> 
                let (IncidenceAngle (Angle s)) = r.startValue
                s |> toDegree
            | PolarizationRange r -> 
                let (Polarization (Angle s)) = r.startValue
                s |> toDegree
            | EllipticityRange r -> 
                let (Ellipticity s) = r.startValue
                s
            | WaveLengthRange r -> 
                let (WaveLength s) = r.startValue
                s |> toNanometers

        member this.plotMaxValue = 
            match this with 
            | IncidenceAngleRange r -> 
                let (IncidenceAngle (Angle e)) = r.endValue
                e |> toDegree
            | PolarizationRange r -> 
                let (Polarization (Angle e)) = r.endValue
                e |> toDegree
            | EllipticityRange r -> 
                let (Ellipticity e) = r.endValue
                e
            | WaveLengthRange r -> 
                let (WaveLength e) = r.endValue
                e |> toNanometers


    let getWaveLength (l : IncidentLightInfo) (v : RangedVariable) i = 
        match v with
        | WaveLengthRange _ -> v.value i |> WaveLength
        | _ -> l.wavelength


    let getIncidenceAngle (l : IncidentLightInfo) (v : RangedVariable) i = 
        match v with 
        | IncidenceAngleRange _ -> v.value i |> Angle |> IncidenceAngle
        | _ -> l.incidenceAngle


    let getPolarization (l : IncidentLightInfo) (v : RangedVariable) i = 
        match v with 
        | PolarizationRange _ -> v.value i |> Angle |> Polarization
        | _ -> l.polarization


    let getEllipticity (l : IncidentLightInfo) (v : RangedVariable) i = 
        match v with 
        | EllipticityRange _ -> v.value i |> Ellipticity
        | _ -> l.ellipticity


    type FixedInfo =
         {
            incidentLightInfo : IncidentLightInfo
            opticalSystem : OpticalSystem
         }


    let calculate (f: FixedInfo) (x : RangedVariable) = 

        let getLight i = 
            {
                wavelength = getWaveLength f.incidentLightInfo x i
                refractionIndex = f.incidentLightInfo.refractionIndex
                incidenceAngle = getIncidenceAngle f.incidentLightInfo x i
                polarization = getPolarization f.incidentLightInfo x i
                ellipticity = getEllipticity f.incidentLightInfo x i
            }

        // TODO kk:20180917 - Implement.
        let getOpticalSystem i = 
            f.opticalSystem

        let getEmSys i = 
            let sol = OpticalSystemSolver(getOpticalSystem i , getLight i)
            sol.emSys

        [ for i in 0..x.length -> (x.plotValue i, getEmSys i) ]
