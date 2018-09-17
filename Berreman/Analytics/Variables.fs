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


    type FixedVariable = 
        | IncidenceAngleFixed of IncidenceAngle
        | PolarizationFixed of Polarization
        | EllipticityFixed of Ellipticity
        | WaveLengthFixed of WaveLength


    type RangedVariable = 
        | IncidenceAngleRange of Range<IncidenceAngle>
        | PolarizationRange of Range<Polarization>
        | EllipticityRange of Range<Ellipticity>
        | WaveLengthRange of Range<WaveLength>


    type Variable = 
        | Fixed of FixedVariable
        | Ranged of RangedVariable


    //type Variable1D = 
    //    {
    //        x : RangedVariable
    //    }


    //type Variable2D = 
    //    {
    //        x : RangedVariable
    //        y : RangedVariable
    //    }


    let getWaveLength (l : IncidentLightInfo) (v : Variable) i = 
        match v with 
        | Fixed (WaveLengthFixed f) -> f
        | Ranged (WaveLengthRange r) -> 
            let (WaveLength s) = r.startValue
            let (WaveLength e) = r.endValue
            s + (e - s) * (double i) / (double r.numberOfPoints) |> WaveLength
        | _ -> l.wavelength


    let getIncidenceAngle (l : IncidentLightInfo) (v : Variable) i = 
        match v with 
        | Fixed (IncidenceAngleFixed f) -> f
        | Ranged (IncidenceAngleRange r) -> 
            let (IncidenceAngle (Angle s)) = r.startValue
            let (IncidenceAngle (Angle e)) = r.endValue
            s + (e - s) * (double i) / (double r.numberOfPoints) |> Angle |> IncidenceAngle
        | _ -> l.incidenceAngle


    let getPolarization (l : IncidentLightInfo) (v : Variable) i = 
        match v with 
        | Fixed (PolarizationFixed f) -> f
        | Ranged (PolarizationRange r) -> 
            let (Polarization (Angle s)) = r.startValue
            let (Polarization (Angle e)) = r.endValue
            s + (e - s) * (double i) / (double r.numberOfPoints) |> Angle |> Polarization
        | _ -> l.polarization


    let getEllipticity (l : IncidentLightInfo) (v : Variable) i = 
        match v with 
        | Fixed (EllipticityFixed f) -> f
        | Ranged (EllipticityRange r) -> 
            let (Ellipticity s) = r.startValue
            let (Ellipticity e) = r.endValue
            s + (e - s) * (double i) / (double r.numberOfPoints) |> Ellipticity
        | _ -> l.ellipticity


    type FixedInfo =
         {
            incidentLightInfo : IncidentLightInfo
            opticalSystem : OpticalSystem
            fixedParameters : List<FixedVariable>
         }


    let calculate (f: FixedInfo) (x : RangedVariable) = 

        let getLight i = 
            {
                wavelength = getWaveLength f.incidentLightInfo (Ranged x) i
                refractionIndex = f.incidentLightInfo.refractionIndex
                incidenceAngle = getIncidenceAngle f.incidentLightInfo (Ranged x) i
                polarization = getPolarization f.incidentLightInfo (Ranged x) i
                ellipticity = getEllipticity f.incidentLightInfo (Ranged x) i
            }

        let getEmSys i = 
            let sol = OpticalSystemSolver(f.opticalSystem, getLight i)
            sol.emSys

        0

