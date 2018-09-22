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

        member this.plotPoints = [| for i in 0..this.length -> this.plotValue i |]



    let getWaveLength (v : RangedVariable) i = 
        match v with
        | WaveLengthRange _ -> v.value i |> WaveLength |> Some
        | _ -> None


    let getIncidenceAngle (v : RangedVariable) i = 
        match v with 
        | IncidenceAngleRange _ -> v.value i |> Angle |> IncidenceAngle |> Some
        | _ -> None


    let getPolarization (v : RangedVariable) i = 
        match v with 
        | PolarizationRange _ -> v.value i |> Angle |> Polarization |> Some
        | _ -> None


    let getEllipticity (v : RangedVariable) i = 
        match v with 
        | EllipticityRange _ -> v.value i |> Ellipticity |> Some
        | _ -> None


    type FixedInfo =
         {
            incidentLightInfo : IncidentLightInfo
            opticalSystem : OpticalSystem
         }


    let calculate (f: FixedInfo) (x : RangedVariable) = 
        let l = f.incidentLightInfo

        let getValue d g i = 
            match g x i with 
            | Some v -> v
            | None -> d

        let getLight i = 
            {
                wavelength = getValue l.wavelength getWaveLength i
                refractionIndex = l.refractionIndex
                incidenceAngle = getValue l.incidenceAngle getIncidenceAngle i
                polarization = getValue l.polarization getPolarization i
                ellipticity = getValue l.ellipticity getEllipticity i
            }

        // TODO kk:20180917 - Implement.
        let getOpticalSystem i = 
            f.opticalSystem

        let getEmSys i = OpticalSystemSolver(getOpticalSystem i, getLight i).emSys
        [| for i in 0..x.length -> (x.plotValue i, getEmSys i) |]


    let calculate3D (f: FixedInfo) (x : RangedVariable) (y : RangedVariable) =
        let l = f.incidentLightInfo

        let getValue d g i j = 
            match g x i with 
            | Some v -> v
            | None ->
                match g y j with 
                | Some w -> w
                | None -> d

        let getLight i j = 
            {
                wavelength = getValue l.wavelength getWaveLength i j
                refractionIndex = l.refractionIndex
                incidenceAngle = 
                    let r = getValue l.incidenceAngle getIncidenceAngle i j
                    //printfn "getLight::i = %A, j = %A, r = %A" i j r
                    r
                polarization = getValue l.polarization getPolarization i j
                ellipticity = getValue l.ellipticity getEllipticity i j
            }

        // TODO kk:20180922 - Implement.
        let getOpticalSystem i j = 
            f.opticalSystem

        let getEmSys i j = OpticalSystemSolver(getOpticalSystem i j, getLight i j).emSys

        [| for i in 0..x.length -> i |]
        |> Seq.map (fun i -> [| for j in 0..y.length -> 
                                (
                                    x.plotValue i, y.plotValue j, 
                                    let r = getEmSys i j

                                    //printfn "x.plotValue i = %A, y.plotValue j = %A, emSys = %A\n====================\n" (x.plotValue i) (y.plotValue j) r
                                    r
                                ) |])
        |> Array.ofSeq

