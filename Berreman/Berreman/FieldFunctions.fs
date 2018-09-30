﻿namespace Berreman

module FieldFunctions =

    open System.Numerics
    open MathNetNumericsMath
    open MatrixExp

    open Constants
    open Geometry
    open Fields
    open Media
    open BerremanMatrix
    open System.ComponentModel
    open System.Net.Configuration
    open Solvers
    open Geometry


    let muellerMatrix (rSS : Complex) (rPP : Complex) (rPS : Complex) (rSP : Complex) =
        (
            [
                [
                    (rPP * rPP.conjugate + rPS * rPS.conjugate + rSP * rSP.conjugate + rSS * rSS.conjugate) / (cplx 2.0)
                    (rPP * rPP.conjugate + rPS * rPS.conjugate - rSP * rSP.conjugate - rSS * rSS.conjugate) / (cplx 2.0)
                    (rSP * rPP.conjugate + rSS * rPS.conjugate + rPP * rSP.conjugate + rPS * rSS.conjugate) / (cplx 2.0)
                    (complexI / (cplx 2.0)) * (rSP * rPP.conjugate + rSS * rPS.conjugate - rPP * rSP.conjugate - rPS * rSS.conjugate)
                ]
                [
                    (rPP * rPP.conjugate - rPS * rPS.conjugate + rSP * rSP.conjugate - rSS * rSS.conjugate) / (cplx 2.0)
                    (rPP * rPP.conjugate - rPS * rPS.conjugate - rSP * rSP.conjugate + rSS * rSS.conjugate) / (cplx 2.0)
                    (rSP * rPP.conjugate - rSS * rPS.conjugate + rPP * rSP.conjugate - rPS * rSS.conjugate) / (cplx 2.0)
                    (complexI / (cplx 2.0)) * (rSP * rPP.conjugate - rSS * rPS.conjugate - rPP * rSP.conjugate + rPS * rSS.conjugate)
                ]
                [
                    (rPS * rPP.conjugate + rPP * rPS.conjugate + rSS * rSP.conjugate + rSP * rSS.conjugate) / (cplx 2.0)
                    (rPS * rPP.conjugate + rPP * rPS.conjugate - rSS * rSP.conjugate - rSP * rSS.conjugate) / (cplx 2.0)
                    (rSS * rPP.conjugate + rSP * rPS.conjugate + rPS * rSP.conjugate + rPP * rSS.conjugate) / (cplx 2.0)
                    (complexI / (cplx 2.0)) * (rSS * rPP.conjugate + rSP * rPS.conjugate - rPS * rSP.conjugate - rPP * rSS.conjugate)
                ]
                [
                    (complexI / (cplx 2.0)) * (-(rPS * rPP.conjugate) + rPP * rPS.conjugate - rSS * rSP.conjugate + rSP * rSS.conjugate)
                    (complexI / (cplx 2.0)) * (-(rPS * rPP.conjugate) + rPP * rPS.conjugate + rSS * rSP.conjugate - rSP * rSS.conjugate)
                    (complexI / (cplx 2.0)) * (-(rSS * rPP.conjugate) + rSP * rPS.conjugate - rPS * rSP.conjugate + rPP * rSS.conjugate)
                    (rSS * rPP.conjugate - rSP * rPS.conjugate - rPS * rSP.conjugate + rPP * rSS.conjugate) / (cplx 2.0)
                ]
            ]
            |> ComplexMatrix4x4.create
        ).re
        |> MuellerMatrix


    type ComplexVector3
        with
        /// I.V. Lindell: Methods for Electromagnetic Field Analysis, Chaptr 1, Vector p.
        member this.pVector = 
            let n = this.norm

            if n < almostZero then RealVector3.zeroVector
            else ((ComplexVector3.cross this this.conjugate) / (createComplex 0.0 (n * n))).re

        /// I.V. Lindell: Methods for Electromagnetic Field Analysis, Chaptr 1, Vector q.
        member a.qVector = 
            let n = a.norm

            if n < almostZero then RealVector3.zeroVector
            else 
                let aa = a * a
                let r = (a / (sqrt aa)).re
                (aa.abs / (n * n)) * (r / r.norm)

        member this.ellipticity : Ellipticity = 
            let v = this.pVector.norm
            if v < almostZero then Ellipticity.defaultValue
            else (1.0 - sqrt(1.0 - v * v)) / v |> Ellipticity


    type EmField
        with
        member em.stokesVector =
            let stokes (b : ComplexBasis3) = 
                let (E e) = em.e
                let ex = e * b.cX
                let ey = e * b.cY
                let s0 = (ex * ex.conjugate + ey * ey.conjugate).Real
                let s1 = (ex * ex.conjugate - ey * ey.conjugate).Real
                let s2 = (ex * ey.conjugate + ey * ex.conjugate).Real
                let s3 = ((createComplex 0.0 1.0) * (ex * ey.conjugate - ey * ex.conjugate)).Real
                [ s0; s1; s2; s3 ] |> StokesVector.create

            thread em.complexBasis stokes

        member em.intensity (i : EmField) = 
            let (S s) = em.s
            let (S is) = i.s
            (s.z |> abs) / is.z

        member em.intensityX (i : EmField) = 
            let (E e) = em.e
            let (H h) = em.h
            let (S is) = i.s
            (ComplexVector3.cross (ComplexBasis3.defaultValue.toX e) h.conjugate).re.norm / is.z

        member em.intensityY (i : EmField) = 
            let (E e) = em.e
            let (H h) = em.h
            let (S is) = i.s
            (ComplexVector3.cross (ComplexBasis3.defaultValue.toY e) h.conjugate).re.norm / is.z

        member em.amplitudeX (i : EmField) = 
            let cZ = 
                match em.complexNormal with 
                | Some z -> z
                | None -> ComplexBasis3.defaultValue.cZ

            let cX = ComplexVector3.cross ComplexBasis3.defaultValue.cY cZ

            let (E e) = em.e
            let (S is) = i.s
            (ComplexBasis3.defaultValue.cX * e) / (sqrt is.z |> cplx)

        member em.amplitudeY (i : EmField) = 
            let (E e) = em.e
            let (S is) = i.s
            (ComplexBasis3.defaultValue.cY * e) // / (sqrt is.z |> cplx)


        member em.ellipticity : Ellipticity = 
            let (E e) = em.e
            let (S s) = em.s
            let p = e.pVector

            if p * s >= 0.0 then -e.ellipticity
            else e.ellipticity

        member em.azimuth : Polarization = 
            let (E e) = em.e
            let q = e.qVector

            let n = q.norm
            if n < almostZero then Polarization.defaultValue
            else 
                let v = (q / q.norm) * RealBasis3.defaultValue.vY
                let a = asin v
                a |> Angle |> Polarization

        member em.muellerMatrix : MuellerMatrix = 
            failwith ""


    type FunctionDescription = 
        {
            name : string
            subscript : string option
            description : string option
        }
        member this.fullName =
            match this.subscript with 
            | Some s -> this.name + s
            | None -> this.name


    type OpticalFunction = 
        | I
        | Ip
        | Is
        | R
        | Rp
        | Rs
        | T
        | Tp
        | Ts

        member this.info = 
            match this with
            | I -> { name = "I"; subscript = None; description = None }
            | Ip -> { name = "I"; subscript = None; description = None }
            | Is -> { name = "I"; subscript = None; description = None }
            | R -> { name = "R"; subscript = None; description = None }
            | Rp -> { name = "R"; subscript = Some "p"; description = None }
            | Rs -> { name = "R"; subscript = Some "s"; description = None }
            | T -> { name = "T"; subscript = None; description = None }
            | Tp -> { name = "T"; subscript = Some "p"; description = None }
            | Ts -> { name = "T"; subscript = Some "s"; description = None }


    type EmFieldSystem
        with 
        member this.i = this.incident.intensity this.incident
        member this.ip = this.incident.intensityX this.incident
        member this.is = this.incident.intensityY this.incident


        member this.r = this.reflected.intensity this.incident
        member this.rp = this.reflected.intensityX this.incident
        member this.rs = this.reflected.intensityY this.incident

        member this.t = this.transmitted.intensity this.incident
        member this.tp = this.transmitted.intensityX this.incident
        member this.ts = this.transmitted.intensityY this.incident

        member this.func f = 
            match f with
            | I -> this.i
            | Ip -> this.ip
            | Is -> this.is
            | R -> this.r
            | Rp -> this.rp
            | Rs -> this.rs
            | T -> this.t
            | Tp -> this.tp
            | Ts -> this.ts


    type OpticalSystemSolver
        with 
        member this.muellerMatrix : MuellerMatrix = 
            let solS = this.solutionS

            failwith ""


    type Solution
        with
        member this.func f = 
            match this with 
            | Single b -> b.emSys.func f |> Some
            | Multiple m -> 
                let r () = m.rt |> List.choose (fun (r, _) -> r)
                let t () = m.rt |> List.choose (fun (_, t) -> t)
                let fn g l = l |> List.fold (fun acc e -> acc + g e m.incident) 0.0 |> Some

                match f with
                | I -> m.incident.intensity m.incident |> Some
                | Ip -> m.incident.intensityX m.incident |> Some
                | Is -> m.incident.intensityY m.incident |> Some
                | R -> r() |> fn (fun e -> e.intensity)
                | Rp -> r() |> fn (fun e -> e.intensityX)
                | Rs -> r() |> fn (fun e -> e.intensityY)
                | T -> t() |> fn (fun e -> e.intensity)
                | Tp -> t() |> fn (fun e -> e.intensityX)
                | Ts -> t() |> fn (fun e -> e.intensityY)
