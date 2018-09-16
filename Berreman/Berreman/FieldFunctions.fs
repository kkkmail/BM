namespace Berreman

module FieldFunctions = 

    open System.Numerics
    open MathNetNumericsMath
    open MatrixExp

    open Geometry
    open Fields
    open Media
    open BerremanMatrix
    open System.ComponentModel
    open System.Net.Configuration


    type EmField
        with
        member em.stokesVector =
            let stokes (b : ComplexBasis3) = 
                let ex = em.e * b.cX
                let ey = em.e * b.cY
                let s0 = (ex * ex.conjugate + ey * ey.conjugate).Real
                let s1 = (ex * ex.conjugate - ey * ey.conjugate).Real
                let s2 = (ex * ey.conjugate + ey * ex.conjugate).Real
                let s3 = ((createComplex 0.0 1.0) * (ex * ey.conjugate - ey * ex.conjugate)).Real
                [ s0; s1; s2; s3 ] |> StokesVector.create

            thread em.complexBasis stokes

        member em.intensity (i : EmField) = (em.s.z |> abs) / i.s.z

        member em.intensityX (i : EmField) = 
            (ComplexVector3.cross (ComplexBasis3.defaultValue.toX em.e) em.h.conjugate).re.norm / i.s.z

        member em.intensityY (i : EmField) = 
            (ComplexVector3.cross (ComplexBasis3.defaultValue.toY em.e) em.h.conjugate).re.norm / i.s.z

        member em.ellipticity : Ellipticity = 
            failwith ""

        member em.azimuth : Angle = 
            failwith ""

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
