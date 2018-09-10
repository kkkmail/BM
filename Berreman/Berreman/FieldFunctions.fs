namespace Berreman

module FieldFunctions = 

    open System.Numerics
    open MathNetNumericsMath
    open MatrixExp

    open Geometry
    open Fields
    open Media
    open BerremanMatrix

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

