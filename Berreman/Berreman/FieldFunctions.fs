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

            thread em.complesBasis stokes

        // (ComplexVector3.cross this.e this.h.conjugate).re
        member em.intensity= em.s.norm
        member em.intensityX = thread em.complesBasis (fun b -> (ComplexVector3.cross (b.toX em.e) em.h.conjugate).re.norm)
        member em.intensityY = thread em.complesBasis (fun b -> (ComplexVector3.cross (b.toY em.e) em.h.conjugate).re.norm)

        member em.ellipticity : Ellipticity = 
            failwith ""

        member em.azimuth : Angle = 
            failwith ""

        member em.muellerMatrix : MuellerMatrix = 
            failwith ""

