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
        member em.intensity= em.s * em.s |> sqrt

        member em.ellipticity : Ellipticity = 
            failwith ""

        member em.azimuth : Angle = 
            failwith ""

        member em.stokesVector =
            let s0 = (em.e.x * em.e.x.conjugate + em.e.y * em.e.y.conjugate).Real
            let s1 = (em.e.x * em.e.x.conjugate - em.e.y * em.e.y.conjugate).Real
            let s2 = (em.e.x * em.e.y.conjugate + em.e.y * em.e.x.conjugate).Real
            let s3 = ((createComplex 0.0 1.0) * (em.e.x * em.e.y.conjugate - em.e.y * em.e.x.conjugate)).Real

            [ s0; s1; s2; s3 ] |> StokesVector.create


        member em.muellerMatrix : MuellerMatrix = 
            failwith ""

