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


    type FunctionNameAttribute (name : string) = 
        inherit System.Attribute()

        member val name = name with get
        member val subscript : string Option = None with get, set
        member val description : string Option = None with get, set

        member this.subscr with set (value) = this.subscript <- Some value
        member this.descr with set (value) = this.description <- Some value


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
        [<FunctionName("I")>]
        member this.i = this.incident.intensity this.incident

        [<FunctionName("I", subscr = "p")>]
        member this.ip = this.incident.intensityX this.incident

        [<FunctionName("I", subscr = "s")>]
        member this.is = this.incident.intensityY this.incident


        [<FunctionName("R")>]
        member this.r = this.reflected.intensity this.incident

        [<FunctionName("R", subscr = "p")>]
        member this.rp = this.reflected.intensityX this.incident

        [<FunctionName("R", subscr = "s")>]
        member this.rs = this.reflected.intensityY this.incident


        [<FunctionName("T")>]
        member this.t = this.transmitted.intensity this.incident

        [<FunctionName("T", subscr = "p")>]
        member this.tp = this.transmitted.intensityX this.incident

        [<FunctionName("T", subscr = "s")>]
        member this.ts = this.transmitted.intensityY this.incident

