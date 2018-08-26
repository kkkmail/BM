namespace Berreman

module Fields = 
    open Constants
    open ExtremeNumericsMath
    open Geometry
    open MaterialProperties
    open System.Numerics
    //open MathNet.Numerics
    open MathNet.Numerics.ComplexExtensions
    //open MathNet.Numerics.LinearAlgebra
    open System

    // CGS usits are used.

    type Ellipticity =
        | Ellipticity of float
        static member create (e : double) =
            Ellipticity (max (min e 1.0) -1.0)

        static member defaultValue = Ellipticity 0.0


    type Polarization = 
        | Polarization of float
        static member create (p : double) =
            p % (pi / 2.0) |> Polarization

        member this.crossed = 
            let (Polarization p) = this
            Polarization (p + (pi / 2.0))

        static member defaultValue = Polarization 0.0


    type IncidentAngle = 
        | IncidentAngle of float
        static member create (p : double) =
            (p % (pi / 2.0) + pi) % (pi / 2.0) |> IncidentAngle


    type IncidentLightInfo = 
        {
            wavelength : double
            refractionIndex : double
            incidentAngle : IncidentAngle
            polarization : Polarization
            ellipticity : Ellipticity
        }
        member this.getEH (Polarization beta) = 
            let n1 = this.refractionIndex
            let (IncidentAngle fita) = this.incidentAngle

            let e = 
                [
                    cos(beta) * cos(fita) |> cplx
                    sin(beta) |> cplx
                    -cos(beta) * sin(fita) |> cplx
                ]
                |> ComplexVector.create
                |> ComplexVector3

            let h = 
                [
                    -n1 * cos(fita) * sin(beta) |> cplx
                    n1 * cos(beta) |> cplx
                    n1 * sin(beta) * sin(fita) |> cplx
                ]
                |> ComplexVector.create
                |> ComplexVector3

            printfn "e = %A" e
            printfn "h = %A" h
            (e, h)

        member this.eh0 = this.getEH this.polarization
        member this.eh90 = this.getEH this.polarization.crossed


    type EmFieldXY =
        {
            wavelength : double
            n1SinFita : double
            e : ComplexVector2
            h : ComplexVector2
        }


    type EmField =
        {
            wavelength : double
            n1SinFita : double
            e : ComplexVector3
            h : ComplexVector3
        }
        member this.d (o : OpticalProperties) : ComplexVector3 = o.eps * this.e + o.rho * this.h

        member this.b (o : OpticalProperties) : ComplexVector3 = o.rhoT * this.e + o.mu * this.h

        // Poynting vector
        member this.s : RealVector3 = (ComplexVector3.cross this.e this.h.conjugate).re

        static member create (emXY : EmFieldXY, eZ, hZ) = 
            { 
                wavelength = emXY.wavelength 
                n1SinFita = emXY.n1SinFita 
                e = [ emXY.e.x; emXY.e.y; eZ ] |> ComplexVector.create |> ComplexVector3
                h = [ emXY.h.x; emXY.h.y; hZ ] |> ComplexVector.create |> ComplexVector3
            }

        static member create (info : IncidentLightInfo ) = 
            let (IncidentAngle a) = info.incidentAngle
            let (Ellipticity e) = info.ellipticity
            let a0 = 1.0 / sqrt(1.0 + e * e) |> cplx
            let a90 = e / sqrt(1.0 + e * e) |> cplx
            let (e0, h0) = info.eh0
            let (e90, h90) = info.eh90

            printfn "e0 = %A" e0
            printfn "h0 = %A" h0
            printfn "e90 = %A" e90
            printfn "h90 = %A" h90

            {
                wavelength = info.wavelength
                n1SinFita = info.refractionIndex * (sin a)
                e = a0 * e0 + a90 * e90
                h = a0 * h0 + a90 * h90
            }


    type EmFieldSystem =
        {
            incident : EmField
            reflected : EmField
            transmitted : EmField
        }
