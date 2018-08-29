namespace Berreman

module Fields = 
    //open ExtremeNumericsMath

    open System.Numerics
    open MathNetNumericsMath

    open Geometry
    open MaterialProperties

    // CGS usits are used.


    // n1 * sin(fita), where fita is the incidence angle and n1 is the refraction index of upper media.
    // This is an invariant and it deserves a type.
    type N1SinFita =
        | N1SinFita of double


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

            (e, h)

        member this.eh0 = this.getEH this.polarization
        member this.eh90 = this.getEH this.polarization.crossed


    type EmFieldXY =
        {
            wavelength : double
            n1SinFita : N1SinFita
            e : ComplexVector2
            h : ComplexVector2
        }


    type EmField =
        {
            wavelength : double
            n1SinFita : N1SinFita
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
