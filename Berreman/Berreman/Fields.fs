namespace Berreman

module Fields = 
    //open ExtremeNumericsMath

    open MathNetNumericsMath

    open Geometry
    open MaterialProperties

    // CGS usits are used.


    type WaveLength = 
        WaveLength of double
        with
        static member nm l = l * Constants.nm |> WaveLength
        static member mkm l = l * Constants.mkm |> WaveLength


    type Ellipticity =
        | Ellipticity of float
        static member create (e : double) =
            Ellipticity (max (min e 1.0) -1.0)

        static member defaultValue = Ellipticity 0.0


    type Polarization = 
        | Polarization of Angle
        static member create (Angle p) =
            p % (pi / 2.0) |> Angle |> Polarization

        member this.crossed = 
            let (Polarization (Angle p)) = this
            Angle (p + (pi / 2.0)) |> Polarization

        static member defaultValue = Angle 0.0 |> Polarization

        // TODO Check angle
        static member s = Angle 0.0 |> Polarization
        static member p = Polarization.s.crossed


    type IncidenceAngle = 
        | IncidenceAngle of Angle
        static member create (p : double) =
            (p % (pi / 2.0) + pi) % (pi / 2.0) |> Angle |> IncidenceAngle

        static member normal = IncidenceAngle.create 0.0


    // n1 * sin(fita), where fita is the incidence angle and n1 is the refraction index of upper media.
    // This is an invariant and it deserves a type.
    type N1SinFita =
        | N1SinFita of double

        static member create n (IncidenceAngle(Angle f)) = n * (sin f) |> N1SinFita
        static member normal = N1SinFita 0.0

        member this.complex = 
            let (N1SinFita nsf) = this
            cplx nsf


    type IncidentLightInfo = 
        {
            wavelength : WaveLength
            refractionIndex : RefractionIndex
            incidenceAngle : IncidenceAngle
            polarization : Polarization
            ellipticity : Ellipticity
        }
        member this.getEH (Polarization (Angle beta)) = 
            let (RefractionIndex n1) = this.refractionIndex
            let (IncidenceAngle (Angle fita)) = this.incidenceAngle

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
        member this.ehS = this.getEH Polarization.s
        member this.ehP = this.getEH Polarization.p

        member this.n1SinFita = 
            let (IncidenceAngle (Angle a)) = this.incidenceAngle
            let (RefractionIndex n) = this.refractionIndex
            n * (sin a) |> N1SinFita

    type EmFieldXY =
        {
            wavelength : WaveLength
            n1SinFita : N1SinFita
            opticalProperties : OpticalProperties
            e : ComplexVector2
            h : ComplexVector2
        }


    type EmField =
        {
            wavelength : WaveLength
            n1SinFita : N1SinFita
            opticalProperties : OpticalProperties
            e : ComplexVector3
            h : ComplexVector3
        }
        member this.d = this.opticalProperties.eps * this.e + this.opticalProperties.rho * this.h
        member this.b = this.opticalProperties.rhoT * this.e + this.opticalProperties.mu * this.h

        // Poynting vector
        member this.s : RealVector3 = (ComplexVector3.cross this.e this.h.conjugate).re

        static member create (emXY : EmFieldXY, eZ, hZ) = 
            { 
                wavelength = emXY.wavelength 
                n1SinFita = emXY.n1SinFita 
                opticalProperties = emXY.opticalProperties
                e = [ emXY.e.x; emXY.e.y; eZ ] |> ComplexVector.create |> ComplexVector3
                h = [ emXY.h.x; emXY.h.y; hZ ] |> ComplexVector.create |> ComplexVector3
            }

        static member create (info : IncidentLightInfo, o : OpticalProperties) = 
            let (Ellipticity e) = info.ellipticity
            let a0 = 1.0 / sqrt(1.0 + e * e) |> cplx
            let a90 = e / sqrt(1.0 + e * e) |> cplx
            let (e0, h0) = info.eh0
            let (e90, h90) = info.eh90

            {
                wavelength = info.wavelength
                n1SinFita = info.n1SinFita
                opticalProperties = o
                e = a0 * e0 + a90 * e90
                h = a0 * h0 + a90 * h90
            }


    type EmFieldSystem =
        {
            incident : EmField
            reflected : EmField
            transmitted : EmField
        }


    type MuellerMatrix = 
        | MuellerMatrix of RealMatrix4x4


    type StokesVector = 
        | StokesVector of RealVector4

        static member create v = v |> RealVector4.create
