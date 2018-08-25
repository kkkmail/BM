module Fields
open Geometry
open MaterialProperties
open System.Numerics
open MathNet.Numerics
open MathNet.Numerics.ComplexExtensions
open MathNet.Numerics.LinearAlgebra
open System

// CGS usits are used.

type Ellipticity =
    | Ellipticity of float
    static member create (e : double) =
        Ellipticity (max (min e 1.0) -1.0)


type Polarization = 
    | Polarization of float
    static member create (p : double) =
        p % (Constants.Pi / 2.0) |> Polarization

type IncidentAngle = 
    | IncidentAngle of float
    static member create (p : double) =
        (p % (Constants.Pi / 2.0) + Constants.Pi) % (Constants.Pi / 2.0) |> IncidentAngle


type IncidentLightInfo = 
    {
        wavelength : double
        refractionIndex : double
        incidentAngle : IncidentAngle
        polarization : Polarization
        ellipticity : Ellipticity
    }
    member this.getEH (beta : double) = 
        let n1 = this.refractionIndex
        let (IncidentAngle fita) = this.incidentAngle

        let e = 
            [
                cos(beta) * cos(fita)
                sin(beta)
                -cos(beta) * sin(fita)
            ]
            |> ComplexVector3

        let h = 
            [
                -n1 * cos(fita) * sin(beta)
                n1 * cos(beta)
                n1 * sin(beta) * sin(fita)
            ]
        failwith ""

type EmFieldXY =
    {
        wavelength : double
        n1SinFita : double
        e : ComplexVector2
        h : ComplexVector2
    }

    static member create (info : IncidentLightInfo ) = 
        let (IncidentAngle a) = info.incidentAngle
        let (Ellipticity e) = info.ellipticity
        let a1 = 1.0 / Math.Sqrt(1.0 + e * e)
        let a2 = e / Math.Sqrt(1.0 + e * e)

        {
            wavelength = info.wavelength
            n1SinFita = info.refractionIndex * (Math.Sin a)
            e = failwith ""
            h = failwith ""
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
            e = [ emXY.e.x; emXY.e.y; eZ ] |> vector |> ComplexVector3.Value
            h = [ emXY.h.x; emXY.h.y; hZ ] |> vector |> ComplexVector3.Value
        }


type EmFieldSystem =
    {
        incident : EmField
        reflected : EmField
        transmitted : EmField
    }
