module Fields
open Geometry
open MaterialProperties
open System.Numerics
open MathNet.Numerics.ComplexExtensions
open MathNet.Numerics.LinearAlgebra
open MathNet.Symbolics

// CGS usits are used.

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
            e = [ emXY.e.x; emXY.e.y; eZ ] |> vector |> ComplexVector3.Value
            h = [ emXY.h.x; emXY.h.y; hZ ] |> vector |> ComplexVector3.Value
        }


type EmFieldSystem =
    {
        incident : EmField
        reflected : EmField
        transmitted : EmField
    }
