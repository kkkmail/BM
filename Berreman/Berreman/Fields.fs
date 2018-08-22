module Fields
open Geometry
open System.Numerics
open MathNet.Numerics.ComplexExtensions
open MathNet.Numerics.LinearAlgebra
open MathNet.Symbolics

// CGS usits are used.
type EmField =
    {
        e : ComplexVector3
        h : ComplexVector3
        d : ComplexVector3
        b : ComplexVector3
    }
    member this.s : ComplexVector3 = 
        failwith ""
