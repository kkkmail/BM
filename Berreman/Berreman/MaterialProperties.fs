﻿module MaterialProperties

open Geometry
open System.Numerics
open MathNet.Numerics.ComplexExtensions
open MathNet.Numerics.LinearAlgebra


type OpticalProperties = 
    {
        eps : ComplexMatrix3x3
        mu : ComplexMatrix3x3
        rho : ComplexMatrix3x3
    }
    //inherit ComplexMatrix3x3 ()
    ////| A of Matrix<Complex>(3, 3)
    ////with
    ////static member create e = 
    ////    0
    //member this.rotate (rotation : Rotation) : OpticalTensor = 
    //    failwith ""

    member this.rhoT : ComplexMatrix3x3 = 
        let (ComplexMatrix3x3.Value r) = this.rho
        r.ConjugateTranspose () |> ComplexMatrix3x3.Value


