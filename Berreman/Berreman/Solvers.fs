module Solvers
open System.Numerics
open MathNet.Numerics.ComplexExtensions
open MathNet.Numerics.LinearAlgebra

open Geometry
open Fields
open MaterialProperties
open Media
open BerremanMatrix
open MathNet.Symbolics
open MathNet.Numerics


type BaseOpticalSystemSolver (system: BaseOpticalSystem, em : EmField) = 
    let sortEvd (evd : Factorization.Evd<Complex>) : (Vector<Complex> * Matrix<Complex>) = 
        (evd.EigenValues, evd.EigenVectors)

    member this.solve() : EmFieldSystem = 
        let (BerremanMatrix.Value upper) = BerremanMatrix.create system.upper em
        let (BerremanMatrix.Value lower) = BerremanMatrix.create system.lower em
        let (BerremanMatrixPropagated.Value (ComplexMatrix4x4.Value layers)) = BerremanMatrixPropagated.propagate (system.thinFilm, em)
        let (v, e) = layers |> Matrix.eigen |> sortEvd



        failwith ""

