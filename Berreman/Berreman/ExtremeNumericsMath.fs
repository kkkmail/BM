namespace Berreman

// The purpose of this module is to abstract away differences in vector / matrix libraries.
// Switching between them is VERY painful.
module ExtremeNumericsMath =
    open System.Numerics
    open Extreme.Numerics
    open Extreme.Mathematics
    open Extreme.Mathematics.LinearAlgebra

    let cplx x = Complex(x, 0.0)


    type RealVector = DenseVector<double>
    type RealMatrix = DenseMatrix<double>


    type ComplexVector = 
        | ComplexVector of DenseVector<Complex>


    type ComplexMatrix = 
        | ComplexMatrix of DenseMatrix<Complex>
        with 
            static member (*) (ComplexMatrix a, ComplexMatrix b) : ComplexMatrix = 
                (a * b).ToDenseMatrix() |> ComplexMatrix

            static member (+) (ComplexMatrix a, ComplexMatrix b) : ComplexMatrix = 
                (a + b).ToDenseMatrix() |> ComplexMatrix


    let diagonalMatrix (n : int) (e : Complex) : ComplexMatrix = 
        Matrix.Create(n, n, fun _ _ -> e) |> ComplexMatrix

    let getComplexVectorItem (ComplexVector v) (i : int) = v.[i]
    let getComplexMatrixItem (ComplexMatrix m) (i : int) (j : int) = m.[i, j]

