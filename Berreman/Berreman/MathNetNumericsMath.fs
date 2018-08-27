namespace Berreman

// The purpose of this module is to abstract away differences in vector / matrix libraries.
// Switching between them is VERY painful.
module MathNetNumericsMath =
    open System.Numerics
    open MathNet.Numerics
    open MathNet.Numerics.LinearAlgebra

    let pi = Constants.Pi
    let degree = Constants.Pi / 180.0

    let cplx r = Complex(r, 0.0)
    let createComplex r i = Complex(r, i)

    type RealVector =
        RealVector of Vector<double>


    type RealMatrix = Matrix<double>


    type ComplexVector = 
        | ComplexVector of Vector<Complex>
        static member (*) (ComplexVector a, ComplexVector b) = 
            a.DotProduct(b)

        static member (*) ((a : Complex), ComplexVector b) = 
            (a * b) |> ComplexVector

        static member (*) (a : ComplexVector, (b : Complex)) = b * a

        static member (+) (ComplexVector a, ComplexVector b) = 
            (a + b) |> ComplexVector

        static member create (a : #seq<Complex>) = 
            vector a |> ComplexVector

        member this.Item 
            with get (i: int) = 
                let (ComplexVector v) = this
                v.[i]

        member this.conjugate = 
            let (ComplexVector v) = this
            v.Conjugate() |> ComplexVector

        member this.re = 
            let (ComplexVector v) = this
            v.Real() |> RealVector

        member this.im = 
            let (ComplexVector v) = this
            v.Imaginary() |> RealVector


    type ComplexMatrix = 
        | ComplexMatrix of Matrix<Complex>
        static member (*) (ComplexMatrix a, ComplexMatrix b) = 
            (a * b) |> ComplexMatrix

        static member (*) ((a : Complex), ComplexMatrix b) = 
            (a * b) |> ComplexMatrix

        static member (*) (a : ComplexMatrix, b : Complex) = b * a

        static member (+) (ComplexMatrix a, ComplexMatrix b) = 
            (a + b) |> ComplexMatrix

        static member (-) (ComplexMatrix a, ComplexMatrix b) = 
            (a - b) |> ComplexMatrix

        static member (*) (ComplexVector a, ComplexMatrix b) : ComplexVector = 
            a * b |> ComplexVector

        static member (*) (ComplexMatrix a, ComplexVector b) : ComplexVector = 
            a * b |> ComplexVector

        static member create (a : #seq<#seq<Complex>>) = 
            matrix a |> ComplexMatrix

        member this.inverse = 
            let (ComplexMatrix m) = this
            m.Inverse() |> ComplexMatrix

        member this.Item
            with get((i : int), (j : int)) =
                let (ComplexMatrix v) = this
                v.[i, j]

        member this.conjugateTranspose = 
            let (ComplexMatrix m) = this
            m.ConjugateTranspose() |> ComplexMatrix

        member this.determinant = 
            let (ComplexMatrix m) = this
            m.Determinant()

    and Evd = 
        {
            eigenValues : ComplexVector
            eigenVectors : ComplexMatrix
        }


    let diagonalMatrix (n : int) (e : Complex) = 
        DiagonalMatrix.create n e |> ComplexMatrix
