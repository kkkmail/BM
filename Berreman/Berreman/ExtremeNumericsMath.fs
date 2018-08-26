namespace Berreman

// The purpose of this module is to abstract away differences in vector / matrix libraries.
// Switching between them is VERY painful.
module ExtremeNumericsMath =
    open System.Numerics
    open Extreme.Numerics
    open Extreme.Mathematics
    open Extreme.Mathematics.LinearAlgebra

    type RealVector =
        RealVector of DenseVector<double>


    type RealMatrix = DenseMatrix<double>


    type ComplexVector = 
        | ComplexVector of DenseVector<Complex>
        static member (*) (ComplexVector a, ComplexVector b) = 
            a.DotProduct(b)

        static member (*) ((a : Complex), ComplexVector b) = 
            printfn "a = %A, b = %A" a b
            let c = (a * b)
            printfn "c = %A" c
            let d = c.ToDenseVector()
            printfn "d = %A" d
            d|> ComplexVector

        static member (*) (ComplexVector a, (b : Complex)) = 
            (a * b).ToDenseVector() |> ComplexVector

        static member (+) (ComplexVector a, ComplexVector b) = 
            (a + b).ToDenseVector() |> ComplexVector

        static member create (a : #seq<Complex>) = 
            Vector.Create(a |> Array.ofSeq) |> ComplexVector

        member this.Item 
            with get (i: int) = 
                let (ComplexVector v) = this
                v.[i]

        member this.conjugate = 
            let (ComplexVector v) = this
            v.Conjugate().ToDenseVector() |> ComplexVector

        member this.re = 
            let (ComplexVector v) = this
            v.ToArray()
            |> Array.map (fun e -> e.Real)
            |> Vector.Create
            |> RealVector

        member this.im = 
            let (ComplexVector v) = this
            v.ToArray()
            |> Array.map (fun e -> e.Imaginary)
            |> Vector.Create
            |> RealVector


    type ComplexMatrix = 
        | ComplexMatrix of DenseMatrix<Complex>
        static member (*) (ComplexMatrix a, ComplexMatrix b) = 
            (a * b).ToDenseMatrix() |> ComplexMatrix

        static member (*) ((a : Complex), ComplexMatrix b) = 
            (a * b).ToDenseMatrix() |> ComplexMatrix

        static member (*) (ComplexMatrix a, (b : Complex)) = 
            (a * b).ToDenseMatrix() |> ComplexMatrix

        static member (+) (ComplexMatrix a, ComplexMatrix b) = 
            (a + b).ToDenseMatrix() |> ComplexMatrix

        static member (*) (ComplexVector a, ComplexMatrix b) : ComplexVector = 
            a * b |> ComplexVector

        static member (*) (ComplexMatrix a, ComplexVector b) : ComplexVector = 
            a * b |> ComplexVector

        static member create (a : #seq<#seq<Complex>>) = 
            Matrix.Create(array2D a) |> ComplexMatrix

        member this.inverse = 
            let (ComplexMatrix m) = this
            m.GetInverse().ToDenseMatrix() |> ComplexMatrix

        member this.matrixExp = 
            let (ComplexMatrix m) = this
            m.GetExponential().ToDenseMatrix() |> ComplexMatrix

        member this.Item
            with get((i : int), (j : int)) =
                let (ComplexMatrix v) = this
                v.[i, j]

        member this.conjugateTranspose = 
            let (ComplexMatrix m) = this
            m.ConjugateTranspose().ToDenseMatrix() |> ComplexMatrix

        member this.evd = 
            let (ComplexMatrix m) = this
            let evd = m.GetEigenvalueDecomposition()

            {
                eigenValues = evd.Eigenvalues.ToDenseVector() |> ComplexVector
                eigenVectors = evd.Eigenvectors.ToDenseMatrix() |> ComplexMatrix
            }

        member this.determinant = 
            let (ComplexMatrix m) = this
            m.GetDeterminant()

    and Evd = 
        {
            eigenValues : ComplexVector
            eigenVectors : ComplexMatrix
        }


    let diagonalMatrix (n : int) (e : Complex) = 
        Matrix.Create(n, n, fun _ _ -> e) |> ComplexMatrix
