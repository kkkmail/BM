module Geometry

open System.Numerics
open MathNet.Numerics.ComplexExtensions
open MathNet.Numerics.LinearAlgebra

//type Vector3 (x : Complex, y : Complex, z : Complex) =
//    member this.X = 0


//type Matrix<'U>
//    with 
//    member this.exp = 0

type RealVector3 =
    | RealVector3 of Vector<double>


type ComplexVector2 =
    | ComplexVector2 of Vector<Complex>
    member this.Item 
        with get i = 
            let (ComplexVector2 v) = this
            v.[i]

    member this.x = this.[0]
    member this.y = this.[1]


type ComplexVector3 =
    | ComplexVector3 of Vector<Complex>
    member this.Item 
        with get i = 
            let (ComplexVector3 v) = this
            v.[i]

    member this.x = this.[0]
    member this.y = this.[1]
    member this.z = this.[2]

    static member (+) (ComplexVector3 a, ComplexVector3 b) : ComplexVector3 = 
        a + b |> ComplexVector3

    // TODO: 3D cross operator
    static member cross (ComplexVector3 a) (ComplexVector3 b) : ComplexVector3 = 
        failwith ""

    static member (*) (ComplexVector3 a, ComplexVector3 b) : Complex = 
        a * b

    member this.conjugate = 
        let (ComplexVector3 v) = this
        v.Conjugate() |> ComplexVector3

    member this.re = 
        let (ComplexVector3 v) = this
        v.Real () |> RealVector3

    member this.im = 
        let (ComplexVector3 v) = this
        v.Imaginary () |> RealVector3


type ComplexVector4 = 
    | ComplexVector4 of Vector<Complex>
    member this.Item 
        with get i = 
            let (ComplexVector4 v) = this
            v.[i]


type ComplexMatrix3x3 = 
    | ComplexMatrix3x3 of Matrix<Complex>
    member this.Item
        with get(i, j) =
            let (ComplexMatrix3x3 v) = this
            v.[i, j]

    static member (*) (ComplexMatrix3x3 a, ComplexMatrix3x3 b) : ComplexMatrix3x3 = 
        a * b |> ComplexMatrix3x3

    static member (*) (a : Complex, ComplexMatrix3x3 b) : ComplexMatrix3x3 = 
        a * b |> ComplexMatrix3x3

    static member (*) (ComplexMatrix3x3 a, b : Complex) : ComplexMatrix3x3 = 
        a * b |> ComplexMatrix3x3

    static member (*) (ComplexVector3 a, ComplexMatrix3x3 b) : ComplexVector3 = 
        a * b |> ComplexVector3

    static member (*) (ComplexMatrix3x3 a, ComplexVector3 b) : ComplexVector3 = 
        a * b |> ComplexVector3

type ComplexMatrix4x4 = 
    | ComplexMatrix4x4 of Matrix<Complex>
    member this.Item
        with get(i, j) =
            let (ComplexMatrix4x4 v) = this
            v.[i, j]

    static member create a = matrix a |> ComplexMatrix4x4

    static member (*) (ComplexMatrix4x4 a, ComplexMatrix4x4 b) : ComplexMatrix4x4 = 
        a * b |> ComplexMatrix4x4

    static member (*) (a : Complex, ComplexMatrix4x4 b) : ComplexMatrix4x4 = 
        a * b |> ComplexMatrix4x4

    static member (*) (ComplexMatrix4x4 a, b : Complex) : ComplexMatrix4x4 = 
        a * b |> ComplexMatrix4x4

    member this.matrixExp (x : Complex) : ComplexMatrix4x4 = 
        let (ComplexMatrix4x4 v) = this
        let evd = v.Evd ()
        let v = (x * evd.EigenValues).PointwiseExp () |> DiagonalMatrix.ofDiag
        let e = evd.EigenVectors
        e * v * e.Inverse() |> ComplexMatrix4x4

    static member identity : ComplexMatrix4x4 =
        failwith ""


type EigenBasis =
    {
        v0 : Complex
        v1 : Complex
        e0 : ComplexVector4
        e1 : ComplexVector4
    }
    member this.values = [this.v0; this.v1]
    member this.vectors = [this.e0; this.e1]


type FullEigenBasis = 
    {
        down : EigenBasis
        up : EigenBasis
    }


type RotationType = 
    | Euler of double * double * double


//type Rotation (rotation: RotationType) = 
//    inherit Matrix3x3 ()

