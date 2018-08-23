module Geometry

open System.Numerics
open MathNet.Numerics.ComplexExtensions
open MathNet.Numerics.LinearAlgebra

//type Vector3 (x : Complex, y : Complex, z : Complex) =
//    member this.X = 0


//type Matrix<'U>
//    with 
//    member this.exp = 0


type ComplexVector3 =
    | Value of Vector<Complex>
    member this.Item 
        with get i = 
            let (Value v) = this
            v.[i]


type ComplexMatrix3x3 = 
    | Value of Matrix<Complex>
    member this.Item
        with get(i, j) =
            let (Value v) = this
            v.[i, j]


type ComplexMatrix4x4 = 
    | Value of Matrix<Complex>
    member this.Item
        with get(i, j) =
            let (Value v) = this
            v.[i, j]

    static member create a = matrix a |> ComplexMatrix4x4.Value

    static member (*) (Value a : ComplexMatrix4x4, Value b : ComplexMatrix4x4) : ComplexMatrix4x4 = 
        a * b |> Value

    static member (*) (a : Complex, Value b : ComplexMatrix4x4) : ComplexMatrix4x4 = 
        a * b |> Value

    static member (*) (Value a : ComplexMatrix4x4, b : Complex) : ComplexMatrix4x4 = 
        a * b |> Value

    member this.matrixExp () : ComplexMatrix4x4 = 
        failwith ""

    static member identity : ComplexMatrix4x4 =
        failwith ""


type RotationType = 
    | Euler of double * double * double


//type Rotation (rotation: RotationType) = 
//    inherit Matrix3x3 ()

