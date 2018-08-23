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
    | Value of Vector<double>


type ComplexVector2 =
    | Value of Vector<Complex>
    member this.Item 
        with get i = 
            let (Value v) = this
            v.[i]

    member this.x = this.[0]
    member this.y = this.[1]


type ComplexVector3 =
    | Value of Vector<Complex>
    member this.Item 
        with get i = 
            let (Value v) = this
            v.[i]

    member this.x = this.[0]
    member this.y = this.[1]
    member this.z = this.[2]

    static member (+) (Value a : ComplexVector3, Value b : ComplexVector3) : ComplexVector3 = 
        a + b |> Value

    // TODO: 3D cross operator
    static member cross (Value a : ComplexVector3) (Value b : ComplexVector3) : ComplexVector3 = 
        failwith ""

    static member (*) (Value a : ComplexVector3, Value b : ComplexVector3) : Complex = 
        a * b

    member this.conjugate = 
        let (Value v) = this
        v.Conjugate() |> Value

    member this.re = 
        let (Value v) = this
        v.Real () |> RealVector3.Value

    member this.im = 
        let (Value v) = this
        v.Imaginary () |> RealVector3.Value


type ComplexMatrix3x3 = 
    | Value of Matrix<Complex>
    member this.Item
        with get(i, j) =
            let (Value v) = this
            v.[i, j]

    static member (*) (Value a : ComplexMatrix3x3, Value b : ComplexMatrix3x3) : ComplexMatrix3x3 = 
        a * b |> Value

    static member (*) (a : Complex, Value b : ComplexMatrix3x3) : ComplexMatrix3x3 = 
        a * b |> Value

    static member (*) (Value a : ComplexMatrix3x3, b : Complex) : ComplexMatrix3x3 = 
        a * b |> Value

    static member (*) (ComplexVector3.Value a : ComplexVector3, Value b : ComplexMatrix3x3) : ComplexVector3 = 
        a * b |> ComplexVector3.Value

    static member (*) (Value a : ComplexMatrix3x3, ComplexVector3.Value b : ComplexVector3) : ComplexVector3 = 
        a * b |> ComplexVector3.Value

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

