namespace Berreman
module Geometry = 

    //open ExtremeNumericsMath

    open System.Numerics
    open MathNetNumericsMath
    open MatrixExp


    let comlpexIdentityMatrix n = diagonalMatrix n (cplx 1.0)
    let comlpexZeroMatrix n = diagonalMatrix n (cplx 0.0)


    // We don't really want to guess if 0.0 is an angle or something else.
    type Angle =
        | Angle of double
        static member degree a = a * degree |> Angle
        static member radian r = r |> Angle


    type RealVector3 =
        | RealVector3 of RealVector


    type ComplexVector2 =
        | ComplexVector2 of ComplexVector
        member this.Item 
            with get i = 
                let (ComplexVector2 v) = this
                v.[i]

        member this.x = this.[0]
        member this.y = this.[1]


    type ComplexVector3 =
        | ComplexVector3 of ComplexVector
        member this.Item 
            with get i = 
                let (ComplexVector3 v) = this
                v.[i]

        member this.x = this.[0]
        member this.y = this.[1]
        member this.z = this.[2]

        static member create a = a |> ComplexVector.create |> ComplexVector3
        static member fromRe a = a |> ComplexVector.fromRe |> ComplexVector3
        static member fromIm a = a |> ComplexVector.fromIm |> ComplexVector3

        static member (+) (ComplexVector3 a, ComplexVector3 b) : ComplexVector3 = 
            a + b |> ComplexVector3

        static member cross (u : ComplexVector3) (v : ComplexVector3) : ComplexVector3 = 
            [
                u.y * v.z - u.z * v.y
                u.z * v.x -  u.x * v.z
                u.x * v.y - u.y * v.x
            ]
            |> ComplexVector3.create

        static member (*) (ComplexVector3 a, ComplexVector3 b) : Complex = 
            a * b

        member this.conjugate = 
            let (ComplexVector3 v) = this
            v.conjugate |> ComplexVector3

        member this.re = 
            let (ComplexVector3 v) = this
            v.re |> RealVector3

        member this.im = 
            let (ComplexVector3 v) = this
            v.im |> RealVector3

        static member (*) (a : Complex, ComplexVector3 b) = 
            a * b |> ComplexVector3

        static member (*) (ComplexVector3 a, b : Complex) = 
            a * b |> ComplexVector3


    type ComplexVector4 = 
        | ComplexVector4 of ComplexVector
        member this.Item 
            with get i = 
                let (ComplexVector4 v) = this
                v.[i]


    type ComplexMatrix3x3 = 
        | ComplexMatrix3x3 of ComplexMatrix
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

        static member identity = comlpexIdentityMatrix 3 |> ComplexMatrix3x3
        static member zero = comlpexZeroMatrix 3 |> ComplexMatrix3x3
        static member create a = a |> ComplexMatrix.create |> ComplexMatrix3x3
        static member fromRe a = a |> ComplexMatrix.fromRe |> ComplexMatrix3x3
        static member fromIm a = a |> ComplexMatrix.fromIm |> ComplexMatrix3x3


    type ComplexMatrix4x4 = 
        | ComplexMatrix4x4 of ComplexMatrix
        member this.Item
            with get(i, j) =
                let (ComplexMatrix4x4 v) = this
                v.[i, j]

        static member create a = ComplexMatrix.create a |> ComplexMatrix4x4
        static member fromRe a = a |> ComplexMatrix.fromRe |> ComplexMatrix4x4
        static member fromIm a = a |> ComplexMatrix.fromIm |> ComplexMatrix4x4

        static member (*) (ComplexMatrix4x4 a, ComplexMatrix4x4 b) : ComplexMatrix4x4 = 
            a * b |> ComplexMatrix4x4

        static member (*) (a : Complex, ComplexMatrix4x4 b) : ComplexMatrix4x4 = 
            a * b |> ComplexMatrix4x4

        static member (*) (ComplexMatrix4x4 a, b : Complex) : ComplexMatrix4x4 = 
            a * b |> ComplexMatrix4x4

        member this.matrixExp (x : Complex) : ComplexMatrix4x4 = 
            let (ComplexMatrix4x4 v) = this * x
            v.matrixExp () |> ComplexMatrix4x4

        static member identity = comlpexIdentityMatrix 4 |> ComplexMatrix4x4


    type EigenBasis =
        {
            v0 : Complex
            v1 : Complex
            e0 : ComplexVector4
            e1 : ComplexVector4
        }
        member this.values = [ this.v0; this.v1 ]
        member this.vectors = [ this.e0; this.e1 ]


    type FullEigenBasis = 
        {
            down : EigenBasis
            up : EigenBasis
        }


    type RotationType = 
        | Euler of Angle * Angle * Angle


    //type Rotation (rotation: RotationType) = 
    //    inherit Matrix3x3 ()

