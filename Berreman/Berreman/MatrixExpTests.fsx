//===========================================================
#I __SOURCE_DIRECTORY__
#r "System.Core.dll"
#r "System.dll"
#r "System.Numerics.dll"
//===========================================================
#r "../packages/MathNet.Numerics.4.5.1/lib/net461/MathNet.Numerics.dll"
#r "../packages/MathNet.Numerics.FSharp.4.5.1/lib/net45/MathNet.Numerics.FSharp.dll"
#r "../packages/System.ValueTuple.4.5.0/lib/net47/System.ValueTuple.dll"
//===========================================================
#r "./bin/Debug/Berreman.dll"
//===========================================================

open System.Numerics
open Berreman.MathNetNumericsMath
open Berreman.MatrixExp
open Berreman.Geometry

//===========================================================
let m1 = 
    [
        [0.0 |> cplx; 0.0 |> cplx; 0.0 |> cplx; 1.0 |> cplx]
        [0.0 |> cplx; 0.0 |> cplx; 1.0 |> cplx; 0.0 |> cplx]
        [0.0 |> cplx; 1.0 |> cplx; 0.0 |> cplx; 0.0 |> cplx]
        [1.0 |> cplx; 0.0 |> cplx; 0.0 |> cplx; 0.0 |> cplx]
    ]
    |> ComplexMatrix.create
    |> ComplexMatrix4x4
printfn "m1 = %A" m1

let (ComplexMatrix4x4 b1) = m1.matrixExp(Complex(0.0, pi))
printfn "b1 = %A" b1

//let m2 = 
//    [
//        [0.0 |> cplx; 0.0 |> cplx; 0.0 |> cplx; 1.0 |> cplx]
//        [0.0 |> cplx; 0.0 |> cplx; 1.0 |> cplx; 0.0 |> cplx]
//        [0.0 |> cplx; 1.0 |> cplx; 0.0 |> cplx; 0.0 |> cplx]
//        [0.0 |> cplx; 0.0 |> cplx; 0.0 |> cplx; 0.0 |> cplx]
//    ]
//    |> matrix
//    |> ComplexMatrix4x4
//printfn "m2 = %A" m2

//let b2 = m2.matrixExp(Complex(0.0, Constants.Pi))
//printfn "b2 = %A" b2

//printfn "Completed"
