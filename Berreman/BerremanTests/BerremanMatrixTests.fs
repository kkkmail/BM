namespace BerremanTests

open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.BerremanMatrix

open Xunit
open Xunit.Abstractions

open MatrixComparison


type BerremanMatrixTestData =
    {
        opticalProperties : OpticalProperties
        n1SinFita : N1SinFita
        expected : ComplexMatrix
    }


type BerremanMatrixTests(output : ITestOutputHelper) =
    let data = 
        [|
            {
                opticalProperties = OpticalProperties.defaultValue 1.5
                n1SinFita = N1SinFita.create 1.0 (Angle.degree 0.0)
                expected = 
                    [
                        [ cplx 0.0; cplx 1.0; cplx 0.0; cplx 0.0 ]
                        [ cplx 2.25; cplx 0.0; cplx 0.0; cplx 0.0 ]
                        [ cplx 0.0; cplx 0.0; cplx 0.0; cplx 1.0 ]
                        [ cplx 0.0; cplx 0.0; cplx 2.25; cplx 0.0 ]
                    ]
                    |> ComplexMatrix.create
            }
        |]

    // Calculated and expected Berreman matrix.
    member __.runTest (d : BerremanMatrixTestData) = 
        let (BerremanMatrix (ComplexMatrix4x4 bm)) = BerremanMatrix.create d.opticalProperties d.n1SinFita
        verifyMatrixEquality output bm d.expected

    [<Fact>]
    member this.berremanMatrixTest0 () = this.runTest (data.[0])

    //[<Fact>]
    //member this.berremanMatrixTest1 () = this.runTest (data.[1])

    //[<Fact>]
    //member this.berremanMatrixTest2 () = this.runTest (data.[2])

    //[<Fact>]
    //member this.berremanMatrixTest3 () = this.runTest (data.[3])

    //[<Fact>]
    //member this.berremanMatrixTest4 () = this.runTest (data.[4])
