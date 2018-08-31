namespace BerremanTests

open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.BerremanMatrix

open Xunit
open Xunit.Abstractions

open MatrixComparison

type ThinFilmTestData =
    {
        opticalProperties : OpticalProperties
        n1SinFita : N1SinFita
        expected : ComplexMatrix
    }

type ThinFilmTests(output : ITestOutputHelper) =

    let data = 
        [
        ]

    member __.runTest (d : ThinFilmTestData) = failwith ""

    [<Fact>]
    member this.berremanMatrixTest0 () = this.runTest (data.[0])

