namespace BerremanTests

open MathNet.Numerics
open Berreman.MatrixExp
open Berreman.MathNetNumericsMath

open Xunit
open Xunit.Abstractions
open Xunit.Should

type MatrixExpTests(output : ITestOutputHelper) =
    let allowedDiff = 1.0e-05

    // Complex matrix and expected matrix exponent
    member __.runTest (m : ComplexMatrix) (e : ComplexMatrix) = 
        let exp = m.matrixExp()
        let (ComplexMatrix diff) = exp - e

        let diffValue = diff.L2Norm ()
        output.WriteLine ("diffValue = {0}", diffValue)
        diffValue.ShouldBeLessThan(allowedDiff)

    [<Fact>]
    member __.thisShouldFail () = 
        Assert.False(true)

