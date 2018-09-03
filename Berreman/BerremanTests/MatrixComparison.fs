namespace BerremanTests

module MatrixComparison =
    open Berreman.MathNetNumericsMath
    open Berreman.Geometry
    open Xunit
    open Xunit.Abstractions

    let allowedDiff = 1.0e-05

    let verifyMatrixEquality (output : ITestOutputHelper) (ComplexMatrix result) (ComplexMatrix expected) =
        output.WriteLine ("result = {0}", result.ToString())
        output.WriteLine ("expected = {0}", expected.ToString())

        let diff = result - expected
        let norm = expected.L2Norm ()
        let diffNorm = diff.L2Norm ()

        output.WriteLine ("diff = {0}", diff.ToString())

        output.WriteLine ("norm = {0}", norm)
        output.WriteLine ("diffValue = {0}", diffNorm)
        Assert.True(diffNorm / norm < allowedDiff)

    let verifyVectorEquality (output : ITestOutputHelper) (msg : string) (ComplexVector3 (ComplexVector result)) (ComplexVector3 (ComplexVector expected)) =
        output.WriteLine ("{0}", msg)
        output.WriteLine ("result = {0}", result.ToString())
        output.WriteLine ("expected = {0}", expected.ToString())

        let diff = result - expected
        let norm = expected.L2Norm ()
        let diffNorm = diff.L2Norm ()

        output.WriteLine ("diff = {0}", diff.ToString())

        output.WriteLine ("norm = {0}", norm)
        output.WriteLine ("diffValue = {0}", diffNorm)
        Assert.True(diffNorm / norm < allowedDiff)
