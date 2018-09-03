namespace BerremanTests

open Berreman.Constants
open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.BerremanMatrix
open Berreman.Solvers

open Xunit
open Xunit.Abstractions

open MatrixComparison
open Berreman.Media
open OpticalProperties.Standard

type EigenVectorTestData =
    {
        description : string
        opticalProperties : OpticalProperties
        n1SinFita : N1SinFita
        expected : FullEigenBasis
    }

type EigenVecgtorTests(output : ITestOutputHelper) =
    let data = 
        [
            {
                description = "Snell's law for standard transparent glass, 7 degrees incidence angle."
                opticalProperties = 1.52 |> RefractionIndex.create |> OpticalProperties.defaultValue
                n1SinFita = N1SinFita.create 7.0 (Angle.degree 0.0 |> IncidenceAngle)
                expected = 
                    {
                        up = 
                            {
                                v0 = failwith "" // : Complex
                                v1 = failwith "" // Complex
                                e0 =
                                    [
                                    ]
                                    |> ComplexVector4.create
                                e1 =
                                    [
                                    ]
                                    |> ComplexVector4.create
                            }
                        down =  
                            {
                                v0 = failwith "" // : Complex
                                v1 = failwith "" // Complex
                                e0 =
                                    [
                                    ]
                                    |> ComplexVector4.create
                                e1 =
                                    [
                                    ]
                                    |> ComplexVector4.create
                            }
                    }
            }
        ]

    member __.runTest (d : EigenVectorTestData) = 
        //output.WriteLine d.description
        //let solver = BaseOpticalSystemSolver (d.opticalSystem, d.info)

        //output.WriteLine("eigenBasisUpper = {0}\n", solver.eigenBasisUpper)
        //output.WriteLine("eigenBasisFilm = {0}\n", solver.eigenBasisFilm)
        //output.WriteLine("eigenBasisLower = {0}\n", solver.eigenBasisLower)
        //output.WriteLine("coeffTbl = {0}\n", solver.coeffTbl)
        //output.WriteLine("freeTbl = {0}\n", solver.freeTbl)
        //output.WriteLine("cfm = {0}\n", solver.cfm)

        //let eI = solver.incidentLight.e
        //let hI = solver.incidentLight.h

        //let eR = solver.reflectedLight.e
        //let hR = solver.reflectedLight.h

        //let eT = solver.transmittedLight.e
        //let hT = solver.transmittedLight.h

        //verifyVectorEquality output "eI" eI d.expected.incident.e
        //verifyVectorEquality output "hI" hI d.expected.incident.h

        //verifyVectorEquality output "eR" eR d.expected.reflected.e
        //verifyVectorEquality output "hR" hR d.expected.reflected.h

        //verifyVectorEquality output "eT" eT d.expected.transmitted.e
        //verifyVectorEquality output "hT" hT d.expected.transmitted.h
        failwith ""


    [<Fact>]
    member this.basicSolverTest0 () = this.runTest (data.[0])
