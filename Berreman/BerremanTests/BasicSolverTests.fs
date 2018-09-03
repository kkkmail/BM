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

type BaseOpticalSystemTestData =
    {
        description : string
        opticalSystem : BaseOpticalSystem
        info : IncidentLightInfo
        expected : EmFieldSystem
    }

type BasicSolverTests(output : ITestOutputHelper) =
    let data = 
        [
            (
                let create opticalProperties incidenceAngle waveLength = 
                    let n1SinFita = N1SinFita.create 1.0 incidenceAngle

                    {
                        description = "Snell's law for standard transparent glass, 7 degrees incidence angle."
                        opticalSystem = 
                            {
                                upper = OpticalProperties.vacuum
                                films =
                                    [
                                    ]
                                lower = opticalProperties
                            }
                        info = 
                            {
                                wavelength = waveLength
                                refractionIndex = RefractionIndex.defaultValue
                                incidenceAngle = incidenceAngle
                                polarization = Polarization.defaultValue
                                ellipticity = Ellipticity.defaultValue
                            }
                        expected = 
                            {
                                incident = 
                                    {
                                        wavelength = waveLength
                                        n1SinFita = n1SinFita
                                        e = 
                                            [ 0.992546151641322; 0.; -0.12186934340514745 ]
                                            |> ComplexVector3.fromRe
                                        h = 
                                            [ 0.; 0.9999999999999998; 0. ]
                                            |> ComplexVector3.fromRe
                                    }
                                reflected = 
                                    {
                                        wavelength = waveLength
                                        n1SinFita = n1SinFita
                                        e = 
                                            [ -0.2027874513413428; 0.; -0.024899168169565895 ]
                                            |> ComplexVector3.fromRe
                                        h = 
                                            [ 0.; 0.2043103497061609; 0. ]
                                            |> ComplexVector3.fromRe
                                    }
                                transmitted = 
                                    {
                                        wavelength = waveLength
                                        n1SinFita = n1SinFita
                                        e = 
                                            [ 0.7897587002999794; 0.; -0.06352515217049573; ]
                                            |> ComplexVector3.fromRe
                                        h = 
                                            [ 0.; 1.2043103497061607; 0. ]
                                            |> ComplexVector3.fromRe
                                    }
                            }
                    }

                create OpticalProperties.transparentGlass (Angle.degree 7.0 |> IncidenceAngle) (WaveLength.nm 600.0)
            )

            //(
            //    let create opticalProperties incidenceAngle waveLength = 
            //        let n1SinFita = N1SinFita.create 1.0 incidenceAngle
            //
            //        {
            //            description = "Snell's law for standard transparent glass, normal incidence angle."
            //            opticalSystem = 
            //                {
            //                    upper = OpticalProperties.vacuum
            //                    films =
            //                        [
            //                        ]
            //                    lower = opticalProperties
            //                }
            //            info = 
            //                {
            //                    wavelength = waveLength
            //                    refractionIndex = RefractionIndex.defaultValue
            //                    incidenceAngle = incidenceAngle
            //                    polarization = Polarization.defaultValue
            //                    ellipticity = Ellipticity.defaultValue
            //                }
            //            expected = 
            //                {
            //                    incident = 
            //                        {
            //                            wavelength = waveLength
            //                            n1SinFita = n1SinFita
            //                            e = 
            //                                [ 1.; 0.; 0.0;]
            //                                |> ComplexVector3.fromRe
            //                            h = 
            //                                [  0.; 1.0000000000000002; 0.0 ]
            //                                |> ComplexVector3.fromRe
            //                        }
            //                    reflected = 
            //                        {
            //                            wavelength = waveLength
            //                            n1SinFita = n1SinFita
            //                            e = 
            //                                [ -0.20634920634920656; 0.; 0.0 ]
            //                                |> ComplexVector3.fromRe
            //                            h = 
            //                                [ 0.; 0.2063492063492065; 0.0 ]
            //                                |> ComplexVector3.fromRe
            //                        }
            //                    transmitted = 
            //                        {
            //                            wavelength = waveLength
            //                            n1SinFita = n1SinFita
            //                            e = 
            //                                [ 0.7936507936507936; 0.; 0. ]
            //                                |> ComplexVector3.fromRe
            //                            h = 
            //                                [ 0.; 1.206349206349207; 0. ]
            //                                |> ComplexVector3.fromRe
            //                        }
            //                }
            //        }
            //
            //    create OpticalProperties.transparentGlass IncidenceAngle.normal (WaveLength.nm 600.0)
            //)

            ///////////////////////////////////////////////////////
            //{
            //    description = "One layer homegenious media, normal incidence angle."
            //    opticalSystem = 
            //        {
            //            upper = OpticalProperties.vacuum
            //            films =
            //                [
            //                    {
            //                        properties = OpticalProperties.transparentGlass
            //                        thickness = Thickness.nm 75.0
            //                    }
            //                ]
            //            lower = OpticalProperties.transparentGlass
            //        }
            //    light = 
            //        {
            //            wavelength = WaveLength.nm 600.0
            //            refractionIndex = RefractionIndex.defaultValue
            //            incidenceAngle = Angle.degree 0.0 |> IncidenceAngle
            //            polarization = Polarization.defaultValue
            //            ellipticity = Ellipticity.defaultValue
            //        }
            //    expected = 
            //        [
            //            [ createComplex 0.36812455268467836 0.; createComplex 0. 0.6116950565054284; createComplex 0. 0.; createComplex 0. 0. ]
            //            [ createComplex 0. 1.4132602585501424; createComplex 0.3681245526846782 0.; createComplex 0. 0.; createComplex 0. 0. ]
            //            [ createComplex 0. 0.; createComplex 0. 0.; createComplex 0.36812455268467836 0.; createComplex 0. 0.6116950565054284 ]
            //            [ createComplex 0. 0.; createComplex 0. 0.; createComplex 0. 1.4132602585501424; createComplex 0.3681245526846782 0.]
            //        ]
            //        |> ComplexMatrix.create
            //}
        ]

    member __.runTest (d : BaseOpticalSystemTestData) = 
        output.WriteLine d.description
        let solver = BaseOpticalSystemSolver (d.opticalSystem, d.info)

        output.WriteLine("eigenBasisUpper = {0}\n", solver.eigenBasisUpper)
        output.WriteLine("eigenBasisFilm = {0}\n", solver.eigenBasisFilm)
        output.WriteLine("eigenBasisLower = {0}\n", solver.eigenBasisLower)
        output.WriteLine("coeffTbl = {0}\n", solver.coeffTbl)
        output.WriteLine("freeTbl = {0}\n", solver.freeTbl)
        output.WriteLine("cfm = {0}\n", solver.cfm)

        let eI = solver.incidentLight.e
        let hI = solver.incidentLight.h

        let eR = solver.reflectedLight.e
        let hR = solver.reflectedLight.h

        let eT = solver.transmittedLight.e
        let hT = solver.transmittedLight.h

        verifyVectorEquality output "eI" eI d.expected.incident.e
        verifyVectorEquality output "hI" hI d.expected.incident.h

        verifyVectorEquality output "eR" eR d.expected.reflected.e
        verifyVectorEquality output "hR" hR d.expected.reflected.h

        verifyVectorEquality output "eT" eT d.expected.transmitted.e
        verifyVectorEquality output "hT" hT d.expected.transmitted.h


    [<Fact>]
    member this.basicSolverTest0 () = this.runTest (data.[0])
