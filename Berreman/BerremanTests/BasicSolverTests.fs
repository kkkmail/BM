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
        light : IncidentLightInfo
        expected : EmFieldSystem
    }

type BasicSolverTests(output : ITestOutputHelper) =
    let data = 
        [
            (
                let create opticalProperties incidenceAngle waveLength = 
                    let n1SinFita = N1SinFita.create 1.0 incidenceAngle

                    {
                        description = "Snell's law for standard transparent glass, normal incidence angle."
                        opticalSystem = 
                            {
                                upper = OpticalProperties.vacuum
                                films =
                                    [
                                    ]
                                lower = opticalProperties
                            }
                        light = 
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
                                            [
                                            ]
                                            |> ComplexVector3.create
                                        h = 
                                            [
                                            ]
                                            |> ComplexVector3.create
                                    }
                                reflected = 
                                    {
                                        wavelength = waveLength
                                        n1SinFita = n1SinFita
                                        e = 
                                            [
                                            ]
                                            |> ComplexVector3.create
                                        h = 
                                            [
                                            ]
                                            |> ComplexVector3.create
                                    }
                                transmitted = 
                                    {
                                        wavelength = waveLength
                                        n1SinFita = n1SinFita
                                        e = 
                                            [
                                            ]
                                            |> ComplexVector3.create
                                        h = 
                                            [
                                            ]
                                            |> ComplexVector3.create
                                    }
                            }
                    }

                create OpticalProperties.transparentGlass IncidenceAngle.normal (WaveLength.nm 600.0)
            )

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
        let solver = BaseOpticalSystemSolver (d.opticalSystem, d.light |> EmField.create)

        //let (BerremanMatrixPropagated (ComplexMatrix4x4 bm)) = 
        //    BerremanMatrixPropagated.propagate (d.thinFilms, d.em)
        //verifyMatrixEquality output bm d.expected
        failwith ""

    [<Fact>]
    member this.basicSolverTest0 () = this.runTest (data.[0])

