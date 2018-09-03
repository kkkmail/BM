namespace BerremanTests

open Berreman.Constants
open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.BerremanMatrix

open Xunit
open Xunit.Abstractions

open MatrixComparison
open Berreman.Media

type ThinFilmTestData =
    {
        description : string
        thinFilms : List<Layer>
        em : EmField
        expected : ComplexMatrix
    }

type ThinFilmTests(output : ITestOutputHelper) =

    let data = 
        [
            {
                description = "One layer homegenious media, normal incidence angle."
                thinFilms =
                    [
                        {
                            properties = 1.52 |> RefractionIndex.create |> OpticalProperties.defaultValue
                            thickness = Thickness.nm 75.0
                        }
                    ]
                em = 
                    {
                        wavelength = WaveLength.nm 600.0
                        refractionIndex = RefractionIndex.defaultValue
                        incidenceAngle = Angle.degree 0.0 |> IncidenceAngle
                        polarization = Polarization.defaultValue
                        ellipticity = Ellipticity.defaultValue
                    }
                    |> EmField.create
                expected = 
                    [
                        [ createComplex 0.36812455268467836 0.; createComplex 0. 0.6116950565054284; createComplex 0. 0.; createComplex 0. 0. ]
                        [ createComplex 0. 1.4132602585501424; createComplex 0.3681245526846782 0.; createComplex 0. 0.; createComplex 0. 0. ]
                        [ createComplex 0. 0.; createComplex 0. 0.; createComplex 0.36812455268467836 0.; createComplex 0. 0.6116950565054284 ]
                        [ createComplex 0. 0.; createComplex 0. 0.; createComplex 0. 1.4132602585501424; createComplex 0.3681245526846782 0.]
                    ]
                    |> ComplexMatrix.create
            }

            //{
            //    description = ""
            //    thinFilms =
            //        [
            //        ]
            //    em = failwith ""
            //    expected = failwith ""
            //}

            //{
            //    description = ""
            //    thinFilms =
            //        [
            //        ]
            //    em = failwith ""
            //    expected = failwith ""
            //}
        ]

    member __.runTest (d : ThinFilmTestData) = 
        output.WriteLine d.description
        let (BerremanMatrixPropagated (ComplexMatrix4x4 bm)) = 
            BerremanMatrixPropagated.propagate (d.thinFilms, d.em)
        verifyMatrixEquality output bm d.expected

    [<Fact>]
    member this.berremanMatrixTest0 () = this.runTest (data.[0])

