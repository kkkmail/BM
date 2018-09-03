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

            {
                description = "One layer homegenious media, 85 degrees incidence angle."
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
                        incidenceAngle = Angle.degree 85.0 |> IncidenceAngle
                        polarization = Polarization.defaultValue
                        ellipticity = Ellipticity.defaultValue
                    }
                    |> EmField.create
                expected = 
                    [
                        [ createComplex 0.6203020411609136 0.; createComplex 0. 0.38975079646975697; createComplex 0. 0.; createComplex 0. 0. ]
                        [ createComplex 0. 1.578509609997277; createComplex 0.6203020411609138 0.; createComplex 0. 0.; createComplex 0. 0. ]
                        [ createComplex 0. 0.; createComplex 0. 0.; createComplex 0.620302041160914 0.; createComplex 0. 0.6832191871525611 ]
                        [ createComplex 0. 0.; createComplex 0. 0.; createComplex 0. 0.900480240163726; createComplex 0.6203020411609139 0. ]
                    ]
                    |> ComplexMatrix.create
            }

            {
                description = "One layer homegenious media, random incident light, random real epsilon."
                thinFilms =
                    [
                        {
                            properties = 
                                {
                                    eps = 
                                        [
                                            [ 3.2348312413417437; -0.13274761403308766; -0.46073345815011246 ]
                                            [ -0.13274761403308766; 3.381257622001705; -0.6979528788314281 ]
                                            [ -0.46073345815011235; -0.697952878831428; 4.5371548045970345 ]
                                        ]
                                        |> ComplexMatrix3x3.fromRe
                                    mu = ComplexMatrix3x3.identity
                                    rho = ComplexMatrix3x3.zero
                                }
                            thickness = Thickness.nm 227.0
                        }
                    ]
                em = 
                    {
                        wavelength = WaveLength.nm 687.0
                        refractionIndex = RefractionIndex.defaultValue
                        incidenceAngle = Angle.degree 53.0 |> IncidenceAngle
                        polarization = Angle.degree 50.0 |> Polarization
                        ellipticity = -0.802933683069591 |> Ellipticity
                    }
                    |> EmField.create
                expected = 
                    [
                        [ createComplex -0.9274014609420038 -0.16759861804121412; createComplex 0.017170207434521613 -0.1456139561785692; createComplex -0.017403663019614386 -0.13060623757618983; createComplex 0.014075507609934673 0.06306021155774927 ]
                        [ createComplex 0.06324527789089204 -0.5351670353761716; createComplex -0.9274014609420037 -0.16759861804121412; createComplex 0.045205165589206765 0.21607199115505854; createComplex -0.0201958480970428 -0.13942823231310325 ]
                        [ createComplex -0.020195848097042807 -0.13942823231310336; createComplex 0.01407550760993468 0.06306021155774927; createComplex -0.9585857675082211 0.0015144028039448532; createComplex -0.009673150258669137 -0.12936024950038505 ]
                        [ createComplex 0.04520516558920681 0.21607199115505868; createComplex -0.0174036630196144 -0.13060623757618986; createComplex -0.03084636619918583 -0.3709728596674446; createComplex -0.9585857675082216 0.0015144028039448393 ]
                    ]
                    |> ComplexMatrix.create
            }

            {
                description = "One layer homegenious media, random incident light, random optical properties."
                thinFilms =
                    [
                        {
                            properties = 
                                {
                                    eps = 
                                        [
                                            [ createComplex 3.5021307590677586 0.004910796055146478; createComplex 0.1448188382704857 0.000834260445303435; createComplex 0.0421100441439744 -0.0008311798560446699 ]
                                            [ createComplex 0.1448188382704858 0.0008342604453034346; createComplex 4.395445093161517 0.004762172783970623; createComplex 0.01455357523209506 -0.00008366130786027271 ]
                                            [ createComplex 0.0421100441439744 -0.0008311798560446706; createComplex 0.01455357523209506 -0.0000836613078602726; createComplex 1.0673056268307946 0.005955215513141588 ]
                                        ]
                                        |> ComplexMatrix3x3.create
                                    mu = 
                                        [
                                            [ createComplex 0.9582047927795885 0.0; createComplex 0.002456702160523204 0.0; createComplex -0.023259102856946406 0.0 ]
                                            [ createComplex 0.002456702160523218 0.0; createComplex 1.0480733247992344 0.0; createComplex 0.012816054669739748 0.0 ]
                                            [ createComplex -0.02325910285694638 0.0; createComplex 0.012816054669739748 0.0; createComplex 1.0726398036824272 0.0 ]
                                        ]
                                        |> ComplexMatrix3x3.create
                                    rho = 
                                        [
                                            [ createComplex 0. -0.044424020098732225; createComplex 0. 0.020697425762887564; createComplex 0. 0.05026489175739626 ]
                                            [ createComplex 0. 0.020697425762887564; createComplex 0. -0.029357526291481178; createComplex 0. 0.07178570497243361 ]
                                            [ createComplex 0. 0.05026489175739626; createComplex 0. 0.07178570497243361; createComplex 0. 0.01603950250386385 ]
                                        ]
                                        |> ComplexMatrix3x3.create
                                }
                            thickness = Thickness.nm 168.0
                        }
                    ]
                em = 
                    {
                        wavelength = WaveLength.nm 504.0
                        refractionIndex = RefractionIndex.defaultValue
                        incidenceAngle = Angle.degree 41.0 |> IncidenceAngle
                        polarization = Angle.degree 64.0 |> Polarization
                        ellipticity = -0.42025305376355426 |> Ellipticity
                    }
                    |> EmField.create
                expected = 
                    [
                        [ createComplex -0.98816443746049 0.055880632349881706; createComplex 0.006127601130424354 0.005034180198318153; createComplex 0.08128797263166429 0.03840650088667767; createComplex 0.035631210452151146 -0.010682458054276114 ]
                        [ createComplex 0.03213856680463725 0.026358904038529737; createComplex -0.9901747181153763 0.05390938194829858; createComplex 0.14380921471417246 -0.07570770676695995; createComplex 0.10398185697252946 0.03570545194284099 ]
                        [ createComplex -0.03172579399246865 -0.034489837610064196; createComplex -0.036589100436663816 -0.037239617827756887; createComplex -0.5703176562171782 -0.013369118260331655; createComplex 0.015451305510010923 -0.39437272074873647 ]
                        [ createComplex -0.1479301284999799 -0.2112747395151823; createComplex -0.03231766219583817 -0.03485213054808696; createComplex 0.06380912203475694 -1.6456372276292885; createComplex -0.5860131981735811 -0.011814338101499983 ]
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

    [<Fact>]
    member this.berremanMatrixTest1 () = this.runTest (data.[1])

    [<Fact>]
    member this.berremanMatrixTest2 () = this.runTest (data.[2])

    [<Fact>]
    member this.berremanMatrixTest3 () = this.runTest (data.[3])

