namespace BerremanTests

open Berreman.Constants
open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.BerremanMatrix
open Berreman.Solvers
open Berreman.FieldFunctions

open Xunit
open Xunit.Abstractions

open MatrixComparison
open Berreman.Media
open OpticalProperties.Standard


type IncidentLightTestData =
    {
        description : string
        info : IncidentLightInfo
        expected : EmField
    }


type IncidentLightTests(output : ITestOutputHelper) =

    let data = 
        [
            (
                let incidenceAngle = Angle.degree 7.0 |> IncidenceAngle
                let waveLength = WaveLength.nm 600.0
                let n1SinFita = N1SinFita.create 1.0 incidenceAngle

                {
                    description = "Linear polarization, 0 degrees polarization angle."
                    info = 
                        {
                            wavelength = waveLength
                            refractionIndex = RefractionIndex.vacuum
                            incidenceAngle = incidenceAngle
                            polarization = Polarization.defaultValue
                            ellipticity = Ellipticity.defaultValue
                        }
                    expected = 
                            {
                                wavelength = waveLength
                                n1SinFita = n1SinFita
                                opticalProperties = OpticalProperties.vacuum
                                e = 
                                    [ 0.992546151641322; 0.; -0.12186934340514745 ]
                                    |> E.fromRe
                                h = 
                                    [ 0.; 0.9999999999999998; 0. ]
                                    |> H.fromRe
                            }
                }
            )

            (
                let incidenceAngle = Angle.degree 7.0 |> IncidenceAngle
                let waveLength = WaveLength.nm 600.0
                let n1SinFita = N1SinFita.create 1.0 incidenceAngle

                {
                    description = "Elliptic polarization (0.2), 0 degrees polarization angle."
                    info = 
                        {
                            wavelength = waveLength
                            refractionIndex = RefractionIndex.vacuum
                            incidenceAngle = incidenceAngle
                            polarization = Polarization.defaultValue
                            ellipticity = Ellipticity 0.2
                        }
                    expected = 
                            {
                                wavelength = waveLength
                                n1SinFita = n1SinFita
                                opticalProperties = OpticalProperties.vacuum
                                e = 
                                    [ createComplex 0.97327157603087 0.; createComplex 0. 0.19611613513818404; createComplex -0.11950272310222826 0. ]
                                    |> E.create
                                h = 
                                    [ createComplex 0. -0.194654315206174; createComplex 0.9805806756909199 0.; createComplex 0. 0.02390054462044566 ]
                                    |> H.create
                            }
                }
            )

            (
                let incidenceAngle = Angle.degree 7.0 |> IncidenceAngle
                let waveLength = WaveLength.nm 600.0
                let n1SinFita = N1SinFita.create 1.0 incidenceAngle
                let beta = Angle.degree 25.0

                {
                    description = "Linear polarization, 25 degrees polarization angle."
                    info = 
                        {
                            wavelength = waveLength
                            refractionIndex = RefractionIndex.vacuum
                            incidenceAngle = incidenceAngle
                            polarization = Polarization beta
                            ellipticity = Ellipticity.defaultValue
                        }
                    expected = 
                            {
                                wavelength = waveLength
                                n1SinFita = n1SinFita
                                opticalProperties = OpticalProperties.vacuum
                                e = 
                                    [ 0.8995523062257899; 0.42261826174069944; -0.11045113492912874 ]
                                    |> E.fromRe
                                h = 
                                    [ -0.4194681293040762; 0.9063077870366498; 0.0515042100693638 ]
                                    |> H.fromRe
                            }
                }
            )
        ]

    member __.runTest (d : IncidentLightTestData) = 
        output.WriteLine d.description

        let i = EmField.create(d.info, OpticalProperties.vacuum)

        verifyVectorEqualityE output "eI" i.e d.expected.e
        verifyVectorEqualityH output "hI" i.h d.expected.h

    [<Fact>]
    member this.incidentLightTest0 () = this.runTest (data.[0])

    [<Fact>]
    member this.incidentLightTest1 () = this.runTest (data.[1])

    [<Fact>]
    member this.incidentLightTest2 () = this.runTest (data.[2])

