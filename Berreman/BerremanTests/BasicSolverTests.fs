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


type BaseOpticalSystemTestData =
    {
        description : string
        opticalSystem : BaseOpticalSystem
        info : IncidentLightInfo
        expected : EmFieldSystem
        stokes : StokesSystem option
    }


type ResultComparisionType = 
    | Field
    | Intensity


type BasicSolverTests(output : ITestOutputHelper) =

    let addLayer (l : Layer) (d : BaseOpticalSystemTestData) = 
        { d with opticalSystem = { d.opticalSystem with films = l :: d.opticalSystem.films } }


    let rec addLayers (ls : List<Layer>) (d : BaseOpticalSystemTestData) = 
        match (ls |> List.rev) with 
        | [] -> d
        | h :: t -> addLayers (t |> List.rev) (addLayer h d)


    let stdGlassLayer = 
        {
            properties = OpticalProperties.transparentGlass
            thickness = Thickness.nm 100.0
        }


    let vacuumLayer = 
        {
            properties = OpticalProperties.vacuum
            thickness = Thickness.nm 150.0
        }


    let createStdGlassLightAt7Degrees description = 
        let opticalProperties = OpticalProperties.transparentGlass
        let incidenceAngle = Angle.degree 7.0 |> IncidenceAngle
        let waveLength = WaveLength.nm 600.0
        let n1SinFita = N1SinFita.create 1.0 incidenceAngle

        {
            description = description
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
                    refractionIndex = RefractionIndex.vacuum
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
                            opticalProperties = OpticalProperties.vacuum
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
                            opticalProperties = OpticalProperties.vacuum
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
                            opticalProperties = opticalProperties
                            e = 
                                [ 0.7897587002999794; 0.; -0.06352515217049573; ]
                                |> ComplexVector3.fromRe
                            h = 
                                [ 0.; 1.2043103497061607; 0. ]
                                |> ComplexVector3.fromRe
                        }
                }

            stokes = 
                {
                    incidentStokes = [ 1.; 1.; 0.; 0. ] |> StokesVector.create
                    reflectedStokes = [ 0.0417427189970538; 0.0417427189970538; 0.; 0. ] |> StokesVector.create
                    transmittedStokes  = [ 0.6277542496577975; 0.6277542496577975; 0.; 0. ] |> StokesVector.create
                } |> Some
        }


    let data = 
        [
            createStdGlassLightAt7Degrees "Snell's law for standard transparent glass, 7 degrees incidence angle."

            createStdGlassLightAt7Degrees "Snell's law for thin standard glass film on standard transparent glass, 7 degrees incidence angle."
            |> addLayer stdGlassLayer

            createStdGlassLightAt7Degrees "Snell's law for vacuum film on standard transparent glass, 7 degrees incidence angle."
            |> addLayer vacuumLayer

            createStdGlassLightAt7Degrees "Snell's law for vacuum film + thin standard glass film on standard transparent glass, 7 degrees incidence angle."
            |> addLayer stdGlassLayer
            |> addLayer vacuumLayer

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


    member __.runTest (d : BaseOpticalSystemTestData) (c : ResultComparisionType) = 
        output.WriteLine d.description
        let solver = BaseOpticalSystemSolver (d.opticalSystem, d.info)

        //output.WriteLine("eigenBasisUpper = {0}\n", solver.eigenBasisUpper)
        //output.WriteLine("eigenBasisFilm = {0}\n", solver.eigenBasisFilm)
        //output.WriteLine("eigenBasisLower = {0}\n", solver.eigenBasisLower)
        //output.WriteLine("coeffTbl = {0}\n", solver.coeffTbl)
        //output.WriteLine("freeTbl = {0}\n", solver.freeTbl)
        //output.WriteLine("cfm = {0}\n", solver.cfm)

        output.WriteLine("stokesVector (I) = {0}\n", solver.incidentLight.stokesVector)
        output.WriteLine("stokesVector (R) = {0}\n", solver.reflectedLight.stokesVector)
        output.WriteLine("stokesVector (T) = {0}\n", solver.transmittedLight.stokesVector)

        let eI = solver.incidentLight.e
        let hI = solver.incidentLight.h

        let eR = solver.reflectedLight.e
        let hR = solver.reflectedLight.h

        let eT = solver.transmittedLight.e
        let hT = solver.transmittedLight.h

        match c with 
        | Field -> 
            verifyVectorEquality output "eI" eI d.expected.incident.e
            verifyVectorEquality output "hI" hI d.expected.incident.h

            verifyVectorEquality output "eR" eR d.expected.reflected.e
            verifyVectorEquality output "hR" hR d.expected.reflected.h

            verifyVectorEquality output "eT" eT d.expected.transmitted.e
            verifyVectorEquality output "hT" hT d.expected.transmitted.h
        | Intensity ->
            ()


    [<Fact>]
    member this.basicSolverTest0 () = this.runTest (data.[0]) Field

    [<Fact>]
    member this.basicSolverTest1 () = this.runTest (data.[1]) Intensity

    [<Fact>]
    member this.basicSolverTest2 () = this.runTest (data.[2]) Intensity

    [<Fact>]
    member this.basicSolverTest3 () = this.runTest (data.[3]) Intensity
