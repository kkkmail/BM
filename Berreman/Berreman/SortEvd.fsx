//===========================================================
#I __SOURCE_DIRECTORY__
#r "System.Core.dll"
#r "System.dll"
#r "System.Numerics.dll"
//===========================================================
#r "../packages/MathNet.Numerics.4.5.1/lib/net461/MathNet.Numerics.dll"
#r "../packages/MathNet.Numerics.FSharp.4.5.1/lib/net45/MathNet.Numerics.FSharp.dll"
#r "../packages/System.ValueTuple.4.5.0/lib/net47/System.ValueTuple.dll"
//===========================================================
#r "./bin/Debug/Berreman.dll"
#r "../OpticalProperties./bin/Debug/OpticalProperties.dll"

open System.Numerics
open MathNet.Numerics.LinearAlgebra
open Berreman.MathNetNumericsMath
open Berreman.MatrixExp

open Berreman.Constants
open Berreman.Fields
open Berreman.BerremanMatrix
open Berreman.Geometry
open Berreman.MaterialProperties
open Berreman.Media
open Berreman.Solvers
open OpticalProperties.Standard
open Berreman

//===========================================================
type BaseOpticalSystemTestData =
    {
        description : string
        opticalSystem : BaseOpticalSystem
        light : IncidentLightInfo
        expected : EmFieldSystem
    }


let data = 
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
                                    cplx 0.0
                                ]
                                |> ComplexVector3.create
                            h = 
                                [
                                    cplx 0.0
                                ]
                                |> ComplexVector3.create
                        }
                    reflected = 
                        {
                            wavelength = waveLength
                            n1SinFita = n1SinFita
                            e = 
                                [
                                    cplx 0.0
                                ]
                                |> ComplexVector3.create
                            h = 
                                [
                                    cplx 0.0
                                ]
                                |> ComplexVector3.create
                        }
                    transmitted = 
                        {
                            wavelength = waveLength
                            n1SinFita = n1SinFita
                            e = 
                                [
                                    cplx 0.0
                                ]
                                |> ComplexVector3.create
                            h = 
                                [
                                    cplx 0.0
                                ]
                                |> ComplexVector3.create
                        }
                }
        }

    create OpticalProperties.transparentGlass IncidenceAngle.normal (WaveLength.nm 600.0)


let (BerremanMatrix bm) = BerremanMatrix.create data.opticalSystem.lower data.light.n1SinFita
printfn "bm = %A" bm

let (ComplexMatrix4x4 (ComplexMatrix m0)) = bm

let evd0 = m0.Evd()
printfn "evd0 = %A\n" evd0
printfn "EigenValues = %A\n" evd0.EigenValues
printfn "EigenVectors = %A\n" evd0.EigenVectors
printfn "EigenVectors.Inv = %A\n" (evd0.EigenVectors.Inverse())

let evd = bm.eigenBasis data.light.wavelength data.light.n1SinFita
let up = evd.up
let down = evd.down
printfn "up = %A\n" up
printfn "down = %A\n" down

//let evdEm = 
//    evd
//    |> List.map (fun e -> e.value, (e.vector |> BerremanField.create data.light).toEmField data.opticalSystem.lower)
//printfn "evdEm = %A" evdEm

//let evdS = 
//    evdEm
//    |> List.map (fun (v, em) -> v, em.s)
//printfn "evdS = %A" evdS

//let sorted = 
//    evdS
//    |> List.sortBy (fun (_, s) -> s.z)
//printfn "sorted = %A" sorted

//let up = sorted |> List.take 2
//let dn = sorted |> List.rev |> List.take 2 |> List.rev

//printfn "up = %A" up
//printfn "dn = %A" dn

//let m = 
//    [
//        [ 0.; 1.; 0.; 0. ]
//        [ 2.3104; 0.; 0.; 0. ]
//        [ 0.; 0.; 0.; 1.; ]
//        [ 0.; 0.; 2.3104; 0. ]
//    ]
//    |> matrix

//let diagonalMatrix (v : Vector<Complex>) = 
//    let len = v.Count
//    let a = v.ToArray()

//    [ for i in 1..v.Count -> [for j in 1..v.Count -> if i = j then a.[i - 1].Real else 0.0 ] ]
//    |> matrix

//let evdReal = m.Evd()
//let d = evdReal.EigenValues |> diagonalMatrix
//let v = evdReal.EigenVectors
//printfn "evdReal = %A" evdReal
//printfn "values = %A" (evdReal.EigenValues)
//printfn "vectors = %A" (evdReal.EigenVectors)
//printfn "vectorsInv = %A" (evdReal.EigenVectors.Inverse())
//let result = v * d * v.Inverse()
//printfn "result = %A" result
//printfn "m - result = %A" (m - result)
