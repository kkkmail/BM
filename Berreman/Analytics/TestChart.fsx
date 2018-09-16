printfn "Loading..."
//===========================================================
//===========================================================
#I __SOURCE_DIRECTORY__
#r "System.Core.dll"
#r "System.dll"
#r "System.Numerics.dll"
//===========================================================
#r "./bin/Debug/MathNet.Numerics.dll"
#r "./bin/Debug/MathNet.Numerics.FSharp.dll"
#r "../packages/System.ValueTuple.4.5.0/lib/net47/System.ValueTuple.dll"
//#r "../packages/FSharp.Charting.2.1.0/lib/net45/FSharp.Charting.dll"
//===========================================================
#r "./bin/Debug/Berreman.dll"
#r "./bin/Debug/Analytics.dll"
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
open Analytics.Charting

//===========================================================
//plot ()
plot3D ()
printfn "Completed."
