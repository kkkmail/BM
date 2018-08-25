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
//===========================================================
open Berreman.Constants
open Berreman.Fields
open Berreman.BerremanMatrix
open Berreman.Geometry
open Berreman.MaterialProperties
open Berreman.Media
open Berreman.Solvers
//===========================================================
let info = 
    {
        wavelength = 500.0 * nm
        refractionIndex = 1.0
        incidentAngle = 0.0 * degree |> IncidentAngle
        polarization = -90.0 * degree |> Polarization
        ellipticity = Ellipticity.defaultValue
    }


let light = EmField.create info

printfn "light = %A" light
