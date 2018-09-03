namespace OpticalProperties

open Berreman.Constants
open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.BerremanMatrix


// Standard optical properties without dispresion to be used in various simple calculations and tests.
// Values here must not be changed.
// If some other values are desired, introduce another module and set the new values there.
module Standard =

    type OpticalProperties
        with 
        static member transparentGlass = 1.52 |> RefractionIndex.create |> OpticalProperties.defaultValue
