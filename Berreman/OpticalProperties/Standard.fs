namespace OpticalProperties

open Berreman.Constants
open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Media
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.BerremanMatrix


// !!! DO NOT CHANGE ANY VALUES HERE !!!
// Standard optical properties without dispresion to be used in various simple calculations and tests.
// If some other values are desired, introduce another module and set the new values there.
module Standard =

    type OpticalProperties
        with 
        static member transparentGlass = 1.52 |> RefractionIndex.create |> OpticalProperties.defaultValue


    /// Standard vacuum / transparent glass system.
    let transparentGlassSystem = 
        {
            upper = OpticalProperties.vacuum
            films = []
            lower = OpticalProperties.transparentGlass
        }

    let private wl600nm = 600.0

    let light600nmNormalLPs = WaveLength.nm wl600nm |> IncidentLightInfo.create
    let light600nmInclinedDegreelLPs angleDegree = IncidentLightInfo.createInclined (WaveLength.nm wl600nm) (Angle.degree angleDegree |> IncidenceAngle.create)
