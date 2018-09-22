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
        
        /// Standard trnasparent glass with refractive index 1.52.
        static member transparentGlass = 1.52 |> RefractionIndex.create |> OpticalProperties.fromRefractionIndex

        static member standardUniaxialCrystalEps = 0


    /// Standard vacuum / transparent glass system.
    let transparentGlassSystem = 
        {
            upper = OpticalProperties.vacuum
            films = []
            lower = OpticalProperties.transparentGlass
        }

    let private w600nm = 600.0

    let light600nmNormalLPs = WaveLength.nm w600nm |> IncidentLightInfo.create
    let light600nmInclinedDegreelLPs angleDegree = IncidentLightInfo.createInclined (WaveLength.nm w600nm) (Angle.degree angleDegree |> IncidenceAngle.create)
