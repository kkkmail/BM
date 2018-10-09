printfn "Loading..."
//===========================================================
#load "References.fsx"
//===========================================================
open Berreman.FieldFunctions
open Berreman.MaterialProperties
open OpticalProperties.Standard
open Analytics.Charting
open Analytics.StandardSystems
open Analytics.Variables
open Berreman.Media
open Berreman.Fields
open Berreman.Dispersion
open OpticalProperties.Dispersive
//===========================================================
#time
let fn = [ R; T ]

//plot transpGlass600nmNormalLPs incidenceAngleRange fn
//plot (transpGlass600nmInclindedLPs 59.0) ellipticityRange fn
//plot3D transpGlass600nmNormalLPs ellipticityRange incidenceAngleRange fn

let thickness = Thickness.nm 200.

let thickness1 = Thickness.nm (600.0 / 1.52 / 4.0)
let thickness2 = Thickness.nm (600.0 / 1.00 / 4.0)

//plot (biaxialCrystalFilm600nmNormalLPs thickness) incidenceAngleRange fn
//plot (biaxialCrystalFilm600nmInclindedLPs 59.0 thickness) ellipticityRange fn
//plot3D (biaxialCrystalFilm600nmNormalLPs thickness) ellipticityRange incidenceAngleRange fn

//plot (transparentGassFilm600nmNormalLPs thickness) incidenceAngleRange fn
//plot (transparentGassFilm600nmInclindedLPs 59.0 thickness) polarizationRange fn
//plot3D (transparentGassFilm600nmNormalLPs thickness) polarizationRange incidenceAngleRange fn

// biaxialFilm600nmNormalLPs

let baseSystem = 
    {
        description = None
        upper = OpticalProperties.vacuum
        films = []
        lower = OpticalProperties.transparentGlass
    }

let filmSystem = 
    {
        description = None
        upper = OpticalProperties.vacuum
        films = 
            [ 
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                { properties = OpticalProperties.vacuum; thickness = thickness2 }
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
            ]
        lower = OpticalProperties.vacuum
    }

let vacuumSubstrate =
    {
        thickness = Thickness.nm 500.0
        properties = OpticalProperties.vacuum
    }

let transparentGlassSubstrate =
    {
        thickness = Thickness.nm 0.0
        properties = OpticalProperties.transparentGlass
    }

//let substrate = 
//    {
//        thickness = Thickness.nm 1000.0
//        properties = OpticalProperties.biaxialCrystal
//    }

let filmSystemWithSubstrate s = { filmSystem.fullSystem with substrate = Some s }

let f0 = { incidentLightInfo = light600nmNormalLPs; opticalSystem = baseSystem.fullSystem.dispersive }
let f1 = { f0 with opticalSystem = filmSystem.fullSystem.dispersive }
//let f2 = { f0 with opticalSystem = (filmSystemWithSubstrate vacuumSubstrate).dispersive }
//let f3 = { f0 with opticalSystem = (filmSystemWithSubstrate transparentGlassSubstrate).dispersive }

//plot f incidenceAngleRange fn

//plot f0 wavelength200to800Range fn
//plot f1 wavelength200to800Range fn
//plot f1 wavelength500to700Range fn
//plot f2 wavelength200to800Range fn
//plot f3 wavelength200to800Range fn

//plot3D f0 polarizationRange incidenceAngleRange fn
//plot3D f1 wavelength500to700Range incidenceAngleRange fn

let (WaveLengthRange w) = wavelength200to800Range
plotN11 (Silicon().opticalProperties) w
plotXi11 (Silicon().opticalProperties) w

#time

printfn "Completed."
