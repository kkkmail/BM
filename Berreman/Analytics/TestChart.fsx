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
//===========================================================
#time
let fn = [R; T]

//plot transpGlass600nmNormalLPs incidenceAngleRange fn
//plot (transpGlass600nmInclindedLPs 59.0) ellipticityRange fn
//plot3D transpGlass600nmNormalLPs ellipticityRange incidenceAngleRange fn

let thickness = Thickness.nm 200.

let thickness1 = Thickness.nm (600.0 / 1.52 / 2.0)
let thickness2 = Thickness.nm (600.0 / 1.00 / 4.0)

//plot (biaxialCrystalFilm600nmNormalLPs thickness) incidenceAngleRange fn
//plot (biaxialCrystalFilm600nmInclindedLPs 59.0 thickness) ellipticityRange fn
//plot3D (biaxialCrystalFilm600nmNormalLPs thickness) ellipticityRange incidenceAngleRange fn

//plot (transparentGassFilm600nmNormalLPs thickness) incidenceAngleRange fn
//plot (transparentGassFilm600nmInclindedLPs 59.0 thickness) polarizationRange fn
//plot3D (transparentGassFilm600nmNormalLPs thickness) polarizationRange incidenceAngleRange fn

// biaxialFilm600nmNormalLPs

let system = 
    {
        description = None
        upper = OpticalProperties.vacuum
        films = 
            [ 
                { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                //{ properties = OpticalProperties.vacuum; thickness = thickness2 }
                //{ properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                //{ properties = OpticalProperties.vacuum; thickness = thickness2 }
                //{ properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                //{ properties = OpticalProperties.vacuum; thickness = thickness2 }
                //{ properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                //{ properties = OpticalProperties.vacuum; thickness = thickness2 }
                //{ properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                //{ properties = OpticalProperties.vacuum; thickness = thickness2 }
                //{ properties = OpticalProperties.transparentGlass; thickness = thickness1 }
            ]
        lower = OpticalProperties.vacuum
    }

let systemWithSubstrate s = { system.fullSystem with substrate = s }

let f = { incidentLightInfo = light600nmNormalLPs; opticalSystem = system.fullSystem }
//plot f incidenceAngleRange fn
plot f wavelength200to800Range fn
//plot3D f polarizationRange incidenceAngleRange fn
//plot3D f wavelength500to700Range incidenceAngleRange fn

let vacuumSubstrate = 
    {
        thickness = Thickness.nm 500.0
        properties = OpticalProperties.vacuum
    }
    |> Some

let f1 = { f with opticalSystem = systemWithSubstrate vacuumSubstrate}
plot f1 wavelength200to800Range fn

#time

printfn "Completed."
