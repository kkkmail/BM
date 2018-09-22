printfn "Loading..."
//===========================================================
#load "References.fsx"
//===========================================================
open Berreman.FieldFunctions
open OpticalProperties.Standard
open Analytics.Charting
open Analytics.StandardSystems
//===========================================================
#time
let fn = [R; T]

plot transpGlass600nmNormalLPs incidenceAngleRange fn
plot (transpGlass600nmInclindedLPs 59.0) ellipticityRange fn
plot3D transpGlass600nmNormalLPs ellipticityRange incidenceAngleRange fn
#time

printfn "Completed."
