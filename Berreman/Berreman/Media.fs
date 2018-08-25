namespace Berreman

module Media =

    open Geometry
    open MaterialProperties

    type Thickness = 
        double

    type Layer =
        {
            properties : OpticalProperties
            thickness : Thickness
        }

    type BaseOpticalSystem = 
        {
            upper : OpticalProperties
            thinFilm : List<Layer>
            lower : OpticalProperties
        }


    type OpticalSystem = 
        {
            upper : OpticalProperties
            thinFilm : List<Layer>
            thickPlate : Layer option
            lower : OpticalProperties
        }

