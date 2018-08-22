module Media

open Geometry
open MaterialProperties


type Media = 
    | UpperSemiInfinite of OpticalProperties
    | LowerSemiInfinite of OpticalProperties
    | ThinFilm of OpticalProperties * double
    | ThickPlate of OpticalProperties * double


type OpticalSystem = 
    | Value of List<Media>

