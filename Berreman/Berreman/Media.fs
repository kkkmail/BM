namespace Berreman

module Media =

    open MaterialProperties


    type Thickness = 
        | Thickness of double
        | Infinity
        with
        static member nm t = t * Constants.nm |> Thickness
        static member mkm t = t * Constants.mkm |> Thickness
        static member mm t = t * Constants.mm |> Thickness
        member __.toInfinity () = Thickness.Infinity


    type Layer =
        {
            properties : OpticalProperties
            thickness : Thickness
        }


    type BaseOpticalSystem = 
        {
            upper : OpticalProperties
            films : List<Layer>
            lower : OpticalProperties
        }


    type OpticalSystem = 
        {
            upper : OpticalProperties
            films : List<Layer>
            substrate : Layer option
            lower : OpticalProperties
        }
