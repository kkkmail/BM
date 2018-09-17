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

        member this.fullSystem = 
            {
                upper = this.upper
                films = this.films
                substrate = None
                lower = this.lower
            }


    and OpticalSystem = 
        {
            upper : OpticalProperties
            films : List<Layer>
            substrate : Layer option
            lower : OpticalProperties
        }

        member this.baseSystem = 
            match this.substrate with
            | None -> 
                {
                    upper = this.upper
                    films = this.films
                    lower = this.lower
                }
            | Some s -> 
                {
                    upper = this.upper
                    films = this.films
                    lower = s.properties
                }

