namespace Berreman

module Media =

    open MaterialProperties
    open Fields
    open Geometry


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

        member this.rotate (r : Rotation) : Layer = 
            failwith ""

        member this.rotatePiX = this.rotate Rotation.rotatePiX


    /// Use when upper system is coming with incident light EmField
    type ShortOpticalSystem = 
        {
            films : List<Layer>
            lower : OpticalProperties
        }


    type BaseOpticalSystem = 
        {
            description : string option
            upper : OpticalProperties
            films : List<Layer>
            lower : OpticalProperties
        }

        member this.fullSystem = 
            {
                description = this.description
                upper = this.upper
                films = this.films
                substrate = None
                lower = this.lower
            }


    and OpticalSystem = 
        {
            description : string option
            upper : OpticalProperties
            films : List<Layer>
            substrate : Layer option
            lower : OpticalProperties
        }

        member this.baseSystem = 
            match this.substrate with
            | None -> 
                {
                    description = this.description
                    upper = this.upper
                    films = this.films
                    lower = this.lower
                }
            | Some s -> 
                {
                    description = this.description
                    upper = this.upper
                    films = this.films
                    lower = s.properties
                }

        member this.rotate (Rotation r) : OpticalSystem = 
            failwith ""

        member this.rotatePiX = this.rotate Rotation.rotatePiX
