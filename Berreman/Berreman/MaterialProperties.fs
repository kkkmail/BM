namespace Berreman
module MaterialProperties = 

    //open ExtremeNumericsMath

    open System.Numerics
    open MathNetNumericsMath

    open Geometry

    // Covers only real refraction indices.
    type RefractionIndex = 
        RefractionIndex of double
        with 
        static member create n = RefractionIndex n
        static member vacuum = RefractionIndex.create 1.0


    type OpticalProperties = 
        {
            eps : ComplexMatrix3x3
            mu : ComplexMatrix3x3
            rho : ComplexMatrix3x3
        }

        member this.rhoT : ComplexMatrix3x3 = 
            let (ComplexMatrix3x3 r) = this.rho
            r.conjugateTranspose |> ComplexMatrix3x3

        static member defaultValue (RefractionIndex n) = 
            {
                eps = (n * n |> cplx) * ComplexMatrix3x3.identity
                mu = ComplexMatrix3x3.identity
                rho = ComplexMatrix3x3.zero
            }

        static member vacuum = RefractionIndex.vacuum |> OpticalProperties.defaultValue

        member this.rotate (Rotation r) = 
            let c = r.toComplex()
            let cInv = c.inverse
            let rotate e = cInv * e * c

            {
                eps = this.eps |> rotate
                mu = this.mu |> rotate
                rho = this.rho |> rotate
            }
