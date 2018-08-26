namespace Berreman
module MaterialProperties = 

    open Geometry
    open System.Numerics
    open MathNet.Numerics.ComplexExtensions
    open MathNet.Numerics.LinearAlgebra


    type OpticalProperties = 
        {
            eps : ComplexMatrix3x3
            mu : ComplexMatrix3x3
            rho : ComplexMatrix3x3
        }
        //inherit ComplexMatrix3x3 ()
        ////| A of Matrix<Complex>(3, 3)
        ////with
        ////static member create e = 
        ////    0
        //member this.rotate (rotation : Rotation) : OpticalTensor = 
        //    failwith ""

        member this.rhoT : ComplexMatrix3x3 = 
            let (ComplexMatrix3x3 r) = this.rho
            r.conjugateTranspose |> ComplexMatrix3x3

        static member defaultValue (n : double) = 
            {
                eps = (n * n |> cplx) * ComplexMatrix3x3.identity
                mu = ComplexMatrix3x3.identity
                rho = ComplexMatrix3x3.zero
            }


