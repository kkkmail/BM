﻿namespace Berreman
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


    type Eps = 
        | Eps of ComplexMatrix3x3

        static member create a = a |> ComplexMatrix3x3.create |> Eps
        static member fromRe a = a |> ComplexMatrix3x3.fromRe |> Eps
        static member vacuum = ComplexMatrix3x3.identity |> Eps

        member eps.Item
            with get(i, j) =
                let (Eps (ComplexMatrix3x3 v)) = eps
                v.[i, j]

        static member fromRefractionIndex (RefractionIndex n) = 
            (n * n |> cplx) * ComplexMatrix3x3.identity |> Eps

        static member fromRefractionIndex (RefractionIndex n1, RefractionIndex n2, RefractionIndex n3) = 
            [
                [ n1 * n1; 0.; 0. ]
                [ 0.; n2 * n2; 0. ]
                [ 0.; 0.; n3 * n3 ]
            ]
            |> Eps.fromRe

        member eps.re =
            let (Eps e) = eps
            e.re.toComplex() |> Eps


    type Mu = 
        | Mu of ComplexMatrix3x3

        static member (*) (ComplexVector3 a, Mu (ComplexMatrix3x3 b)) : ComplexVector3 = a * b |> ComplexVector3
        static member (*) (Mu (ComplexMatrix3x3 a), ComplexVector3 b) : ComplexVector3 = a * b |> ComplexVector3
        static member create a = a |> ComplexMatrix3x3.create |> Mu
        static member fromRe a = a |> ComplexMatrix3x3.fromRe |> Mu
        static member vacuum = ComplexMatrix3x3.identity |> Mu

        member this.Item
            with get(i, j) =
                let (Mu (ComplexMatrix3x3 v)) = this
                v.[i, j]


    type Rho = 
        | Rho of ComplexMatrix3x3

        static member (*) (ComplexVector3 a, Rho (ComplexMatrix3x3 b)) : ComplexVector3 = a * b |> ComplexVector3
        static member (*) (Rho (ComplexMatrix3x3 a), ComplexVector3 b) : ComplexVector3 = a * b |> ComplexVector3
        static member create a = a |> ComplexMatrix3x3.create |> Rho
        static member fromIm a = a |> ComplexMatrix3x3.fromIm |> Rho
        static member vacuum = ComplexMatrix3x3.zero |> Rho

        member this.Item
            with get(i, j) =
                let (Rho (ComplexMatrix3x3 v)) = this
                v.[i, j]


    type RhoT = 
        | RhoT of ComplexMatrix3x3

        static member (*) (ComplexVector3 a, RhoT (ComplexMatrix3x3 b)) : ComplexVector3 = a * b |> ComplexVector3
        static member (*) (RhoT (ComplexMatrix3x3 a), ComplexVector3 b) : ComplexVector3 = a * b |> ComplexVector3

        member this.Item
            with get(i, j) =
                let (RhoT (ComplexMatrix3x3 v)) = this
                v.[i, j]


    type OpticalProperties = 
        {
            eps : Eps
            mu : Mu
            rho : Rho
        }

        member this.rhoT : RhoT = 
            let (Rho (ComplexMatrix3x3 r)) = this.rho
            r.conjugateTranspose |> ComplexMatrix3x3 |> RhoT

        static member fromEpsion eps =
            {
                eps = eps
                mu = ComplexMatrix3x3.identity |> Mu
                rho = ComplexMatrix3x3.zero |> Rho
            }

        static member fromRefractionIndex n = Eps.fromRefractionIndex n |> OpticalProperties.fromEpsion
        static member fromRefractionIndex (n1, n2, n3) = Eps.fromRefractionIndex(n1, n2, n3) |> OpticalProperties.fromEpsion
        static member vacuum = Eps.vacuum |> OpticalProperties.fromEpsion

        member this.rotate (Rotation r) = 
            let c = r.toComplex()
            let cInv = c.inverse
            let rotate e = cInv * e * c

            {
                eps =
                    let (Eps a) = this.eps
                    a |> rotate |> Eps
                mu = 
                    let (Mu a) = this.mu
                    a |> rotate |> Mu
                rho = 
                    let (Rho a) = this.rho
                    a |> rotate |> Rho
            }

        member this.rotatePiX = this.rotate Rotation.rotatePiX
        member this.rotateX a = Rotation.rotateX a |> this.rotate
        member this.rotateY a = Rotation.rotateY a |> this.rotate
        member this.rotateZ a = Rotation.rotateZ a |> this.rotate
