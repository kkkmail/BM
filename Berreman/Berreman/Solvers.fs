namespace Berreman

module Solvers = 
    //open ExtremeNumericsMath

    open System.Numerics
    open MathNetNumericsMath
    open MatrixExp

    open Geometry
    open Fields
    open Media
    open BerremanMatrix


    type BaseOpticalSystemSolver (system: BaseOpticalSystem, info : IncidentLightInfo) = 
        let fixB1 (b : FullEigenBasis) = 
            {
                down = 
                    {
                        v0 = b.down.v1
                        v1 = b.down.v0
                        e0 = b.down.e1
                        e1 = b.down.e0
                    }
                up = 
                    {
                        v0 = b.up.v1
                        v1 = b.up.v0
                        e0 = b.up.e1
                        e1 = b.up.e0
                    }
            }

        let fixB2 (b : FullEigenBasis) = 
            {
                down = 
                    {
                        v0 = b.down.v1
                        v1 = b.down.v0
                        e0 = b.down.e1
                        e1 = b.down.e0
                    }
                up = 
                    {
                        v0 = b.up.v0
                        v1 = b.up.v1
                        e0 = b.up.e0 * (-1.0 |> cplx)
                        e1 = b.up.e1
                    }
            }
            
        let i : EmField = info |> EmField.create
        let (BerremanMatrix m1) = BerremanMatrix.create system.upper info.n1SinFita
        let (BerremanMatrix m2) = BerremanMatrix.create system.lower info.n1SinFita
        let (BerremanMatrixPropagated p) = BerremanMatrixPropagated.propagate (system.films, i)
        let evd = p.eigenBasis i.wavelength i.n1SinFita
        //let (b1, b2)= sortEvd evd

        // eigenBasis (wavelength : WaveLength) (n1SinFita : N1SinFita)
        let b1 = m1.eigenBasis i.wavelength i.n1SinFita // |> fixB1
        let b2 = m2.eigenBasis i.wavelength i.n1SinFita // |> fixB2

        // Generated, do not modify.
        let coeffTblVal = 
            [
                [
                    b1.up.e0.[0] * p.[0, 0] + b1.up.e0.[1] * p.[0, 1] + b1.up.e0.[2] * p.[0, 2] + b1.up.e0.[3] * p.[0, 3]
                    b1.up.e1.[0] * p.[0, 0] + b1.up.e1.[1] * p.[0, 1] + b1.up.e1.[2] * p.[0, 2] + b1.up.e1.[3] * p.[0, 3]
                    -b2.down.e0.[0]
                    -b2.down.e1.[0]
                ]
                [
                    b1.up.e0.[0] * p.[1, 0] + b1.up.e0.[1] * p.[1, 1] + b1.up.e0.[2] * p.[1, 2] + b1.up.e0.[3] * p.[1, 3]
                    b1.up.e1.[0] * p.[1, 0] + b1.up.e1.[1] * p.[1, 1] + b1.up.e1.[2] * p.[1, 2] + b1.up.e1.[3] * p.[1, 3]
                    -b2.down.e0.[1]
                    -b2.down.e1.[1]
                ]
                [
                    b1.up.e0.[0] * p.[2, 0] + b1.up.e0.[1] * p.[2, 1] + b1.up.e0.[2] * p.[2, 2] + b1.up.e0.[3] * p.[2, 3]
                    b1.up.e1.[0] * p.[2, 0] + b1.up.e1.[1] * p.[2, 1] + b1.up.e1.[2] * p.[2, 2] + b1.up.e1.[3] * p.[2, 3]
                    -b2.down.e0.[2]
                    -b2.down.e1.[2]
                ]
                [
                    b1.up.e0.[0] * p.[3, 0] + b1.up.e0.[1] * p.[3, 1] + b1.up.e0.[2] * p.[3, 2] + b1.up.e0.[3] * p.[3, 3]
                    b1.up.e1.[0] * p.[3, 0] + b1.up.e1.[1] * p.[3, 1] + b1.up.e1.[2] * p.[3, 2] + b1.up.e1.[3] * p.[3, 3]
                    -b2.down.e0.[3]
                    -b2.down.e1.[3]
                ]
            ]
            |> ComplexMatrix.create

        let cfmVal = coeffTblVal.inverse

        let freeTblVal = 
            [
                ((b1.down.e1.[2] * i.e.x - b1.down.e1.[0] * i.e.y) * (b1.down.e0.[1] * p.[0, 1] + b1.down.e0.[3] * p.[0, 3]) - b1.down.e0.[2] * (b1.down.e1.[0] * i.e.x * p.[0, 0] + b1.down.e1.[1] * i.e.x * p.[0, 1] + b1.down.e1.[0] * i.e.y * p.[0, 2] + b1.down.e1.[3] * i.e.x * p.[0, 3]) + b1.down.e0.[0] * (b1.down.e1.[2] * i.e.x * p.[0, 0] + b1.down.e1.[1] * i.e.y * p.[0, 1] + b1.down.e1.[2] * i.e.y * p.[0, 2] + b1.down.e1.[3] * i.e.y * p.[0, 3]))/(b1.down.e0.[2] * b1.down.e1.[0] - b1.down.e0.[0] * b1.down.e1.[2])
                ((b1.down.e1.[2] * i.e.x - b1.down.e1.[0] * i.e.y) * (b1.down.e0.[1] * p.[1, 1] + b1.down.e0.[3] * p.[1, 3]) - b1.down.e0.[2] * (b1.down.e1.[0] * i.e.x * p.[1, 0] + b1.down.e1.[1] * i.e.x * p.[1, 1] + b1.down.e1.[0] * i.e.y * p.[1, 2] + b1.down.e1.[3] * i.e.x * p.[1, 3]) + b1.down.e0.[0] * (b1.down.e1.[2] * i.e.x * p.[1, 0] + b1.down.e1.[1] * i.e.y * p.[1, 1] + b1.down.e1.[2] * i.e.y * p.[1, 2] + b1.down.e1.[3] * i.e.y * p.[1, 3]))/(b1.down.e0.[2] * b1.down.e1.[0] - b1.down.e0.[0] * b1.down.e1.[2])
                ((b1.down.e1.[2] * i.e.x - b1.down.e1.[0] * i.e.y) * (b1.down.e0.[1] * p.[2, 1] + b1.down.e0.[3] * p.[2, 3]) - b1.down.e0.[2] * (b1.down.e1.[0] * i.e.x * p.[2, 0] + b1.down.e1.[1] * i.e.x * p.[2, 1] + b1.down.e1.[0] * i.e.y * p.[2, 2] + b1.down.e1.[3] * i.e.x * p.[2, 3]) + b1.down.e0.[0] * (b1.down.e1.[2] * i.e.x * p.[2, 0] + b1.down.e1.[1] * i.e.y * p.[2, 1] + b1.down.e1.[2] * i.e.y * p.[2, 2] + b1.down.e1.[3] * i.e.y * p.[2, 3]))/(b1.down.e0.[2] * b1.down.e1.[0] - b1.down.e0.[0] * b1.down.e1.[2])
                ((b1.down.e1.[2] * i.e.x - b1.down.e1.[0] * i.e.y) * (b1.down.e0.[1] * p.[3, 1] + b1.down.e0.[3] * p.[3, 3]) - b1.down.e0.[2] * (b1.down.e1.[0] * i.e.x * p.[3, 0] + b1.down.e1.[1] * i.e.x * p.[3, 1] + b1.down.e1.[0] * i.e.y * p.[3, 2] + b1.down.e1.[3] * i.e.x * p.[3, 3]) + b1.down.e0.[0] * (b1.down.e1.[2] * i.e.x * p.[3, 0] + b1.down.e1.[1] * i.e.y * p.[3, 1] + b1.down.e1.[2] * i.e.y * p.[3, 2] + b1.down.e1.[3] * i.e.y * p.[3, 3]))/(b1.down.e0.[2] * b1.down.e1.[0] - b1.down.e0.[0] * b1.down.e1.[2])
            ]
            |> ComplexVector.create

        let sol = cfmVal * freeTblVal

        let ehr = 
            [
                b1.up.e0.[0] * sol.[0] + b1.up.e1.[0] * sol.[1]
                b1.up.e0.[1] * sol.[0] + b1.up.e1.[1] * sol.[1]
                b1.up.e0.[2] * sol.[0] + b1.up.e1.[2] * sol.[1]
                b1.up.e0.[3] * sol.[0] + b1.up.e1.[3] * sol.[1]
            ]
            |> ComplexVector.create
            |> ComplexVector4

        let eht = 
            [
                b2.down.e0.[0] * sol.[2] + b2.down.e1.[0] * sol.[3]
                b2.down.e0.[1] * sol.[2] + b2.down.e1.[1] * sol.[3]
                b2.down.e0.[2] * sol.[2] + b2.down.e1.[2] * sol.[3]
                b2.down.e0.[3] * sol.[2] + b2.down.e1.[3] * sol.[3]
            ]
            |> ComplexVector.create
            |> ComplexVector4

        let r = 
            {
                wavelength = info.wavelength
                n1SinFita = info.n1SinFita
                eh = ehr
            }.toEmField system.upper

        let t = 
            {
                wavelength = info.wavelength
                n1SinFita = info.n1SinFita
                eh = eht
            }.toEmField system.lower

        member __.reflectedLight = r
        member __.transmittedLight = t
        member __.incidentLight = i
        member __.eigenBasisFilm = evd
        member __.eigenBasisUpper = b1
        member __.eigenBasisLower = b2
        member __.coeffTbl = coeffTblVal
        member __.freeTbl = freeTblVal
        member __.cfm = cfmVal
