namespace Berreman

module BerremanMatrix = 

    //open ExtremeNumericsMath

    open System.Numerics
    open MathNetNumericsMath

    open Geometry
    open Fields
    open MaterialProperties
    open Media

    // [ Ex, Hy, Ey, -Hx ]
    type BerremanField =
        {
            wavelength : WaveLength
            n1SinFita : N1SinFita
            eh : ComplexVector4
        }
        member this.eX = this.eh.[0]
        member this.hY = this.eh.[1]
        member this.eY = this.eh.[2]
        member this.hX = - this.eh.[3]

        static member create (info : IncidentLightInfo) (eh : ComplexVector4) = 
            {
                wavelength = info.wavelength
                n1SinFita = info.n1SinFita
                eh = eh
            }


    type BerremanMatrix = 
        | BerremanMatrix of ComplexMatrix4x4

        // Generated, do not modify.
        static member create (o : OpticalProperties) (N1SinFita nsf) =
            let n1SinFita = cplx nsf

            [
                [
                    (o.eps.[2, 2] * (o.mu.[2, 2] * o.rhoT.[1, 0] - o.mu.[1, 2] * o.rhoT.[2, 0]) + o.eps.[2, 0] * o.mu.[1, 2] * o.rhoT.[2, 2] - o.rho.[2, 2] * o.rhoT.[1, 0] * o.rhoT.[2, 2] - o.eps.[2, 0] * o.mu.[2, 2] * (o.rhoT.[1, 2] + n1SinFita) + o.rho.[2, 2] * o.rhoT.[2, 0] * (o.rhoT.[1, 2] + n1SinFita))/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                    (o.eps.[2, 2] * (-(o.mu.[1, 2] * o.mu.[2, 1]) + o.mu.[1, 1] * o.mu.[2, 2]) + o.mu.[2, 1] * o.rho.[2, 2] * o.rhoT.[1, 2] + o.mu.[1, 2] * o.rho.[2, 1] * o.rhoT.[2, 2] - o.mu.[1, 1] * o.rho.[2, 2] * o.rhoT.[2, 2] + o.mu.[2, 1] * o.rho.[2, 2] * n1SinFita + o.mu.[1, 2] * o.rhoT.[2, 2] * n1SinFita - o.mu.[2, 2] * (o.rho.[2, 1] + n1SinFita) * (o.rhoT.[1, 2] + n1SinFita))/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                    (o.eps.[2, 2] * o.mu.[2, 2] * o.rhoT.[1, 1] + o.eps.[2, 1] * o.mu.[1, 2] * o.rhoT.[2, 2] - o.rho.[2, 2] * o.rhoT.[1, 1] * o.rhoT.[2, 2] - o.eps.[2, 1] * o.mu.[2, 2] * (o.rhoT.[1, 2] + n1SinFita) + o.rho.[2, 2] * (o.rhoT.[2, 1] - n1SinFita) * (o.rhoT.[1, 2] + n1SinFita) + o.eps.[2, 2] * o.mu.[1, 2] * (-o.rhoT.[2, 1] + n1SinFita))/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                    (o.eps.[2, 2] * (o.mu.[1, 2] * o.mu.[2, 0] - o.mu.[1, 0] * o.mu.[2, 2]) - o.mu.[1, 2] * o.rho.[2, 0] * o.rhoT.[2, 2] + o.mu.[1, 0] * o.rho.[2, 2] * o.rhoT.[2, 2] + o.mu.[2, 2] * o.rho.[2, 0] * (o.rhoT.[1, 2] + n1SinFita) - o.mu.[2, 0] * o.rho.[2, 2] * (o.rhoT.[1, 2] + n1SinFita))/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                ]
                [
                    (-(o.eps.[0, 2] * o.eps.[2, 0] * o.mu.[2, 2]) + o.eps.[0, 0] * o.eps.[2, 2] * o.mu.[2, 2] - o.eps.[2, 2] * o.rho.[0, 2] * o.rhoT.[2, 0] + o.eps.[0, 2] * o.rho.[2, 2] * o.rhoT.[2, 0] + o.eps.[2, 0] * o.rho.[0, 2] * o.rhoT.[2, 2] - o.eps.[0, 0] * o.rho.[2, 2] * o.rhoT.[2, 2])/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                    (o.eps.[2, 2] * (o.mu.[2, 2] * o.rho.[0, 1] - o.mu.[2, 1] * o.rho.[0, 2]) + o.eps.[0, 2] * o.mu.[2, 1] * o.rho.[2, 2] - o.rho.[0, 1] * o.rho.[2, 2] * o.rhoT.[2, 2] - o.eps.[0, 2] * o.mu.[2, 2] * (o.rho.[2, 1] + n1SinFita) + o.rho.[0, 2] * o.rhoT.[2, 2] * (o.rho.[2, 1] + n1SinFita))/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                    (o.eps.[0, 1] * o.eps.[2, 2] * o.mu.[2, 2] - o.eps.[2, 2] * o.rho.[0, 2] * o.rhoT.[2, 1] + o.eps.[2, 1] * o.rho.[0, 2] * o.rhoT.[2, 2] - o.eps.[0, 1] * o.rho.[2, 2] * o.rhoT.[2, 2] + o.eps.[2, 2] * o.rho.[0, 2] * n1SinFita - o.eps.[0, 2] * (o.eps.[2, 1] * o.mu.[2, 2] + o.rho.[2, 2] * (-o.rhoT.[2, 1] + n1SinFita)))/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                    (-(o.eps.[2, 2] * o.mu.[2, 2] * o.rho.[0, 0]) + o.eps.[2, 2] * o.mu.[2, 0] * o.rho.[0, 2] + o.eps.[0, 2] * o.mu.[2, 2] * o.rho.[2, 0] - o.eps.[0, 2] * o.mu.[2, 0] * o.rho.[2, 2] - o.rho.[0, 2] * o.rho.[2, 0] * o.rhoT.[2, 2] + o.rho.[0, 0] * o.rho.[2, 2] * o.rhoT.[2, 2])/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                ]
                [
                    (-(o.eps.[2, 2] * o.mu.[2, 2] * o.rhoT.[0, 0]) + o.eps.[2, 0] * o.mu.[2, 2] * o.rhoT.[0, 2] + o.eps.[2, 2] * o.mu.[0, 2] * o.rhoT.[2, 0] - o.rho.[2, 2] * o.rhoT.[0, 2] * o.rhoT.[2, 0] - o.eps.[2, 0] * o.mu.[0, 2] * o.rhoT.[2, 2] + o.rho.[2, 2] * o.rhoT.[0, 0] * o.rhoT.[2, 2])/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                    -((o.eps.[2, 2] * (-(o.mu.[0, 2] * o.mu.[2, 1]) + o.mu.[0, 1] * o.mu.[2, 2]) + o.mu.[2, 1] * o.rho.[2, 2] * o.rhoT.[0, 2] + o.mu.[0, 2] * o.rho.[2, 1] * o.rhoT.[2, 2] - o.mu.[0, 1] * o.rho.[2, 2] * o.rhoT.[2, 2] + o.mu.[0, 2] * o.rhoT.[2, 2] * n1SinFita - o.mu.[2, 2] * o.rhoT.[0, 2] * (o.rho.[2, 1] + n1SinFita))/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2]))
                    (o.eps.[2, 1] * o.mu.[2, 2] * o.rhoT.[0, 2] - o.rho.[2, 2] * o.rhoT.[0, 2] * o.rhoT.[2, 1] - o.eps.[2, 1] * o.mu.[0, 2] * o.rhoT.[2, 2] + o.rho.[2, 2] * o.rhoT.[0, 1] * o.rhoT.[2, 2] + o.rho.[2, 2] * o.rhoT.[0, 2] * n1SinFita - o.eps.[2, 2] * (o.mu.[2, 2] * o.rhoT.[0, 1] + o.mu.[0, 2] * (-o.rhoT.[2, 1] + n1SinFita)))/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                    (-(o.eps.[2, 2] * o.mu.[0, 2] * o.mu.[2, 0]) + o.eps.[2, 2] * o.mu.[0, 0] * o.mu.[2, 2] - o.mu.[2, 2] * o.rho.[2, 0] * o.rhoT.[0, 2] + o.mu.[2, 0] * o.rho.[2, 2] * o.rhoT.[0, 2] + o.mu.[0, 2] * o.rho.[2, 0] * o.rhoT.[2, 2] - o.mu.[0, 0] * o.rho.[2, 2] * o.rhoT.[2, 2])/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                ]
                [
                    (o.eps.[1, 2] * (-(o.eps.[2, 0] * o.mu.[2, 2]) + o.rho.[2, 2] * o.rhoT.[2, 0]) + o.eps.[1, 0] * (o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2]) - (o.eps.[2, 2] * o.rhoT.[2, 0] - o.eps.[2, 0] * o.rhoT.[2, 2]) * (o.rho.[1, 2] - n1SinFita))/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                    (o.eps.[2, 2] * o.mu.[2, 2] * o.rho.[1, 1] + o.eps.[1, 2] * o.mu.[2, 1] * o.rho.[2, 2] - o.rho.[1, 1] * o.rho.[2, 2] * o.rhoT.[2, 2] + o.eps.[2, 2] * o.mu.[2, 1] * (-o.rho.[1, 2] + n1SinFita) - o.eps.[1, 2] * o.mu.[2, 2] * (o.rho.[2, 1] + n1SinFita) + o.rhoT.[2, 2] * (o.rho.[1, 2] - n1SinFita) * (o.rho.[2, 1] + n1SinFita))/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                    (o.eps.[1, 1] * (o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2]) + (o.rho.[1, 2] - n1SinFita) * (-(o.eps.[2, 2] * o.rhoT.[2, 1]) + o.eps.[2, 1] * o.rhoT.[2, 2] + o.eps.[2, 2] * n1SinFita) - o.eps.[1, 2] * (o.eps.[2, 1] * o.mu.[2, 2] + o.rho.[2, 2] * (-o.rhoT.[2, 1] + n1SinFita)))/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                    (o.eps.[1, 2] * o.mu.[2, 2] * o.rho.[2, 0] - o.eps.[1, 2] * o.mu.[2, 0] * o.rho.[2, 2] - o.rho.[1, 2] * o.rho.[2, 0] * o.rhoT.[2, 2] + o.rho.[1, 0] * o.rho.[2, 2] * o.rhoT.[2, 2] + o.rho.[2, 0] * o.rhoT.[2, 2] * n1SinFita - o.eps.[2, 2] * (o.mu.[2, 2] * o.rho.[1, 0] + o.mu.[2, 0] * (-o.rho.[1, 2] + n1SinFita)))/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
                ]
            ]
            |> ComplexMatrix4x4.create
            |> BerremanMatrix

        static member identity = ComplexMatrix4x4.identity |> BerremanMatrix

        // Generated, do not modify.
        static member createEmField (o : OpticalProperties) (emXY : EmFieldXY) = 
            let (N1SinFita nsf) = emXY.n1SinFita
            let n1SinFita = cplx nsf
            let eX = emXY.e.x
            let eY = emXY.e.y
            let hX = emXY.h.x
            let hY = emXY.h.y
            let eZ = ((-(o.eps.[2, 0] * o.mu.[2, 2]) + o.rho.[2, 2] * o.rhoT.[2, 0]) * eX - o.eps.[2, 1] * o.mu.[2, 2] * eY + o.rho.[2, 2] * o.rhoT.[2, 1] * eY - o.rho.[2, 2] * n1SinFita * eY - o.mu.[2, 2] * o.rho.[2, 0] * hX + o.mu.[2, 0] * o.rho.[2, 2] * hX + (o.mu.[2, 1] * o.rho.[2, 2] - o.mu.[2, 2] * (o.rho.[2, 1] + n1SinFita)) * hY)/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
            let hZ = ((-(o.eps.[2, 2] * o.rhoT.[2, 0]) + o.eps.[2, 0] * o.rhoT.[2, 2]) * eX - o.eps.[2, 2] * o.rhoT.[2, 1] * eY + o.eps.[2, 1] * o.rhoT.[2, 2] * eY + o.eps.[2, 2] * n1SinFita * eY - o.eps.[2, 2] * o.mu.[2, 0] * hX + o.rho.[2, 0] * o.rhoT.[2, 2] * hX + (-(o.eps.[2, 2] * o.mu.[2, 1]) + o.rhoT.[2, 2] * (o.rho.[2, 1] + n1SinFita)) * hY)/(o.eps.[2, 2] * o.mu.[2, 2] - o.rho.[2, 2] * o.rhoT.[2, 2])
            EmField.create (emXY, eZ, hZ)


    type BerremanMatrixPropagated = 
        | BerremanMatrixPropagated of ComplexMatrix4x4

        static member propagate (l : Layer, em : EmField) = 
            let (BerremanMatrix m) = BerremanMatrix.create l.properties em.n1SinFita
            let (WaveLength w) = em.wavelength
            
            match l.thickness with 
            | Thickness t -> m.matrixExp (Complex(0.0, (2.0 * pi * t / w))) |> BerremanMatrixPropagated
            | Infinity -> failwith "TODO Implelement infinite thickness by making that layer the output media."

        static member propagate (ls : List<Layer>, em : EmField) : BerremanMatrixPropagated = 
            ls |> List.fold (fun acc r -> (BerremanMatrixPropagated.propagate(r, em)) * acc) BerremanMatrixPropagated.identity

        static member identity = ComplexMatrix4x4.identity |> BerremanMatrixPropagated

        static member (*) (BerremanMatrixPropagated a, BerremanMatrixPropagated b) = 
            (a * b) |> BerremanMatrixPropagated


    type BerremanField
        with
        member this.toEmField (o : OpticalProperties) = 
            let emXY = 
                {
                    wavelength = this.wavelength
                    n1SinFita = this.n1SinFita
                    e = [ this.eX; this.eY ] |> ComplexVector.create |> ComplexVector2
                    h = [ this.hX; this.hY ] |> ComplexVector.create |> ComplexVector2
                } : EmFieldXY

            BerremanMatrix.createEmField o emXY


    type EmField
        with 
        member this.toBerremanField () : BerremanField = 
            {
                wavelength = this.wavelength
                n1SinFita = this.n1SinFita
                eh = [ this.e.x; this.h.y; this.e.y; -this.h.x ] |> ComplexVector.create |> ComplexVector4
            }


    type ComplexMatrix4x4 
        with 

        member this.eigenBasis (wavelength : WaveLength) (n1SinFita : N1SinFita) (o : OpticalProperties) : FullEigenBasis = 
            let (ComplexMatrix4x4 (ComplexMatrix m)) = this
            let evd = m.Evd()

            let normalize (v : #seq<Complex>) = 
                let norm = v |> Seq.fold (fun acc r -> acc + r.Real * r.Real + r.Imaginary * r.Imaginary) 0.0 |> sqrt |> cplx
                v |> Seq.map (fun e -> e / norm)

            let toBerremanField eh= 
                {
                    wavelength = wavelength
                    n1SinFita = n1SinFita
                    eh = eh
                }

            let ve =
                Array.zip (evd.EigenValues.ToArray()) (evd.EigenVectors.ToColumnArrays())
                |> List.ofArray
                |> List.map (fun (v, e) -> v, e |> normalize |> ComplexVector4.create)
                |> List.map (fun (v, e) -> v, e, ((e |> toBerremanField).toEmField o).s)
                |> List.sortBy (fun (_, _, s) -> s.z)
                |> List.map (fun (v, e, _) -> v, e)

            let up = ve |> List.take 2 |> EigenBasis.create
            let dn = ve |> List.rev |> List.take 2 |> List.rev |> EigenBasis.create

            {
                down = dn
                up = up
            }
