namespace Berreman

module MatrixExp = 

    open System.Numerics
    open MathNet.Numerics
    open MathNet.Numerics.LinearAlgebra
    open MathNetNumericsMath

    // Port of https://people.sc.fsu.edu/~jburkardt/c_src/matrix_exponential/matrix_exponential.c
    // No optimization was performed.
    // This code is distributed under the GNU LGPL license.
    type ComplexMatrix
    with
        static member identity (n : int) : ComplexMatrix = 
            diagonalMatrix n (cplx 1.0)

        member this.lInfinityNorm () : double =
            let (ComplexMatrix m) = this

            let rowSum (v : Vector<Complex>) = 
                v.ToArray()
                |> Array.fold (fun acc e -> acc + e.Norm()) 0.0

            let rows = [ for i in 0..m.RowCount - 1 -> m.Row(i) ]
            rows |> List.fold (fun acc v -> max acc (rowSum v)) 0.0

        static member addScaled (a : Complex) (ma : ComplexMatrix) (b : Complex) (mb : ComplexMatrix) = 
            a * ma + b * mb

        member this.matrixExp () : ComplexMatrix = 
            let (ComplexMatrix m) = this
            let q = 6
            let aNorm = this.lInfinityNorm ()
            let ee = int ((log aNorm) / (log 2.0)) + 1
            let s = max 0 (ee + 1)
            let t = 1.0 / (pown 2.0 s)
            let a2 = (cplx t) * this
            let one = cplx 1.0
            let x = a2
            let c = cplx 0.5
            let e = ComplexMatrix.identity m.RowCount
            let e1 = ComplexMatrix.addScaled one e c a2
            let d1 = ComplexMatrix.addScaled one e (-c) a2
            let p = true

            let rec update 
                (pp : bool)
                (kk : int) 
                (cc : Complex) 
                (dd : ComplexMatrix) 
                (ee : ComplexMatrix)
                (xx : ComplexMatrix) = 
                if kk <= q
                then 
                    let cn = cc * (cplx ((double (q - kk + 1)) / (double (kk * (2 * q - kk + 1)))))
                    let xn = a2 * xx
                    let en = ComplexMatrix.addScaled cn xn one ee

                    let dn = 
                        match pp with 
                        | true -> ComplexMatrix.addScaled cn xn one dd
                        | false -> ComplexMatrix.addScaled (-cn) xn one dd

                    update (not pp) (kk + 1) cn dn en xn
                else
                    ee

            let en = update p 2 c d1 e1 x
            let e1 = d1.inverse * en

            let rec mult (k : int) (res : ComplexMatrix) = 
                if k <= s
                then mult (k + 1) (res * e1)
                else res

            let retVal = mult 1 e1
            retVal

