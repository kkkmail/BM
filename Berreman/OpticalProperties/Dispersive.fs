namespace OpticalProperties

open System.Numerics
open MathNet.Numerics
open Berreman.Constants
open Berreman.MathNetNumericsMath
open Berreman.Geometry
open Berreman.Media
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Dispersion
open Berreman.BerremanMatrix

module Dispersive =
    let refrIndexSquared (WaveLength lambda) kCoeff (WaveLength lambdaNull) =
         1.0 + kCoeff * lambda * lambda / (lambda * lambda - lambdaNull * lambdaNull)


    let sigmaAbsorption (WaveLength lambdaMidPoint) (WaveLength lambdaHalfWidth) = (lambdaHalfWidth - lambdaMidPoint) / (log 2.0);


    let absorptionCoeff (WaveLength lambda) kAbsorption lambdaMidPoint lambdaHalfWidth =
        let (WaveLength lmbMid) = lambdaMidPoint
        kAbsorption * exp (-(lambda - lmbMid) ** 2.0 / (sigmaAbsorption lambdaMidPoint lambdaHalfWidth) ** 2.0)


    let refrIndex lambda kCoeff lambdaNull kAbsorption lambdaMidPoint lambdaHalfWidth =
        (refrIndexSquared lambda kCoeff lambdaNull |> sqrt, absorptionCoeff lambda kAbsorption lambdaMidPoint lambdaHalfWidth)
        |> Complex


    let gyration11Func (WaveLength lambda) refrIndAverageFunc a2Coeff a3Coeff (WaveLength lambda2Coeff) =
        (0.0, (lambda * (refrIndAverageFunc (WaveLength lambda)) * ((a2Coeff / (lambda ** 2.0 - lambda2Coeff * 2.0)) + (a3Coeff * lambda ** 2.0 / (lambda * 2.0 - lambda2Coeff ** 2.0) ** 2.0))))
        |> Complex


    let gyration33Func (WaveLength lambda) refrIndAverageFunc a1Coeff (WaveLength lambda1Coeff) =
        (0.0, (lambda * (refrIndAverageFunc (WaveLength lambda)) * a1Coeff / (lambda ** 2.0 - lambda1Coeff ** 2.0)))
        |> Complex


    [<AbstractClass>]
    type DispersiveMaterial() = 
        abstract member opticalProperties : OpticalPropertiesWithDisp


    /// La3Ga5SiO14
    type Langasite () =
        inherit DispersiveMaterial()

        /// La3Ga5SiO14, extraordinary referaction index,
        /// 0.4 mkm < lambda < 1.0 mkm.
        let refrIndexLa3Ga5SiO14Ordinary lambda =
            refrIndex lambda 2.4981088 (WaveLength.mkm 0.12978841) 0.5e-4 (WaveLength.mkm 0.28) (WaveLength.mkm 0.3)


        /// La3Ga5SiO14, ordinary refreaction index,
        /// 0.4 mkm < lambda < 1.0 mkm.
        let refrIndexLa3Ga5SiO14ExtraOrdinary lambda =
            refrIndex lambda 2.5408145 (WaveLength.mkm 0.12914765) 1.0e-4 (WaveLength.mkm 0.28) (WaveLength.mkm 0.3)


        /// La3Ga5SiO14, average (real) refraction index.
        let refrIndexLa3Ga5SiO14Average lambda = 
            ((refrIndexLa3Ga5SiO14Ordinary lambda) + (refrIndexLa3Ga5SiO14ExtraOrdinary lambda)).Real / 2.0


        /// La3Ga5SiO14, g11.
        let g11La3Ga5SiO14 lambda =
            gyration11Func lambda refrIndexLa3Ga5SiO14Average 0.6106e-11 0.6278e-11 (WaveLength.mkm 0.156)


        /// La3Ga5SiO14, g33.
        let g33La3Ga5SiO14 lambda =
            gyration33Func lambda refrIndexLa3Ga5SiO14Average 0.6072e-11 (WaveLength.mkm 0.198)


        let epsLa3Ga5SiO14 lambda =
            let nVal1 = refrIndexLa3Ga5SiO14Ordinary lambda |> ComplexRefractionIndex
            let nVal2 = refrIndexLa3Ga5SiO14ExtraOrdinary lambda |> ComplexRefractionIndex
            let nVal3 = refrIndexLa3Ga5SiO14Ordinary lambda |> ComplexRefractionIndex
            (nVal1, nVal2, nVal3) |> Eps.fromComplexRefractionIndex


        let rhoLa3Ga5SiO14 lambda =
            [| g11La3Ga5SiO14 lambda; complexZero; g33La3Ga5SiO14 lambda |]
            |> complexFromDiagonal
            |> ComplexMatrix
            |> ComplexMatrix3x3
            |> Rho


        override __.opticalProperties=
            {
                epsWithDisp = epsLa3Ga5SiO14 |> EpsWithDisp
                muWithDisp = Mu.vacuum.dispersive
                rhoWithDisp = rhoLa3Ga5SiO14 |> RhoWithDisp
            }

//(* ============================================== *)
//(* ============================================== *)

    type Silicon () =
        inherit DispersiveMaterial()

        let mkmVal = 1.0e-6

        /// Refraction index.
        let lSi lambda = 1.0 / ((lambda / mkmVal) ** 2.0 - 0.028)

        let aSi = 3.41696
        let bSi = 0.138497
        let c1Si = 0.013924
        let d1Si = -0.0000209
        let e1Si = 0.000000148

        let nSi (WaveLength lambda) = aSi + bSi * (lSi lambda) + c1Si * (lSi lambda) ** 2.0 + d1Si * (lambda / mkmVal) ** 2.0 + e1Si * (lambda / mkmVal) ** 4.0;

        /// Absorption Coefficient.

        let lambda1Si = WaveLength.mkm 0.3757
        let kappa1Si = 1.32
        let lambda2Si = WaveLength.mkm 0.589
        let kappa2Si = 0.030104

        let xiSi lambda ko lambda0 eps = ko * (lambda / mkmVal) ** 2.0 / (eps + ((lambda / mkmVal) ** 2.0 - (lambda0 / mkmVal) ** 2.0) * 2.0)

//sol$Si =
//    Solve[
//      {
//        xi$Si[lambda1$Si, Ko$Si, lambda0$Si, eps$Si] == kappa1$Si,
//        xi$Si[lambda2$Si, Ko$Si, lambda0$Si, eps$Si] == kappa2$Si
//      },
//      {Ko$Si, lambda0$Si}
//    ];

//xi$Si[lambda_, epsValue_] := ((xi$Si[lambda, Ko$Si, lambda0$Si, eps$Si] /. sol$Si[[2]]) /. {eps$Si -> epsValue});
//xi$Si[lambda_] := xi$Si[lambda, 10^-4];

//refrIndex$Si[lambda_] := n$Si[lambda] + I * xi$Si[lambda];

//eps$Si[lambda_] :=
//    Module[{epsRet},
//      epsRet = EpsilonFromN[refrIndex$Si[lambda]];
//      Return[N[epsRet]];
//    ];

        override __.opticalProperties=
            {
                epsWithDisp = failwith "" // epsLa3Ga5SiO14 |> EpsWithDisp
                muWithDisp = Mu.vacuum.dispersive
                rhoWithDisp = failwith "" // rhoLa3Ga5SiO14 |> RhoWithDisp
            }

