(* La3Ga5SiO14 *)
(* 0.4 mkm < lambda < 1.0 mkm *)

mkm = 10^-6;

refrIndexSquared[lambda_, kCoeff_, lambdaNull_] := 1 + kCoeff * lambda^2 / (lambda^2 - lambdaNull^2);
sigmaAbsorption[lambdaMidPoint_, lambdaHalfWidth_] := (lambdaHalfWidth - lambdaMidPoint) / Log[2];
absorptionCoeff[lambda_, kAbsorption_, lambdaMidPoint_, lambdaHalfWidth_] :=
    kAbsorption * Exp[-(lambda - lambdaMidPoint)^2 / sigmaAbsorption[lambdaMidPoint, lambdaHalfWidth]^2];

refrIndex[lambda_, kCoeff_, lambdaNull_, kAbsorption_, lambdaMidPoint_, lambdaHalfWidth_] :=
    Sqrt[refrIndexSquared[lambda, kCoeff, lambdaNull]] + I * absorptionCoeff[lambda, kAbsorption, lambdaMidPoint, lambdaHalfWidth];

gyration11Func[lambda_, refrIndAverageFunc_, a2Coeff_, a3Coeff_, lambda2Coeff_] :=
    lambda * refrIndAverageFunc[lambda] * ((a2Coeff / (lambda^2 - lambda2Coeff^2)) + (a3Coeff * lambda^2 / (lambda^2 - lambda2Coeff^2)^2));

gyration33Func[lambda_, refrIndAverageFunc_, a1Coeff_, lambda1Coeff_] :=
    lambda * refrIndAverageFunc[lambda] * a1Coeff / (lambda^2 - lambda1Coeff^2);

refrIndex$La3Ga5SiO14$Ordinary[lambda_] :=
    refrIndex[lambda, 2.4981088, 0.12978841 mkm, 0.5 * 10^-4, 0.28 * mkm, 0.3 * mkm];

refrIndex$La3Ga5SiO14$ExtraOrdinary[lambda_] :=
    refrIndex[lambda, 2.5408145, 0.12914765 mkm, 1.0 * 10^-4, 0.28 * mkm, 0.3 * mkm];

refrIndex$La3Ga5SiO14$Average[lambda_] :=
    (Re[refrIndex$La3Ga5SiO14$Ordinary[lambda]] + Re[refrIndex$La3Ga5SiO14$ExtraOrdinary[lambda]]) / 2;

g11$La3Ga5SiO14[lambda_] :=
    gyration11Func[lambda, refrIndex$La3Ga5SiO14$Average, 0.6106 * 10^-11, 0.6278 * 10^-11, 0.156 mkm];

g33$La3Ga5SiO14[lambda_] :=
    gyration33Func[lambda, refrIndex$La3Ga5SiO14$Average, 0.6072 * 10^-11, 0.198 mkm];

Print[Plot[Re[refrIndex$La3Ga5SiO14$ExtraOrdinary[lmb mkm]] - Re[refrIndex$La3Ga5SiO14$Ordinary[lmb mkm]], {lmb, 0.25, 0.60}]];
Print[Plot[{Re[refrIndex$La3Ga5SiO14$ExtraOrdinary[lmb mkm]], Re[refrIndex$La3Ga5SiO14$Ordinary[lmb mkm]]}, {lmb, 0.25, 0.60}]];
Print[Plot[{10^5 * g11$La3Ga5SiO14[lmb mkm], 10^5 * g33$La3Ga5SiO14[lmb mkm]}, {lmb, 0.25, 0.60}]];
