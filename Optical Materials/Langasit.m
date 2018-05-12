(* La3Ga5SiO14 *)
(* 0.4 mkm < lambda < 1.0 mkm *)

mkm = 10^-6;

KO = 2.4981088;
KE = 2.5408145;

lambdaO = 0.12978841 mkm;
lambdaE = 0.12914765 mkm;

n2Ofunc[lambda_?NumericQ] := 1 + KO * lambda^2 / (lambda^2 - lambdaO^2);
n2Efunc[lambda_?NumericQ] := 1 + KE * lambda^2 / (lambda^2 - lambdaE^2);

1 + KO * (0.2 mkm)^2 / ((0.2 mkm)^2 - lambdaO^2)

lmb0O = 0.28 * mkm;
lmb0E = 0.28 * mkm;

lmbO = 0.3 * mkm;
lmbE = 0.3 * mkm;

sigmaO = (lmbO - lmb0O) / Log[2];
sigmaE = (lmbE - lmb0E) / Log[2];

kO = 0.5 * 10^-4;
kE = 1.0 * 10^-4;

kOfunc[lambda_] := kO * Exp[-(lambda - lmb0O)^2 / sigmaO^2];
kEfunc[lambda_] := kE * Exp[-(lambda - lmb0E)^2 / sigmaE^2];

Print[Plot[Sqrt[n2Efunc[lmb mkm]] - Sqrt[n2Ofunc[lmb mkm]], {lmb, 0.20, 0.60}]];
Print[Plot[{Sqrt[n2Efunc[lmb mkm]], Sqrt[n2Ofunc[lmb mkm]]}, {lmb, 0.20, 0.60}]];



