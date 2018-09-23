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


    type SolverParameters =
        {
            /// Number of reflections to use when there is a thick substrate plate.
            numberOfReflections : int
        }

        static member defaultValue = 
            {
                numberOfReflections = 5
            }


    type EmFieldInfo = 
        {
            emField : EmField
            m1 : BerremanMatrix
            m2 : BerremanMatrix
            waveLength : WaveLength
            n1SinFita : N1SinFita
        }


    type InputData =
        | InfoBased of IncidentLightInfo
        | EmFieldBased of EmFieldInfo


    type BaseOpticalSystemSolver private (system: BaseOpticalSystem, input : InputData) = 
        let i, m1, m2, waveLength, n1SinFita =
            match input with 
            | InfoBased info ->
                let bm = BerremanMatrix.create info.n1SinFita
                EmField.create (info, system.upper), bm system.upper, bm system.lower, info.waveLength, info.n1SinFita
            | EmFieldBased e -> e.emField, e.m1, e.m2, e.waveLength, e.n1SinFita

        let (BerremanMatrixPropagated p) = BerremanMatrixPropagated.propagate (system.films, i)
        let b1 = m1.eigenBasis ()
        let b2 = m2.eigenBasis ()

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
            |> ComplexVector4.create

        let eht = 
            [
                b2.down.e0.[0] * sol.[2] + b2.down.e1.[0] * sol.[3]
                b2.down.e0.[1] * sol.[2] + b2.down.e1.[1] * sol.[3]
                b2.down.e0.[2] * sol.[2] + b2.down.e1.[2] * sol.[3]
                b2.down.e0.[3] * sol.[2] + b2.down.e1.[3] * sol.[3]
            ]
            |> ComplexVector4.create

        let r = 
            {
                wavelength = waveLength
                n1SinFita = n1SinFita
                opticalProperties = system.upper
                eh = ehr |> BerremanFieldEH
            }.toEmField ()

        let t = 
            {
                wavelength = waveLength
                n1SinFita = n1SinFita
                opticalProperties = system.lower
                eh = eht |> BerremanFieldEH
            }.toEmField ()

        let ems = 
            {
                incident= i
                reflected = r
                transmitted = t
            }

        // None of that seems currently needed
        //member __.reflectedLight = r
        //member __.transmittedLight = t
        //member __.incidentLight = i
        //member __.eigenBasisFilm = evd
        //member __.eigenBasisUpper = b1
        //member __.eigenBasisLower = b2
        //member __.coeffTbl = coeffTblVal
        //member __.freeTbl = freeTblVal

        member __.cfm = cfmVal
        member __.emSys = ems

        new (system: BaseOpticalSystem, info : IncidentLightInfo) = BaseOpticalSystemSolver (system, InfoBased info)
        new (system: BaseOpticalSystem, emfInfo : EmFieldInfo) = BaseOpticalSystemSolver (system, EmFieldBased emfInfo)


    type Solution = 
        | Single of BaseOpticalSystemSolver
        | Multiple of List<BaseOpticalSystemSolver>


    /// First step is when we solve the base system where the upper semi-indefinite media is a substrate.
    /// Down step is the transmitted light from the first step OR _______ ...
    /// Up step is the light reflected from lower semi-infinite media going up toward thin films.
    /// We use BaseOpticalSystemSolver at each step.
    /// The steps go as follows: FirstStep, DownStep, UpStep, DownStep, UpStep, ...
    type SolutionStep =
        | FirstStep
        | DownStep
        | UpStep


    type StepData =
        {
            step : SolutionStep
            data : EmFieldInfo
        }

        //member this.nextStep =
        //    match this.step with
        //    | FirstStep -> DownStep
        //    | DownStep -> UpStep
        //    | UpStep -> DownStep

        member this.nextStepData (d : StepData) : StepData = 
            match this.step with
            | FirstStep -> failwith ""
            | DownStep -> failwith ""
            | UpStep -> failwith ""

        static member start (system: OpticalSystem) (info : IncidentLightInfo) : StepData = 
            failwith ""


    /// TODO kk:20180916 Implement in full.
    type OpticalSystemSolver (system: OpticalSystem, info : IncidentLightInfo, parameters : SolverParameters) = 
        //inherit BaseOpticalSystemSolver(system.baseSystem, info)

        let next step results =
            (step, results)

        let sol = 
            match system.substrate with
            | None -> BaseOpticalSystemSolver(system.baseSystem, info) |> Single
            | Some s -> 
                [ for i in 0..parameters.numberOfReflections -> i ]
                |> List.fold (fun (step, results : List<BaseOpticalSystemSolver>) _ -> next step results) (StepData.start system info, [])
                |> snd
                |> Multiple

        member __.solution = sol
