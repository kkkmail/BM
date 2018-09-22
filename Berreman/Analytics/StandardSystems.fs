namespace Analytics

open FSharp.Collections.ParallelSeq
open System.Numerics
open MathNet.Numerics.LinearAlgebra
open Berreman.MathNetNumericsMath
open Berreman.MatrixExp

open Berreman.Constants
open Berreman.Fields
open Berreman.BerremanMatrix
open Berreman.Geometry
open Berreman.MaterialProperties
open Berreman.Media
open Berreman.Solvers
open Berreman.FieldFunctions
open OpticalProperties.Standard
open Berreman
open Variables

module StandardSystems = 
    /// Incident light variable from 0 to 89 degrees
    let incidenceAngleRange = IncidenceAngleRange (Range<_>.create IncidenceAngle.normal IncidenceAngle.maxValue)

    /// Ellipticity variable from 0 to 1.
    let ellipticityRange = EllipticityRange (Range<_>.create Ellipticity.defaultValue Ellipticity.maxValue)

    /// Polarization variable from 0 to 90 degrees.
    let polarizationRange = PolarizationRange (Range<_>.create Polarization.s Polarization.p)

    /// Vacuum / standard transparent glass system with s polarized light falling at normal.
    let transpGlass600nmNormalLPs = { incidentLightInfo = light600nmNormalLPs; opticalSystem = transparentGlassSystem.fullSystem }

    /// Vacuum / standard transparent glass system with s polarized light falling at angleDegree.
    let transpGlass600nmInclindedLPs angleDegree = { incidentLightInfo = light600nmInclinedDegreelLPs angleDegree; opticalSystem = transparentGlassSystem.fullSystem }

