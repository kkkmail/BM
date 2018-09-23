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

    /// Wavelength variable from 200 to 800 nm.
    let wavelength200to800Range = WaveLengthRange (Range<_>.create (WaveLength.nm 200.0) (WaveLength.nm 800.0))

    /// Wavelength variable from 500 to 700 nm.
    let wavelength500to700Range = WaveLengthRange (Range<_>.create (WaveLength.nm 500.0) (WaveLength.nm 700.0))

    /// Vacuum / standard transparent glass system with s polarized light falling at normal.
    let transpGlass600nmNormalLPs = 
        { incidentLightInfo = light600nmNormalLPs; opticalSystem = BaseOpticalSystem.transparentGlassSystem.fullSystem }

    /// Vacuum / standard transparent glass system with s polarized light falling at angleDegree.
    let transpGlass600nmInclindedLPs angleDegree = 
        { incidentLightInfo = light600nmInclinedDegreelLPs angleDegree; opticalSystem = BaseOpticalSystem.transparentGlassSystem.fullSystem }

    /// Vacuum / standard transparent glass film / vacuum system with s polarized light falling at normal.
    let transparentGassFilm600nmNormalLPs thickness = 
        { incidentLightInfo = light600nmNormalLPs; opticalSystem = (BaseOpticalSystem.transparentGlasslFilmSystem thickness).fullSystem }

    /// Vacuum / standard transparent glass film / vacuum system with s polarized light falling at angleDegree.
    let transparentGassFilm600nmInclindedLPs angleDegree thickness = 
        { incidentLightInfo = light600nmInclinedDegreelLPs angleDegree; opticalSystem =  (BaseOpticalSystem.transparentGlasslFilmSystem thickness).fullSystem }

    /// Vacuum / standard biaxial thin film / vacuum system with s polarized light falling at normal.
    let biaxialCrystalFilm600nmNormalLPs thickness = 
        { incidentLightInfo = light600nmNormalLPs; opticalSystem = (BaseOpticalSystem.biaxialCrystalFilmSystem thickness).fullSystem }

    /// Vacuum / standard biaxial thin film / vacuum system with s polarized light falling at angleDegree.
    let biaxialCrystalFilm600nmInclindedLPs angleDegree thickness = 
        { incidentLightInfo = light600nmInclinedDegreelLPs angleDegree; opticalSystem = (BaseOpticalSystem.biaxialCrystalFilmSystem thickness).fullSystem }
