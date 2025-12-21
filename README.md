# BM - Berreman 4x4 Matrix Method in Mathematica

A sophisticated optical physics simulation framework implementing the **Berreman Matrix method** for analyzing polarized light behavior in anisotropic and optically active media.

**Version:** 6.03.001
**Author:** Konstantin K. Konstantinov
**Email:** konstantin.k.konstantinov@gmail.com
**License:** GPL v3 or later
**Mathematica Version:** 11.2+

## Overview

This framework provides a complete solution for:
- Simulating light propagation through multilayered thin films and crystals
- Calculating Mueller matrices for polarization transformation
- Analyzing reflection, transmission, and absorption characteristics
- Handling complex electromagnetic phenomena including optical rotation and circular dichroism
- Solving forward problems (given materials, predict light behavior) and inverse problems (given experimental data, determine material properties)

## Architecture

The codebase comprises 11 modules (~7,200 lines of code) organized as follows:

```
BerremanInit.m (Entry Point)
    │
    ├── BerremanCommon.m (1,632 lines) - Shared Data Structures
    │       ├── RotationNew[], Transform[]
    │       ├── FilmLayerNew[], MediaNew[]
    │       ├── IncidentLightNew[], EpsilonFromN[]
    │
    ├── BerremanDirect.m (1,852 lines) - Forward Problem Solver
    │       ├── TransformMedia[] - applies rotations to media
    │       ├── Matrix propagation algorithms
    │       └── Eigenvalue/eigenvector calculations
    │
    ├── FieldAlgebra.m (1,490 lines) - Field Transformations
    │       ├── RFull[], TFull[], IFull[] - reflection/transmission intensities
    │       ├── Ellipticity calculations
    │       ├── Poynting vector operations
    │       └── Field component extraction
    │
    ├── BerremanInverse.m (637 lines) - Inverse Problem Solver
    │       ├── MFHlpSetEpsilonLimits[] - parameter constraints
    │       └── Minimization framework
    │
    ├── OpticalModel.m (419 lines) - Model Composition Framework
    ├── OpticalElements.m (395 lines) - Mueller Matrix Elements
    ├── OpticalModelFunctions.m (64 lines) - Stokes Vector Operations
    ├── OpticalDispersion.m (118 lines) - Material Dispersion Properties
    ├── FieldIO.m (466 lines) - Input/Output Operations
    └── FSharpHelpers.m (164 lines) - F# Code Generation Utilities
```

## Core Data Structures

### FilmLayer (9-element list)
```mathematica
{Thickness, Epsilon_current, Mu_current, Ro_current, RoT_current,
 Epsilon_base, Mu_base, Ro_base, RoT_base}
```
Represents a single optical layer with rotation-dependent and base properties. Stores permittivity (epsilon), permeability (mu), and gyration (rho) tensors.

### Media (8-component structure)
```mathematica
{n1, n2, gamma, Film, Description, nOut, h2_thickness,
 {eps_upper, mu_upper, ro_upper, roT_upper,
  eps_lower, mu_lower, ro_lower, roT_lower}}
```
Models multilayered system with upper/lower semi-infinite media.

### IncidentLight (7-element vector)
```mathematica
{lambda, fita, beta, EH_field, ellipticity, n1, Amplitude}
```
- `lambda`: wavelength
- `fita`: angle of incidence
- `beta`: azimuth angle
- `EH_field`: complex electric/magnetic field components (6-element vector)
- `ellipticity`: polarization ellipticity (-1 to +1)

### FullSol (Solution object)
Comprehensive result containing media, incident light, M1/M2 Berreman matrices, reflected/transmitted light fields, Mueller matrices, and eigenvalues/eigenvectors.

## Key Modules

### BerremanCommon.m - Fundamental Operations
- **Data Generators:** `FilmLayerNew[]`, `FilmAddLayer[]`, `MediaNew[]`, `IncidentLightNew[]`
- **Transformations:** `RotationNew[fi, theta, psi]`, `Transform[tensor, rotation]`, `EpsilonFromN[n1, n2, n3]`
- **Utilities:** `PsiAngle[fita, n1, n2]` (Snell's law), `M[epsilon, mu, ro, rotr]` (6x6 Berreman matrix)

### BerremanDirect.m - Forward Optical Calculations
- `TransformMedia[media, varlist, opts]` - applies wavelength-dependent rotations
- Matrix eigenvalue problem solving
- Field propagation through layered systems
- Averaging operations for periodic structures

### FieldAlgebra.m - Field & Polarization Analysis
- **Intensity:** `RFull[]`, `TFull[]`, `IFull[]`, `Rx[]`, `Ry[]`, `Tx[]`, `Ty[]`
- **Polarization:** `Xit[]`, `Xir[]` (azimuth), `Elt[]`, `Elr[]` (ellipticity)
- **Poynting Vector:** `PoyntingI[]`, `PoyntingR[]`, `PoyntingT[]`
- **Mueller/Stokes:** `MuellerMatrixR`, `StokesVectorR[]`, `StokesVectorT[]`

### BerremanInverse.m - Parameter Fitting
- `MFHlpSetParLimits[par, minVal, maxVal]` - parameter bounds
- `MFHlpSetEpsilonLimits[epsilon, opts]` - permittivity constraints
- Support for transparent and absorbing materials

### OpticalElements.m - Mueller Matrix Building Blocks
- `CreatePolarizer[px, py]` - polarizer elements
- `CreatePolarizerTrig[p, gamma]` - trigonometric polarizer
- `CreateRetarader[theta, delta]` - retarder (waveplate)
- `CreateRotator[theta]` - optical rotator
- `CreateSample[psi, delta]` - sample element

### OpticalDispersion.m - Material Properties
**Dispersion Functions:**
- `refrIndexSquared[lambda, kCoeff, lambdaNull]` - Lorentz oscillator model
- `gyration11Func[]`, `gyration33Func[]` - optical activity coefficients

**Built-in Materials:**
- **La₃Ga₅SiO₁₄** (Langasite): `eps$La3Ga5SiO14[lambda]`, `rho$La3Ga5SiO14[lambda]`
- **Silicon (Si):** `n$Si[lambda]`, `eps$Si[lambda]`

## The Berreman Matrix Method

The framework implements the 4x4 Berreman matrix method:

1. **Construct M-matrix** for each layer using epsilon, mu, and rho tensors
2. **Apply rotations** via Euler angles: M_rot = R^T · M · R
3. **Propagate waves** through eigenvalue decomposition and boundary matching
4. **Calculate observables:** reflection/transmission, Mueller matrices, polarization states, Poynting vectors

## Quick Start

```mathematica
(* Initialize the framework *)
BaseDir = "C:\\GitHub\\BM\\";
PathList = {BaseDir <> "Kernel\\"};
Get["BerremanInit.m", Path -> PathList];
InitializeBM[PathList, useParallelTbl];

(* Create incident light *)
incidentLight = CreateIncidentRay[nUpper, lambda, fita, beta, ellipt];

(* Create layers and system *)
layer1 = CreateFilm[thickness, rotationAngles, eps];
lowerMedia = CreateSemiInfiniteMediaFromN[nLower];
layeredSystem = CreateLayeredSystem[incidentLight, gamma, layer1, lowerMedia];

(* Perform calculations *)
allCalc = PerformAllCalculations[layeredSystem, FuncList, description, opts];
```

## Physics Capabilities

**Optical Phenomena:**
- Birefringence (ordinary vs. extraordinary rays)
- Dichroism (differential absorption)
- Optical rotation (circular birefringence)
- Ellipsometry
- Multilayer thin film interference

**Material Types:**
- Isotropic media
- Anisotropic crystals (arbitrary orientations)
- Absorbing materials (complex refractive indices)
- Gyrotropic/optically active crystals
- Magneto-optic materials

## Documentation

Main documentation is available in:
- `Doc/BM_Documentation_404_001.docx` - Technical reference
- `References/` - Academic papers and material property notebooks

---

## Examples

The `Examples/` folder contains a comprehensive set of demonstrations organized by complexity and functionality. All examples follow a consistent initialization pattern and build progressively in complexity.

### Getting Started

All examples begin with:
```mathematica
BaseDir = "C:\\GitHub\\BM\\";
PathList = {BaseDir <> "Kernel\\"};
Get["BerremanInit.m", Path -> PathList];
InitializeBM[PathList, useParallelTbl];
```

### Snell's Law Examples (Basic Optics)

| Example | File | Description |
|---------|------|-------------|
| 01 | `BM_603_Example_01_Snell_Law.m` | Basic refraction at vacuum/glass (n=1.52) interface |
| 01 Au | `BM_603_Example_01_Snell_Law__Au.m` | Reflection from gold (n=0.35+2.45i) - absorbing metals |
| 02 | `BM_603_Example_02_Snell_Law_For_Thick_Plate.m` | Thick isotropic glass plate (1mm) |
| 03 | `BM_603_Example_03_Snell_Law_For_Thick_Absorbing_Plate.m` | Absorbing plate with n=1.5+3×10⁻⁵i |
| 03a | `BM_603_Example_03a_Snell_Law_For_Thick_Anisotropic_Plate.m` | Biaxial plate with optical activity |
| 04 | `BM_603_Example_04_Snell_Law_For_Anisotropic_Media.m` | Semi-infinite anisotropic medium with full tensor |
| 04a | `BM_603_Example_04a_Snell_Law_For_Anisotropic_Media_v2.m` | Refined with specific material data |
| 04r | `BM_603_Example_04r_Snell_Law_For_RandomAnisotropic_Media.m` | Random tensor generation for testing |
| 05 | `BM_603_Example_05_Snell_Law_For_Thick_Anisotropic_Plate.m` | 10 μm birefringent crystal plate |

### Thin Film Examples

| Example | File | Description |
|---------|------|-------------|
| 00 | `BM_603_Example_00_MuellerMatrix.m` | Mueller matrix and Stokes vector demonstration |
| 06 | `BM_603_Example_06_1L_Thin_Film_Simple.m` | Single isotropic film (75nm, n=1.52) on glass |
| 06a | `BM_603_Example_06a_1L_Thin_Film.m` | Biaxial thin film (n₁=1.50, n₂=2.00, n₃=1.75) |
| 06b | `BM_603_Example_06b_1L_Thin_Film_OptActivity.m` | Film with optical activity (gyration tensor) |
| 06c | `BM_603_Example_06c_1L_Thin_Film__BN.m` | Boron nitride film with wavelength-dependent properties |
| 06d | `BM_603_Example_06d_1L_Thin_Film_GlassBetweenVacuum.m` | Symmetric system: vacuum-glass-vacuum |
| 06r | `BM_603_Example_06r_1L_RandomThin_Film.m` | Random single-layer film for testing |
| 07 | `BM_603_Example_07_1L_Thin_Film_On_Absorbing_Substrate_Plate.m` | Film on lossy substrate |
| 08 | `BM_603_Example_08_2L_Thin_Film_on_Substrate_Plate.m` | Two-layer stack with absorption |
| 08a | `BM_603_Example_08a_2L_Thin_Film.m` | Alternative two-layer configuration |
| 08r | `BM_603_Example_08r_2L_RandomThin_Film.m` | Random two-layer system |
| 09 | `BM_603_Example_09_1L_Thin_Film_on_Substrate_Plate_Complex_Graph.m` | Wavelength-dependent dispersion calculations |

### Inverse Problem Examples

| Example | File | Description |
|---------|------|-------------|
| Inv 000 | `BM_603_Inv_000_Direct.m` | Baseline calculation for inverse validation |
| Inv 001 | `BM_603_Inv_001_Direct.m` | Two-layer system on absorbing substrate |
| Inv 003 | `BM_405_Inv_003_Direct.m` | Legacy API inverse problem example |

### Key Example Patterns

**Creating Isotropic Materials:**
```mathematica
eps = EpsilonFromN[1.52];  (* Single refractive index *)
```

**Creating Biaxial Materials:**
```mathematica
eps = EpsilonFromN[1.50, 2.00, 1.75];  (* Three principal indices *)
```

**Creating Absorbing Materials:**
```mathematica
eps = EpsilonFromN[1.5 + I * 0.00003];  (* Complex index with loss *)
```

**Adding Optical Activity:**
```mathematica
rho = I * {{1.9*10^-3, -3.5*10^-3, 0}, {3.5*10^-3, 1.9*10^-3, 0}, {0, 0, 3.8*10^-3}};
```

**Parameter Specification Format:**
```mathematica
{minVal, maxVal, stepSize, "label", unit}
(* Example: {0, 85, 5, "θ", Degree} for angle sweep *)
```

### Output Calculation Hierarchy

- **Level 1 - Basic:** `Rx`, `Ry` (reflectance components)
- **Level 2 - Transmission:** `Tx`, `Ty` (transmittance components)
- **Level 3 - Stokes:** `StokesVectorR[1-4]`, `StokesVectorT[1-4]`, `StokesVectorI[1-4]`
- **Level 4 - Ellipsometry:** `Psi` (amplitude), `Delta` (phase), `Elr` (ellipsometry ratio)
- **Level 5 - Polarimetry:** `Chi` (helicity), `Gamma` (degree of polarization)

### Learning Path

**Beginner:**
1. Example 01 - Understand basic Snell's law
2. Example 06 - First thin film calculation
3. Example 02 - Thick plate effects

**Intermediate:**
4. Example 03a - Anisotropic effects
5. Example 04 - Complex tensor materials
6. Example 08 - Multi-layer stacks

**Advanced:**
7. Example 09 - Wavelength-dependent dispersion
8. Example 04r - Random media validation
9. Inverse Problem examples - Parameter fitting

### Notebook Versions

Interactive Mathematica notebooks (`.nb` files) for many examples are available in `Examples/NB/`, providing a more visual experience with embedded plots and step-by-step execution.

---

## References

- Academic papers in `References/` folder
- Langasite material properties: `References/Langasite_001.nb`
