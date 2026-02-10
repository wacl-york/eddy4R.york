# eddy4R.york

## Description 
eddy4R.york is an R package that extends [eddy4R](https://github.com/NEONScience/eddy4R) by providing features and helpers that are used to build the WACL-York flux workflows. 

The major feature allows for flux workflows to be defined by relatively short configuration scripts, making it simple to iterate on the parameters used, while maintaining reproducibility. 

### Some other features include: 
- Flexible aggregation periods - `def.avg()` determines what files are relevant to the user defined aggregation period so inputs can be stored in any time bins e.g. if files are organised into 30 min files, but the user wishes to to calculate fluxes hourly, the correct files will be loaded for this to occur.

- Support for double rotation - `wrap.rot()` implements double rotation and wraps it with the existing planar fit capability. Additionally allows for sector or time based changes to planar fit coefficients.

- Options to restrict time lag ranges via `wrap.lag()`.

- Error catching and logging 

### Getting Started

> [!NOTE]
> As eddy4R is provided as a docker image and as such eddy4R.york provides updated image - currently based on the [eddy4R:maps](https://quay.io/repository/battelleecology/eddy4r) image, but this will change to the stable annual releases when available.

#### Installation

The docker image can be installed using:

```
docker pull ghcr.io/wacl-york/eddy4r.york:dev
```

> [!NOTE]
> This example is for the development version of the container, which should be considered 'unstable'. Packages with a tagged version number will be released in the future, or by request to be able to be included alongside publications.

#### Run the container

To run the container as an interactive session run the following:

```
docker run --name eddy4r.york --rm -d -p 8787:8787 -v <extDir>:/home/rstudio/data ghcr.io/wacl-york/eddy4r.york:dev
```
Where `<extDir>` is the path to the volume you wish to mount on your machine



#### Configuring Run Parameters

`eddy4R.york` uses a parameters list created using `def.para()`. This is the main input to `wrap.towr()` and defines all of the settings of the calculation. Creating this configuration looks like:

```R
para = eddy4R.york::def.para(
  DirWrk = "/home/rstudio/data/",
  DirInp = "in/",
  siteName = "MySite",
  analysis = "nitrate_fluxes",
  runID = "standard_run",
  fileMask = "input_%y%m%d_%H%M.csv",
  species = c("NO","NO2"),
  aggregationPeriod = 3600,
  writeFastData = TRUE,
  AlgBase = "trnd",
  idepVar = "unixTime",
  MethRot = "double",
  missingMethod = "mean",
  lagApplyCorrection = TRUE,
  lagApplyRangeLimit = TRUE,
  lagRangeLimit = list(c(0,0),
                      c(0,0),
                      c(-2,-8),
                      c(-4,-10)),
  lagDefaults = c(0,0,-6, -8))

```

This has set up a run to calculate hourly fluxes of CO<sub>2</sub> and O<sub>3</sub> as well as sensible and latent heat. It will use using linear detrending, double rotation and perform a lag correction of all the scalars relative to the vertical wind. If the determined lag falls out of the ranges supplied per scalar, a fixed lag will be used instead.

#### Input Data
Input files contain the following columns. They should all be read by `read.csv()` as numeric. By default the listed units are expected, but this can be changed by using `def.para(unitList = ...)`

| Column Header    | Description                                   | Unit                                                  |
|------------------|-----------------------------------------------|-------------------------------------------------------| 
| unixTime         | seconds since midnight 1970-01-01             | s                                                     |
| veloXaxs         | x component of the 3D wind*                   | m s<sup>-1</sup>                                      |
| veloYaxs         | y component of the 3D wind*                   | m s<sup>-1</sup>                                      |
| veloZaxs         | z component of the 3D wind*                   | m s<sup>-1</sup>                                      |
| tempAir          | air temperature                               | Kelvin                                                |
| presAtm          | atmospheric pressure                          | Pa                                                    |
| distZaxsAbl      | Atmospheric Boundary Layer Height             | m                                                     |
| distZaxsMeas     | measurement height above surface              | m                                                     |
| rtioMoleDryH2o   | dry mole water vapour concentration           | mol<sub>H2O</sub> mol<sub>DryAir</sub><sup>-1</sup>   |
| rtioMoleDry{Spc} | dry mole concentration of other species "spc" | mol<sub>{spc}</sub> mol<sub>DryAir</sub><sup>-1</sup> |

*x/y/z == u/v/w == east/north/up

#### Overview of `wrap.towr()` 

`wrap.towr()` is the function that contains all possible steps of the workflow which are configured .by `def.para()`. This consists of some initial steps, then a loop around all of the flux aggregation periods that have been defined:

- Determine aggregation periods - `def.avg()`
- For each aggregation period:
    1. Read input data - `read_input()`
    2. Validity Checks - `def.valid.input()`
    3. Anemometer Corrections - `wrap.anem.cor()`
    4. Despiking - `wrap.despike()`
    5. Lag Correction - `wrap.lag()`
    6. Rotation of Wind Vectors - `wrap.rot()`
    7. Calculate Fluxes - `eddy4R.turb::wrap.flux()`
    8. Stationarity Testing - `eddy4R.turb::def.stna()`
    9. Integrated Turbulence Characteristics - `eddy4R.turb::def.itc()`
    10. Integral Length Scales - `wrap.isca()`
    11. Random and Systematic Errors - `eddy4R.turb::def.ucrt.samp()`
    12. Limits of Detection - `def.lod()`
    13. Write files - `write.REYN()`

