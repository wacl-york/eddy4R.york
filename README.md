# eddy4R.york

## Description 
eddy4R.york is an R package that extends [eddy4R](https://github.com/NEONScience/eddy4R) by providing features and helpers that are used to build the WACL-York flux workflows. 

The major feature allows for flux workflows to be defined by relatively short configuration scripts, making it simple to iterate on the parameters used, while maintaining reproducibility. 

#### Some other features include: 
- Flexible aggregation periods - `def.avg()` determines what files are relevant to the user defined aggregation period so inputs can be stored in any time bins e.g. if files are organised into 30 min files, but the user wishes to to calculate fluxes hourly, the correct files will be loaded for this to occur.

- Support for double rotation - `wrap.rot()` implements double rotation and wraps it with the existing planar fit capability. Additionally allows for sector or time based changes to planar fit coefficients.

- Options to restrict time lag ranges via `wrap.lag()`.

- Error catching and logging 

#### Getting Started

> :page_facing_up: **Note**  \
> As eddy4R is provided as a docker image and as such eddy4R.york provides updated image - currently based on the [eddy4R:deve](https://quay.io/repository/battelleecology/eddy4r) image, but this will change to the stable annual releases when available.
> Currently eddy4R.york uses development versions of `eddy4R.turb::wrap.flux()` and `eddy4R.turb::def.stna()` to enable the use of custom scalars. When these features are pulled in upstream it will no longer make direct changes to any of the base image's packages.

#### Installation

The docker image can be installed using:

```
docker pull ghcr.io/wacl-york/eddy4r.york:0.1
```

#### Run the container

To run the container as an interactive session run the following:

```
docker run --name eddy4r.york --rm -d -p 8787:8787 -v <extDir>:/home/rstudio/data eddy4r.york
```
Where `<extDir>` is the path to the volume you wish to mount on your machine

> :page_facing_up: **Note** \
> The eddy4R.york image sets `DISABLE_AUTH=true` by default, which is different from the `eddy4R` and `rocker` images

#### Configuring Run Parameters

`eddy4R.york` uses a parameters list created using `def.para()`. This is the main input to `wrap.towr()` and defines all of the settings of the calculation. Creating this configuration looks like:

```R
para = eddy4R.york::def.para(
  DirWrk = "/home/rstudio/data/",
  DirInp = "in/",
  site_name = "MySite",
  analysis = "nitrate_fluxes",
  run_id = "standard_run",
  file_mask = "input_%y%m%d_%H%M.csv",
  species = c("NO","NO2"),
  agg_period = 3600,
  write_fast_data = TRUE,
  AlgBase = "trnd",
  idepVar = "unixTime",
  MethRot = "double",
  missing_method = "mean",
  lag_correction = TRUE,
  restrict_lag_range = TRUE,
  lag_boundary = list(c(0,0),
                      c(0,0),
                      c(-2,-8),
                      c(-4,-10)),
  absolute_lag = c(0,0,-6, -8))

```

This has set up a run to calculate hourly fluxes of CO~2~ and O~3~ as well as sensible and latent heat. It will use using linear detrending, double rotation and perform a lag correction of all the scalars relative to the vertical wind. If the determined lag falls out of the ranges supplied per scalar, a fixed lag will be used instead.

#### Input Data
Input files contain the following columns. They should all be read by `read.csv()` as numeric. By default the listed units are expected, but this can be changed by using `def.para(unitList = ...)`

| Column Header    | Description                                   | Unit                      |
|------------------|-----------------------------------------------|---------------------------| 
| unixTime         | seconds since midnight 1970-01-01             | s                         |
| veloXaxs         | x component of the 3D wind*                   | m s^-1^                   |
| veloYaxs         | y component of the 3D wind*                   | m s^-1^                   |
| veloZaxs         | z component of the 3D wind*                   | m s^-1^                   |
| tempAir          | air temperature                               | Kelvin                    |
| presAtm          | atmospheric pressure                          | Pa                        |
| distZaxsAbl      | Atmospheric Boundary Layer Height             | m                         |
| distZaxsMeas     | measurement height above surface              | m                         |
| rtioMoleDryH2o   | dry mole water vapour concentration           | mol~H2O~ mol~DryAir~^-1^  |
| rtioMoleDry{Spc} | dry mole concentration of other species "spc" | mol~{spc}~ mol~DryAir~^-1^|

*x/y/z == u/v/w == east/north/up

#### Overview of `wrap.towr()` 

`wrap.towr()` is the function that contains all possible steps of the workflow which are configured .by `def.para()`. This consists of some initial steps, then a loop around all of the flux aggregation periods that have been defined:

- Determine aggregation periods - `det.avg()`
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


















