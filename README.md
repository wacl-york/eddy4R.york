# eddy4R.york

## Description 
eddy4R.york is an R package that extends [eddy4R](https://github.com/NEONScience/eddy4R) by providing features and helpers that are used to build the WACL-York flux workflows. 

The major feature allows for flux workflows to be defined by relatively short configuration scripts, making it simple to iterate on the parameters used, while maintaining reproducibility. 

#### Some other features include: 
- Flexible aggregation periods - `def.avg()` determines what files are relevant to the user defined aggregation period so inputs can be stored in any time bins e.g. if files are organised into 30 min files, but the user wishes to to calculate fluxes hourly, the correct files will be loaded for this to occur.

- Support for double rotation - `wrap.rot()` implements double rotation and wraps it with the existing planar fit capability. Additionally allows for sector or time based changes to planar fit coefficients.

- Options to restrict time lag ranges via `wrap.lag()`.

- Error catching and logging 

## Usage

As eddy4R is provided as a docker image and as such eddy4R.york provides updated image - currently based on the [eddy4R:deve](https://quay.io/repository/battelleecology/eddy4r) image, but this will change to the stable annual releases when available.

> :page_facing_up: <span style="color:#01C400;font-weight:bold"> Note </span> \
> Currently eddy4R.york uses development versions of `eddy4R.turb::wrap.flux()` and `eddy4R.turb::def.stna()` to enable the use of custom scalars. When these features are pulled in upstream it will no longer make direct changes to any of the base image's packages.

## Installation

The docker image can be installed using:

```
docker pull ghcr.io/wacl-york/eddy4r.york:0.1
```

## Run the container

To run the container as an interactive session run the following:

```
docker run --name eddy4r.york --rm -d -p 8787:8787 -v <extDir>:/home/rstudio/data eddy4r.york
```
Where `<extDir>` is the path to the volume you wish to mount on your machine

> :page_facing_up: <span style="color:#01C400;font-weight:bold"> Note </span> \
> The eddy4R.york image sets `DISABLE_AUTH=true` by default, which is different from the `eddy4R` and `rocker` images