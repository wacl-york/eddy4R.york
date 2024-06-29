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

> [!NOTE]
> The eddy4R.york image sets `DISABLE_AUTH=true` by default, which is different from the `eddy4R` and `rocker` images

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

## Running on Viking

The following outlines the steps to get the eddy4r.york container running on the University of York Viking HPC. It assumes you are already registered to use Viking. If not you can follow the guidelines on the [Viking docs](https://vikingdocs.york.ac.uk/) pages.

### Installing the Container

Docker is not available on Viking - Apptainer is used instead. It is simple to convert the docker container to an apptainer .sif. Currently you will need to store the .sif on Viking. This guide stores the image on `/mnt/longship` so that it is not accidentally deleted from `scratch` after 90 days - as running the container does not update the modified flag of the file. Run the following commands, replacing `<usr>` with your username. Note that this step has to be performed on the login node as interactive jobs only have read only access to Longship.

```bash
module load Apptainer/latest

apptainer pull --docker-login /mnt/longship/users/<usr>/eddy4r.york docker://ghcr.io/wacl-york/eddy4r.york:0.1
```

Once this is complete the file `eddy4r.york` should exist in `/mnt/longship/users/<usr>/`

### Running the Container

#### Interactively  

Begin an interactive job with

```bash
srun  srun --time=08:00:00 --partition=interactive --pty /bin/bash
```

Then load apptainer and launch the container. In this example we bind your Viking `scratch` directory to the `/scratch` directory inside the container. You can adjust the `def.para` example above to use: `DirWrk = "/scratch/path/to/data"`. 

```
module load Apptainer/latest

apptainer exec /users/<usr>/scratch:/scratch /mnt/longship/users/<usr>/eddy4r.york R
```
> [!NOTE] 
> The container must be launched using `exec` rather than `run` so that the RStudio server can be overridden with a simple R session. RStudio server requires some root permissions to run that are not available here. 

You should now be inside the container, have access to scratch and be able to source a config script.

#### Running as a Job

You can directly source a config file from the `exec` command. The path to your script needs to be visible to the container, which is simplest when it is located within the directory you have assigned via `--bind`.

```
apptainer exec --bind /users/<usr>/scratch:/scratch /mnt/longship/users/<usr>/eddy4r.york Rscript '/scratch/config.R'
```

You can use this to run `eddy4R` as a job using a .job file similar to:

```
#!/usr/bin/env bash
#SBATCH --job-name=my_job               # Job name
#SBATCH --partition=nodes               # What partition the job should run on
#SBATCH --time=0-00:15:00               # Time limit (DD-HH:MM:SS)
#SBATCH --ntasks=1                      # Number of MPI tasks to request
#SBATCH --cpus-per-task=1               # Number of CPU cores per MPI task
#SBATCH --mem=1G                        # Total memory to request
#SBATCH --account=dept-proj-year        # Project account to use
#SBATCH --mail-type=END,FAIL            # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=abc123@york.ac.uk   # Where to send mail
#SBATCH --output=%x_log/%x-%j.log       # Standard output log
#SBATCH --error=%x_err/%x-%j.err        # Standard error log

# Abort if any command fails
set -e

# Purge any previously loaded modules
module purge

# Load modules
ml Apptainer/latest

# run container
apptainer exec --bind /users/<usr>/scratch:/scratch /mnt/longship/users/<usr>/eddy4r.york Rscript '/scratch/config.R'
```

#### Running as an Array Job
A benefit of running eddy4R as a container is parallelisation is reasonably straight forward. You should run one container per aggregation period, which can be done by modifying `para$files` to only contain the relevant aggregation period. Depending on your files this may require pre calculation of the aggregation periods. For this example we will assume a 1:1 match between input file and aggregation period - i.e. calculating 1 hourly fluxes from 1 hourly input files, with no exceptions - so we do not need to pre calculate aggregation periods. 

Modify your config script to name the analysis after the input file, passed by the `FILE_SELECT` environment variable. This will make each container write to its own folder to avoid conflicts. After `def.para()` has been run, edit `para$files` to only contain the desired input file.

```
para = eddy4R.york::def.para(
  ...
  analysis = stringr::str_remove(Sys.getenv("FILE_SELECT"), ".csv"),
  ...
  )

para$files = para$files[para$files %in% Sys.getenv("fileSelect")]

eddy4R.york::wrap.towr(para)
```

Then, configure an array job similar to:
```
#!/usr/bin/env bash
#SBATCH --job-name=my_job               # Job name
#SBATCH --partition=nodes               # What partition the job should run on
#SBATCH --time=0-00:01:00               # Time limit (DD-HH:MM:SS)
#SBATCH --ntasks=1                      # Number of MPI tasks to request
#SBATCH --cpus-per-task=1               # Number of CPU cores per MPI task
#SBATCH --mem=1G                        # Total memory to request
#SBATCH --account=dept-proj-year        # Project account to use
#SBATCH --mail-type=END,FAIL            # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=abc123@york.ac.uk   # Where to send mail
#SBATCH --output=%x_log/%x-%j.log       # Standard output log
#SBATCH --error=%x_err/%x-%j.err        # Standard error log
#SBATCH --array=0-23%10

# Abort if any command fails
set -e

# Purge any previously loaded modules
module purge

# Load modules
ml Apptainer/latest

# Filter the input files based on the array ID
FILES=(/users/<usr>/scratch/in/*)
FULLFILE=${FILES[$SLURM_ARRAY_TASK_ID]}
FILE=$(basename -- "$FULLFILE")

# run container
srun apptainer exec --env FILE_SELECT=${FILE} --bind /users/<usr>/scratch:/scratch /mnt/longship/users/<usr>/eddy4r.york Rscript '/scratch/eddy4R/testdata/test_config2.R'
```

> [!IMPORTANT]
> You must ammend the `--array=0-23%10` to match the number of containers you expect to spawn. Here we are processing 24 hours of data 10 jobs at a time. We filter the input files using the `$SLURM_ARRAY_TASK_ID` and pass this to the container using `--env`, which is then avalible to R via `Sys.getenv()`.

> [!NOTE]
> You can should adjust the `--time` argument to be greater than but close to the time you expect each container to take to run. For hourly input files this is around 1 min, but you may wish to test this in advance.

You can then trigger the job via `sbatch` as usual. 
