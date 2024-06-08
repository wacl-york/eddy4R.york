#' Read eddy4R input
#'
#' reads the files relevant for a given flux aggregation period
#' TODO this can be substantially tidied
#'
#' @param DirInp input directory
#' @param dateFormat format of date when reading csv for \code{as.POSIXct()}
#' @param agg_f files to aggregate
#' @param agg_p period which files are relvant too
#' @param Tz timestamp timezone
#' @param freq data aquisiton frequency
#' @param file_type either ".csv" or ".rds"
#' @param PltfEc platform either "towr" or "airc"
#'
#' @author W. S. Drysdale
#'
#' @export

read_input = function(DirInp,
                      dateFormat,
                      agg_f,
                      agg_p,
                      Tz,
                      freq,
                      file_type,
                      PltfEc){

  #load Tower analysis files, with option to time clip
  if(PltfEc=="towr"){

    if(file_type==".csv"){
      flux_agg = purrr::map_df(file.path(DirInp,agg_f), utils::read.csv) %>%
        dplyr::mutate(date = as.POSIXct(date, format = dateFormat))
    }

    if(file_type=="rds"){
      flux_agg = purrr::map_df(file.path(DirInp,agg_f), readRDS)
    }

    eddy.data = flux_agg[flux_agg$date >= agg_p$avg_start &
                           flux_agg$date <= agg_p$avg_end,]

    eddy.data = def.wind.dir.flow(eddy.data,freq)
  }

  #Load aicraft data. Files should already be formated and need to cutting
  if(PltfEc=="airc"){
    if(file_type=="csv"){
      eddy.data = utils::read.csv(paste0(DirInp,"/",agg_f)) %>%
        dplyr::mutate(date = lubridate::ymd_hms(date,tz = Tz))}

    if(file_type=="rds"){eddy.data = readRDS(file=paste0(DirInp,"/",agg_f))}
  }

  #Return eddy data
  return(eddy.data)

}
