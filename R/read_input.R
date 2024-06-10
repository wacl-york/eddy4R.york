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
                      idepVar,
                      PltfEc){

  # R CMD check
  unixTime = NULL

  # load Tower analysis files, with option to time clip
  if(PltfEc=="towr"){

    flux_agg = purrr::map_df(file.path(DirInp,agg_f), utils::read.csv) %>%
      dplyr::mutate(date = as.POSIXct(unixTime, format = dateFormat, origin = "1970-01-01 00:00:00"))

    flux_agg$idep = flux_agg[[idepVar]]


    eddy.data = flux_agg[flux_agg$date >= agg_p$avg_start &
                           flux_agg$date <= agg_p$avg_end,]

    eddy.data = def.wind.dir.flow(eddy.data,freq)
  }

  #Return eddy data
  dplyr::select(eddy.data, -date)

}
