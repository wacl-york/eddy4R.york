#' Read eddy4R input
#'
#' reads the files relevant for a given flux aggregation period
#' TODO this can be substantially tidied
#'
#' @param DirInp input directory
#' @param dateFormat format of date when reading csv for \code{as.POSIXct()} - reading from UNIX time to this format - default %Y-%m-%d %H:%M:%OS"
#' @param filePaths files to aggregate
#' @param periodStartDate start of aggregation period
#' @param periodEndDate end of aggregation period
#' @param tz time zone
#' @param freq data acquisition frequency
#' @param idepVar variable that could be used if Algbase in "trend" or "ord03"
#' column gets duplicated to a column called "idep" used in \code{eddy4R.turb::def.stat.sta.diff}
#' @param PltfEc platform either "towr" or "airc"
#'
#' @author W. S. Drysdale
#'
#' @export

read_input = function(DirInp,
                      dateFormat,
                      filePaths,
                      periodStartDate,
                      periodEndDate,
                      tz,
                      freq,
                      idepVar,
                      PltfEc){

  # R CMD check
  unixTime = NULL

  # load Tower analysis files, with option to time clip
  if(PltfEc=="towr"){

    flux_agg = purrr::map_df(filePaths, utils::read.csv) |>
      dplyr::mutate(
        date = as.POSIXct(unixTime, format = dateFormat, origin = "1970-01-01 00:00:00", tz = tz))

    flux_agg$idep = flux_agg[[idepVar]]


    eddy.data = flux_agg[flux_agg$date >= periodStartDate & flux_agg$date <= periodEndDate,]

  }

  #Return eddy data
  dplyr::select(eddy.data, -date)

}
