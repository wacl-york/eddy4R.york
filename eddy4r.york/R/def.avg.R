#' Define Averaging Period
#'
#' Determine the files that correspond to the selected flux averaging period using a mask for the file name
#'
#' @param filePaths vector of file names
#' @param fileNames vector of file paths
#' @param fileMask string mask that can be passed to \code{as.POSIXct(format = fileMask)}
#' @param fileDuration numeric length of files in seconds
#' @param aggregationDuration numeric required flux duration
#'
#'
#' @author W. S. Drysdale
#'
#' @export


def.avg = function(
    filePaths,
    fileNames,
    fileMask,
    fileDuration,
    aggregationDuration
){

  fileData = tibble::tibble(
    filePaths,
    fileNames,
    fileMask
  ) |>
    dplyr::mutate(
      fileStartDate = as.POSIXct(fileNames, format = fileMask),
    )

  # Floor the minimum start and maximum end times so we start on an round time stamp
  minStartUnix = as.numeric(min(fileData$fileStartDate))
  minStartFloorDate = as.POSIXct(minStartUnix - (minStartUnix %% aggregationDuration), origin = "1970-01-01", tz = "UTC")

  maxStartUnix = as.numeric(max(fileData$fileStartDate))
  maxStartFloorDate = as.POSIXct(maxStartUnix - (maxStartUnix %% aggregationDuration), origin = "1970-01-01", tz = "UTC")

  aggregationPeriods = tibble::tibble(
    periodStartDate = seq(minStartFloorDate, maxStartFloorDate, aggregationDuration) # create regular sequence of aggregation period start times
  ) |>
    dplyr::mutate(
      periodEndDate = .data$periodStartDate + aggregationDuration, # period end times
      earliestFileStartDate = .data$periodStartDate - fileDuration, # broaden the periods so we catch files that might span start times.
      latestFileStartDate = .data$periodEndDate + fileDuration # likely looking too wide here, but its fine to read in a few more files to ensure coverage
    )

  # Satify R CMD check.  the .data$ method is beyond me for the join_by()
  between = fileStartDate = earliestFileStartDate = latestFileStartDate = NULL

  dplyr::left_join(
    fileData,
    aggregationPeriods,
    by = dplyr::join_by(
      between(fileStartDate, earliestFileStartDate, latestFileStartDate)
    )
  ) |>
    dplyr::group_by(.data$periodStartDate, .data$periodEndDate) |>
    dplyr::summarise(filePaths = list(filePaths), .groups = "drop") |>
    dplyr::arrange(.data$periodStartDate)

}

