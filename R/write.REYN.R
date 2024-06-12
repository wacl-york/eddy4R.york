#' Write REYN
#'
#' Writes the data associated with the REYN (classical ec) processing.
#'
#' @param REYN REYN List of objects at the end of calcualtions
#' @param lag_time lag time data frame
#' @param para parameters list
#' @param ACF ACF data.frame
#' @param file_name file name based on aggreation period
#' @param qfOut A list of data frame containing the calculated sensors quality flags and statistical flags. Defaults as NULL.
#'
#' @export

write.REYN = function(REYN,
                      lag_out,
                      DirOut,
                      tz,
                      write_fast_data,
                      subDirMonthly){

  REYN = c(REYN, lag_out)
  REYN$eddy.data = NULL

  unixTimeMin = min(REYN$data$unixTime,na.rm = T)

  if(subDirMonthly){
    fileStart = as.POSIXct(unixTimeMin, tz = tz, origin = "1970-01-01")
    fileYear = format(fileStart, "%Y")
    fileMonth = format(fileStart, "%m")

    DirOut = file.path(DirOut, fileYear, fileMonth)

  }

  if(!dir.exists(DirOut)){
    dir.create(DirOut, recursive = T)
  }

  purrr::iwalk(REYN, ~{
    # flatten list data
    if(.y %in% c("itc", "isca","error","stna","mtrxRot01")){
      .x = as.data.frame(.x)
    }

    # give all outputs a common date column.
    .x$unixTimeMin = unixTimeMin

    # write fast data if set to.
    if(.y %in% c("base", "conv", "data", "diff")){
      if(write_fast_data){

        # check / create fast_data dir - no longer created during housekeeping
        # save zipped versions of data


      }else{
        next
      }
      # write other outputs
    }else{

      # write or append .csv

    }

  })

}
