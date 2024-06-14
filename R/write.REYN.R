#' Write REYN
#'
#' Writes the data associated with the REYN (classical ec) processing.
#'
#' @param REYN REYN List of objects at the end of calcualtions
#' @param lag_out output list from \code{wrap.lag()}
#' @param DirOut output directory (if subDirMonthly == TRUE, annual and monthly subfolders will be created here)
#' @param analysis name of analysis - used to construct file name
#' @param tz timezone
#' @param write_fast_data TRUE/FALSE should the fast (base, diff, conv, data) outputs be written to disk. They will be compressed using gzip
#' @param subDirMonthly TRUE/FALSE should outputs be split into subdirectorys following %Y / %m of the aggregation start date.
#'
#' @export

write.REYN = function(REYN,
                      lag_out,
                      DirOut,
                      analysis,
                      tz,
                      write_fast_data,
                      subDirMonthly){

  REYN = c(REYN, lag_out)
  REYN$eddy.data = NULL

  unixTimeMin = min(REYN$data$unixTime,na.rm = T)
  fileStart = as.POSIXct(unixTimeMin, tz = tz, origin = "1970-01-01")

  if(subDirMonthly){
    fileYear = format(fileStart, "%Y")
    fileMonth = format(fileStart, "%m")

    DirOut = file.path(DirOut, fileYear, fileMonth)

  }

  if(!dir.exists(DirOut)){
    dir.create(DirOut, recursive = T)
  }


  fastFolders = c("base", "conv", "data", "diff")
  DirFast = file.path(DirOut, "fast_data", fastFolders)
  purrr::walk(DirFast[!dir.exists(DirFast)], ~dir.create(.x, recursive = T))

  purrr::iwalk(REYN, ~{
    # flatten list data
    if(.y %in% c("itc", "isca","error","stna","mtrxRot01")){
      .x = as.data.frame(.x)
    }

    # give all outputs a common date column.
    .x$unixTimeMin = unixTimeMin

    # write fast data if set to.
    if(.y %in% fastFolders){
      if(write_fast_data){

        outputFile = paste0(format(fileStart, "%Y%m%d_%H"),"_",analysis,"_",.y, ".csv.gz")

        write.csv(REYN[[.y]],
                  file = gzfile(file.path(DirOut,"fast_data", .y, outputFile)),
                  row.names = F)

      }else{
        next
      }
      # write other outputs
    }else{
      # write or append .csv

      outputFile = file.path(DirOut, paste0(analysis,"_",.y, ".csv"))

      if(!file.exists(outputFile)){
        write.table(REYN[[.y]],
                    file = outputFile,
                    row.names = F,
                    sep = ",")
      }else{
        write.table(REYN[[.y]],
                    file = outputFile,
                    row.names = F,
                    append = T,
                    col.names = F,
                    sep = ",")
      }


    }

  })

}
