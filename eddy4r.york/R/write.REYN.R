#' Write REYN
#'
#' Writes the data associated with the REYN (classical ec) processing.
#'
#' @param REYN REYN List of objects at the end of calcualtions
#' @param lag_out output list from \code{wrap.lag()}
#' @param DirOut output directory (if subDirMonthly == TRUE, annual and monthly subfolders will be created here)
#' @param analysis name of analysis - used to construct file name
#' @param tz timezone
#' @param writeFastData TRUE/FALSE should the fast (base, diff, conv, data) outputs be written to disk. They will be compressed using gzip
#' @param subDir one of c("none", "monthly", "daily") - default none. Should the outputs be split into monthly or daily subdirectories
#'
#' @export

write.REYN = function(REYN,
                      lag_out,
                      DirOut,
                      analysis,
                      tz,
                      writeFastData,
                      subDir){

  REYN = c(REYN, lag_out)
  # lag out can contain eddy.data, which is the same as REYN$data, so we NULL it here to avoid duplication
  REYN$eddy.data = NULL

  unixTimeMin = min(REYN$data$unixTime,na.rm = T)
  fileStart = as.POSIXct(unixTimeMin, tz = tz, origin = "1970-01-01")

  if(subDir != "none"){
    fileYear = format(fileStart, "%Y")
    fileMonth = format(fileStart, "%m")
    fileDay = format(fileStart, "%d")

    if(subDir == "monthly"){
      DirOut = file.path(DirOut, fileYear, fileMonth)
    }else{
      DirOut = file.path(DirOut, fileYear, fileMonth, fileDay)
    }
  }

  files = REYN |>
    dplyr::tibble() |>
    dplyr::mutate(fileBase = names(REYN)) |>
    dplyr::mutate(
      alwaysWrite = !.data$fileBase %in% c("base", "conv", "data","diff"),
      compress = .data$fileBase %in% c("ACF","foot", "spec", "base", "conv", "data","diff"),
      dirName = ifelse(.data$compress, .data$fileBase, NA),
      write = .data$alwaysWrite | writeFastData,
      dirForFile = ifelse(is.na(.data$dirName), DirOut, file.path(DirOut, "compressedData" , .data$dirName)),
      fileSuffix = paste0(analysis,"_", .data$fileBase, ifelse(.data$compress, ".csv.gz", ".csv")),
      fileName = ifelse(.data$compress, paste0(format(fileStart, "%Y%m%d_%H"),"_", .data$fileSuffix), .data$fileSuffix),
      fileOut = file.path(.data$dirForFile, .data$fileName),
      flatten = .data$fileBase %in% c("itc", "isca","error","stna","mtrxRot01")) |>
    dplyr::filter(.data$write)

  for(i in 1:nrow(files)){

    if(!dir.exists(files$dirForFile[i])){
      dir.create(files$dirForFile[i], recursive = T)
    }

    if(files$flatten[i]){
      files$REYN[[i]] = as.data.frame(files$REYN[[i]])
    }

    files$REYN[[i]]$unixTimeMin = unixTimeMin

    if(files$compress[i]){
      utils::write.csv(files$REYN[[i]],
                       file = files$fileOut[i],
                       row.names = F)
    }else{
      if(!file.exists(files$fileOut[i])){
        utils::write.table(files$REYN[[i]],
                           file = files$fileOut[i],
                           row.names = F,
                           sep = ",")
      }else{
        utils::write.table(files$REYN[[i]],
                           file = files$fileOut[i],
                           row.names = F,
                           append = T,
                           col.names = F,
                           sep = ",")
      }
    }
  }

}
