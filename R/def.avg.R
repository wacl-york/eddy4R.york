#' Define Avgeraging Period
#'
#' Determing the files that correspond to the selected flux averaging period using a mask for the file name
#'
#' @param files vector of files
#' @param fileMask string mask that can be passed to \code{as.POSIXct(format = fileMask)}
#' @param fileDuration numeric length of files in seconds
#' @param aggr_dur numeric required flux duration
#' @param tz string timezone of date in file name
#' @param freq data aquistion frequency (Hz)
#' @param fileFirstStart optional. This is usually equal to the timestamp in the first file name
#' can be overridden here if the first file does not at the begining of a theoretical aggregation period
#' @param fileLastStart optional. as first file begin but for the last file.
#'
#'
#' @author W S Drysdale
#'
#' @export


def.avg = function(files,
                   fileMask = "NOx_5Hz_yymmdd_HHMM0_170322_000015_cor_temp",
                   fileDuration = 3600,
                   aggr_dur = 7200,
                   freq = 5,
                   tz,
                   fileFirstStart = NULL,
                   fileLastStart = NULL){

  #list of files in input directory
  files = files[nchar(files) == nchar(fileMask)] %>% as.array

  #get file start times from names
  act_file_start = as.POSIXct(files, format = fileMask, tz = tz)


  #user can input a fileFirstStart time if the first file starts at an unusual interval e.g. most files start on an hour but this
  #particular file starts at 20 past the hour
  if(is.null(fileFirstStart)){
    fileFirstStart = act_file_start[1]
  }

  if(is.null(fileLastStart)){
    fileLastStart = act_file_start[length(act_file_start)]
  }

  #create a list of 'theoretical' file ranges i.e. if all files were complete
  #Files can be assigned to this list which in turn can be associated with an agg period (avoids problems where files aren't complete/have
  #irregular start times)
  est_file_range = data.frame(file_start = seq(fileFirstStart,fileLastStart,fileDuration))
  est_file_range$file_end = est_file_range$file_start + fileDuration - (1/freq)
  est_file_range$index = 1:nrow(est_file_range)

  file_list <- data.frame(act_file_start, files)

  #match up real data files to theoretical file periods
  act_files = list()
  for(i in 1:nrow(est_file_range)){
    act_files[[i]] = file_list$files[file_list$act_file_start >= est_file_range$file_start[i] &
                                       file_list$act_file_start <= est_file_range$file_end[i]]
  }

  aggregationPeriod = data.frame(avg_start = seq(act_file_start[1],act_file_start[length(act_file_start)]+(aggr_dur-1/freq),aggr_dur))
  aggregationPeriod$avg_end = aggregationPeriod$avg_start + aggr_dur - (1/freq)
  aggregationPeriod$avg_mid = aggregationPeriod$avg_start + ((aggregationPeriod$avg_end - aggregationPeriod$avg_start)/2)
  aggregationPeriod$avg_mid = aggregationPeriod$avg_mid %>%
    lubridate::round_date("min")
  agg_files = list()

  #select files associated with the aggregation period
  #rephrase as lapply?
  for (i in 1:nrow(aggregationPeriod)){
    file_ref = est_file_range$index[est_file_range$file_end >= aggregationPeriod$avg_start[i] & est_file_range$file_start <= aggregationPeriod$avg_end[i]]
    temp = c()

    for(j in file_ref)
      temp = c(temp,act_files[[j]])

    agg_files[[i]] = temp
  }

  for(i in 1:length(agg_files)){
    if(identical(agg_files[[i]], character(0)))
      agg_files[[i]] = NA
  }

  #return
  list(
    agg_files = agg_files,
    aggregationPeriod = tibble::tibble(aggregationPeriod))
}

