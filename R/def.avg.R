#' Define Avgeraging Period
#'
#' Determing the files that correspond to the selected flux averaging period using a mask for the file name
#'
#' @param files vector of files
#' @param mask string mask that can be passed to \code{mask_extract_date()}
#' @param file_duration numeric length of files in seconds
#' @param aggr_dur numeric required flux duration
#' @param tz string timezone of date in file name
#' @param freq data aquistion frequency (Hz)
#' @param first_file_begin optional. This is usually equal to the timestamp in the first file name
#' can be overridden here if the first file does not at the begining of a theoretical aggregation period
#' @param final_file_begin optional. as first file begin but for the last file.
#'
#'
#' @author W S Drysdale
#'
#' @export


def.avg = function(files,
                   mask = "NOx_5Hz_yymmdd_HHMM0_170322_000015_cor_temp",
                   file_duration = 3600,
                   aggr_dur = 7200,
                   freq = 5,
                   tz,
                   first_file_begin = NULL,
                   final_file_begin = NULL){

  #list of files in input directory
  files = files[nchar(files) == nchar(mask)] %>% as.array

  #get file start times from names
  act_file_start = base::apply(files,1,function(x) mask_extract_date(x,mask,tz = tz)) %>% as.POSIXct(tz = tz,origin = "1970-01-01")

  #user can input a first_file_begin time if the first file starts at an unusual interval e.g. most files start on an hour but this
  #particular file starts at 20 past the hour
  if(is.null(first_file_begin))
    first_file_begin = act_file_start[1]

  if(is.null(final_file_begin))
    final_file_begin = act_file_start[length(act_file_start)]

  #create a list of 'theoretical' file ranges i.e. if all files were complete
  #Files can be assigned to this list which in turn can be associated with an agg period (avoids problems where files aren't complete/have
  #irregular start times)
  est_file_range = data.frame(file_start = seq(first_file_begin,final_file_begin,file_duration))
  est_file_range$file_end = est_file_range$file_start + file_duration - (1/freq)
  est_file_range$index = 1:nrow(est_file_range)

  file_list <- data.frame(act_file_start, files)

  #match up real data files to theoretical file periods
  act_files = list()
  for(i in 1:nrow(est_file_range)){
    act_files[[i]] = file_list$files[file_list$act_file_start >= est_file_range$file_start[i] &
                                       file_list$act_file_start <= est_file_range$file_end[i]]
  }

  avg_period = data.frame(avg_start = seq(act_file_start[1],act_file_start[length(act_file_start)]+(aggr_dur-1/freq),aggr_dur))
  avg_period$avg_end = avg_period$avg_start + aggr_dur - (1/freq)
  avg_period$avg_mid = avg_period$avg_start + ((avg_period$avg_end - avg_period$avg_start)/2)
  avg_period$avg_mid = avg_period$avg_mid %>%
    lubridate::round_date("min")
  agg_files = list()

  #select files associated with the aggregation period
  #rephrase as lapply?
  for (i in 1:nrow(avg_period)){
    file_ref = est_file_range$index[est_file_range$file_end >= avg_period$avg_start[i] & est_file_range$file_start <= avg_period$avg_end[i]]
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
    avg_period = tibble::tibble(avg_period))
}

