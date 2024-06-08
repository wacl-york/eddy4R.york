#' UoY Towr Wrapper, Wrapper
#'
#' Handles file reading, progress and error logging. Calls wrap.uoy.ec.towr on valid input files.
#' Can resume the loop from an agg_files index, supplied to resume
#'
#' @param para
#' @param resume
#' @param thshFile The file directory where the threshold table are being saved. Default as NULL.
#' @param diagSens Logical to state if the sensor diqgnostic flags are calculated. Default as FALSE.
#'
#' @author W. S. Drysdale
#'
#' @export

uoy.towr.ec = function(para,
                       resume = NULL,
                       thshFile = NULL,
                       diagSens = FALSE){

  if(is.null(resume)){
    start = 1
  }else{
    start = resume
  }

  setwd(para$DirWrk)
  # input directory
  if(!dir.exists(para$DirInp)){
    dir.create(para$DirInp)
  }

  # output directory
  if(!dir.exists(para$DirOut)){
    dir.create(para$DirOut,recursive = T)
  }

  # Fast data dir if required
  if(para$write_fast_data){
    if(!dir.exists(para$DirFast)){
      dir.create(para$DirFast,recursive = T)
    }
  }

  saveRDS(para,file = file.path(para$DirOut, paste0(para$analysis,"_para.RDS")))

  #determine flux aggregation
  det_avg = eddy4R.york::def.avg(files = para$files,
                                 mask = para$file_mask,
                                 file_duration = para$file_duration,
                                 aggr_dur = para$agg_period,
                                 freq = para$freqIN,
                                 tz = para$Tz,
                                 first_file_begin = para$first_file_begin,
                                 final_file_begin = para$final_file_begin)

  agg_files = det_avg$agg_files
  agg_period = det_avg$agg_period

  for(i in start:length(agg_files)){
    # if there are no files for this aggregationg period, skip
    if(is.na(agg_files[i])){
      next
    }
    # create progress bar
    pb = progress::progress_bar$new(
      format = " :file/:tfile [:bar] :percent | :praise", total = 12)
    # create this files praise
    some_praise = praise::praise("${EXCLAMATION}! - This flux you are ${adverb_manner} ${creating} is ${ADJECTIVE}")
    # store bar and variables in a list for passing to wrap.uoy.ec.towr
    progress_bar = list(pb = pb,
                        total_file = length(agg_files),
                        some_praise = praise::praise(
                          "${EXCLAMATION}! - This flux you are ${adverb_manner} ${creating} is ${ADJECTIVE}"
                        )
    )
    # start progress bar
    progress_bar$pb$tick(tokens = list(file = i,
                                       tfile = progress_bar$total_file,
                                       praise = progress_bar$some_praise))

    # Read data
    eddy.data = eddy4R.york::read_input(DirInp = para$DirInp,
                                        dateFormat = para$dateFormat,
                                        agg_f = agg_files[[i]],
                                        agg_p = agg_period[i,],
                                        Tz = para$Tz,
                                        freq = para$freqIN,
                                        file_type = para$file_type_in,
                                        PltfEc=para$PltfEc)

    # Check input file
    valid = eddy4R.york::def.valid.input(eddy.data, para, i)
    error_input = valid$error_list
    error_workflow = list()
    # If the input file has not been flagged to skip
    if(!eddy4R.york::err_skip(error_input)){
      error_workflow = eddy4R.york::wrap.towr(eddy.data = eddy.data,
                                              para = para,
                                              file_count = i,
                                              skip_scalar = valid$skip_scalar,
                                              verbose = FALSE,
                                              progress_bar = progress_bar,
                                              agg_period = agg_period,
                                              thshFile = thshFile,
                                              diagSens = diagSens)
    }

    # collate errors
    errors = c(error_input,error_workflow)

    # If there was an error, update the log
    if(length(errors) > 0){
      errors = eddy4R.york::error_list_to_df(errors,file = paste0(agg_period$avg_start[i],collapse = "_"))

      # save errors
      out_file_name = paste0(para$DirOut, "/", para$analysis, "/", para$analysis,"_", para$run_id, "_error_log", ".csv")

      if(!file.exists(out_file_name)){
        utils::write.table(errors, file = out_file_name, na = "NA", row.names = F,sep = ",",col.names = T)
      }else{
        utils::write.table(errors, file = out_file_name, na = "NA", row.names = F,append = T,sep = ",",col.names = F)
      }
    }


    # inc pb

  }
  print("Run Complete!")
}
