#' UoY Towr Wrapper, Wrapper
#'
#' Handles file reading, progress and error logging. Calls wrap.uoy.ec.towr on valid input files.
#' Can resume the loop from an agg_files index, supplied to resume
#'
#' @param agg_files
#' @param agg_period
#' @param para
#' @param resume
#' @param thshFile The file directory where the threshold table are being saved. Default as NULL.
#' @param diagSens Logical to state if the sensor diqgnostic flags are calculated. Default as FALSE.

uoy.towr.ec = function(agg_files,
                       agg_period,
                       para,
                       resume = NULL,
                       thshFile = NULL,
                       diagSens = FALSE){

  if(is.null(resume))
    start = 1
  else
    start = resume

  save(para,file = paste0(para$DirOut,"/",para$analysis,"/",para$analysis,"_para.Rdata"))

  for(i in start:length(agg_files)){
    # if there are no files for this aggregationg period, skip
    if(is.na(agg_files[i]))
      next
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
    eddy.data = read.e4r_input(DirInp = para$DirInp,
                               agg_f = agg_files[[i]],
                               agg_p = agg_period[i,],
                               Tz = para$Tz,
                               freq = para$freqIN,
                               file_type = para$file_type_in,
                               PltfEc=para$PltfEc)

    # Check input file
    valid = def.valid.input(eddy.data,para,i)
    error_input = valid$error_list
    error_workflow = list()
    # If the input file has not been flagged to skip
    if(!err_skip(error_input))
      error_workflow = wrap.uoy.ec.towr(eddy.data = eddy.data,
                                        para = para,
                                        file_count = i,
                                        skip_scalar = valid$skip_scalar,
                                        verbose = FALSE,
                                        progress_bar = progress_bar,
                                        agg_period = agg_period,
                                        thshFile = thshFile,
                                        diagSens = diagSens)
    # collate errors
    errors = c(error_input,error_workflow)

    # If there was an error, update the log
    if(length(errors) > 0){
      errors = error_list_to_df(errors,file = paste0(agg_period$avg_start[i],collapse = "_"))

      # save errors
      out_file_name = paste0(para$DirOut, "/", para$analysis, "/", para$analysis,"_", para$run_id, "_error_log", ".csv")

      if(!file.exists(out_file_name))
        write.table(errors, file = out_file_name, na = "NA", row.names = F,sep = ",",col.names = T)
      else
        write.table(errors, file = out_file_name, na = "NA", row.names = F,append = T,sep = ",",col.names = F)
    }


    # inc pb

  }
  print("Run Complete!")
}
