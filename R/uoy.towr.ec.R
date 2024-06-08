#' UoY Towr Wrapper, Wrapper
#'
#' Handles file reading, progress and error logging. Calls wrap.uoy.ec.towr on valid input files.
#' Can resume the loop from an agg_files index, supplied to resume
#'
#' @param paraMain parameters list made by \code{def.para()}
#' @param resume loop iterator to restart at.
#' @param thshFile The file directory where the threshold table are being saved. Default as NULL.
#' @param diagSens Logical to state if the sensor diqgnostic flags are calculated. Default as FALSE.
#'
#' @author W. S. Drysdale
#'
#' @export

uoy.towr.ec = function(paraMain,
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

    # Read data
    eddy.data = eddy4R.york::read_input(DirInp = paraMain$DirInp,
                                        dateFormat = paraMain$dateFormat,
                                        agg_f = agg_files[[i]],
                                        agg_p = agg_period[i,],
                                        Tz = paraMain$Tz,
                                        freq = paraMain$freqIN,
                                        file_type = paraMain$file_type_in,
                                        PltfEc=paraMain$PltfEc)

    # Check input file
    valid = eddy4R.york::def.valid.input(eddy.data, paraMain, i)

    if(length(valid$skip_scalar) > 0){
      para = eddy4R.york::def.para.tmp(paraMain, valid$skip_scalar)
    }else{
      para = paraMain
    }

    # If the input file has not been flagged to skip
    if(!eddy4R.york::err_skip(error_input)){
      error_workflow = eddy4R.york::wrap.towr(eddy.data = eddy.data,
                                              para = para,
                                              file_count = i,
                                              skip_scalar = valid$skip_scalar,
                                              verbose = FALSE,
                                              agg_period = agg_period,
                                              thshFile = thshFile,
                                              diagSens = diagSens)
    }

  }
  print("Run Complete!")
}
