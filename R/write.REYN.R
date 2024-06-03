#' Write REYN
#'
#' Write REYN
#'
#' @param REYN REYN List of objects at the end of calcualtions
#' @param lag_time lag time data frame
#' @param para parameters list
#' @param ACF ACF data.frame
#' @param file_name file name based on aggreation period
#' @param qfOut A list of data frame containing the calculated sensors quality flags and statistical flags. Defaults as NULL.
#'
#' @export

write.REYN = function(REYN,lag_time,ACF = NULL,para,file_name, qfOut = NULL){
  if(para$file_type_out == "csv")
    write.REYN.csv(REYN,lag_time,ACF,para,file_name, qfOut = qfOut)
}

#' Write REYN csv
#'
#' Writes REYN output as RDS objects - as Will's workflow
#'
#'
#' @inheritParams write.REYN

write.REYN.csv = function(REYN,lag_time,ACF,para,file_name, qfOut = NULL){

  #save lag times
  out_file_name = paste0(para$DirOut, "/", para$analysis, "/",para$analysis, "_lag_time", ".csv")
  lag_time$DOY = rep(REYN$mn$DOY)
  if(!file.exists(out_file_name))
    utils::write.table(lag_time, file = out_file_name, na = "NA", row.names = F,sep = ",",col.names = T)
  else
    utils::write.table(lag_time, file = out_file_name, na = "NA", row.names = F,append = T,sep = ",",col.names = F)

  #Save Full ACF
  out_file_name = paste0(para$DirOut, "/", para$analysis, "/", para$analysis, "_ACF", ".csv")

  if(!file.exists(out_file_name))
    utils::write.table(ACF, file = out_file_name, na = "NA", row.names = F,sep = ",",col.names = T)
  else
    utils::write.table(ACF, file = out_file_name, na = "NA", row.names = F,append = T,sep = ",",col.names = F)


  if(para$write_fast_data){
    fast_out_folder = paste0(para$DirOut, "/", para$analysis,"/fast_data/", file_name,"/")
    dir.create(fast_out_folder)
  }


  #save REYN
  for (n in names(REYN)){
    if(n %in% c("data","imfl","base")){#save 5hz data separately

      if(para$write_fast_data){
        out_file_name = paste0(fast_out_folder,para$analysis, "_EC_", n, ".csv")
        utils::write.table(REYN[[n]], file = out_file_name, na = "NA", row.names = F,sep = ",")
      }

    }else{#save aggregated data bound

      out_file_name = paste0(para$DirOut, "/", para$analysis, "/",para$analysis, "_EC_", n, ".csv")
      if(!file.exists(out_file_name))
        utils::write.table(REYN[[n]], file = out_file_name, na = "NA", row.names = F,sep = ",",col.names = T)
      else{
        if("date" %in% names(REYN[[n]]))
          REYN[[n]]$date = REYN[[n]]$date %>%
            as.character

        temp = utils::read.csv(out_file_name,stringsAsFactors = F,header = T)
        temp = dplyr::bind_rows(temp,data.frame(REYN[[n]]))
        utils::write.table(temp, file = out_file_name, na = "NA", row.names = F,append = F,sep = ",",col.names = T)
      }

    }
  }
  #write out qf
  if (!is.null(qfOut)){
    for (idxDf in names(qfOut)){
      if (idxDf == "qm"){
        for (idxName in names(qfOut[[idxDf]])){
          #Check if download directory exists and create if not
          DirQmOut <- paste0(para$DirOut, "/", para$analysis, "/qm")
          if(dir.exists(DirQmOut) == FALSE) dir.create(DirQmOut, recursive = TRUE)
          out_file_name <- paste0(para$DirOut, "/", para$analysis, "/qm/",para$analysis, "_qm_", idxName, ".csv")

          if(!file.exists(out_file_name)){
            utils::write.table(qfOut[[idxDf]][[idxName]], file = out_file_name, na = "NA", row.names = F,sep = ",",col.names = T)
          }else{
            #if("date" %in% names(qfOut[[idxDf]][[idxName]])){
            qfOut[[idxDf]][[idxName]]$date = qfOut[[idxDf]][[idxName]]$date %>%
              as.character
            temp <- utils::read.csv(out_file_name,stringsAsFactors = F,header = T)
            temp <- dplyr::bind_rows(temp,data.frame(qfOut[[idxDf]][[idxName]]))
            utils::write.table(temp, file = out_file_name, na = "NA", row.names = F,append = F,sep = ",",col.names = T)
            #}
          }
        }#end loop for idxName
      } else{
        out_file_name <- paste0(para$DirOut, "/", para$analysis, "/",para$analysis, "_qaqc_", idxDf, ".csv")
        if(!file.exists(out_file_name)){
          utils::write.table(qfOut[[idxDf]], file = out_file_name, na = "NA", row.names = F,sep = ",",col.names = T)
        }else{
          #if("date" %in% names(qfOut[[idxDf]])){
          qfOut[[idxDf]]$date = qfOut[[idxDf]]$date %>%
            as.character
          temp <- utils::read.csv(out_file_name,stringsAsFactors = F,header = T)
          temp <- dplyr::bind_rows(temp,data.frame(qfOut[[idxDf]]))
          utils::write.table(temp, file = out_file_name, na = "NA", row.names = F,append = F,sep = ",",col.names = T)
          #}
        }
      }
    }#end loop for

  }
}
