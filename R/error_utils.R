##' Add Error
##'
##' Quickly add errors to an error list without all that tedious mucking around with \code{[[]]}
##'
##' @param new_message Error message text
##' @param error_list list where errors are stored
##' @param location indentify where in the code this error is occuring (i.e what function)
##' @param condition character either "skip" or "continue". Skip logs the error
##' and moves to the next aggreation period. Continue logs the error and attempts to execute the next code block
##'
##' @author W. S. Drysdale
##'
##' @export

add_err = function(new_message,
                   error_list = error_list,
                   location,
                   condition = c("skip","continue")[1]){
  error_list[[(length(error_list)+1)]] = paste0(location,": ",new_message," condition: ",condition)
  error_list
}

##' err_skip
##'
##' Does the error_list contain skip conditions
##'
##' @param error_list error list object
##'
##' @author W. S. Drysdale
##'
##' @export

err_skip = function(error_list){
  condition_is_skip = function(err){
    err = stringr::str_split(err,"condition: ") %>% '[['(1) %>% '['(2)
    #return
    (err == "skip")
  }

  skips = purrr::map_lgl(error_list,condition_is_skip)

  #return
  (T %in% skips)
}

##' error list to data.frame
##'
##' transforms the add_err output to a dataframe that can be more sensibly be written to disk
##'
##' @param error_list error list object
##' @param file id of the input file where the error occured
##'
##' @author W. S. Drysdale
##'
##' @export

error_list_to_df = function(error_list,file){

  error_list = error_list
    purrr::map(stringr::str_replace_all,pattern = "condition","") %>%
    purrr::map(stringr::str_split,patter = ":") %>%
    data.frame(stringsAsFactors = F)

  error_list = t(error_list)

  colnames(error_list) = c("location","error","condtion")
  rownames(error_list) = 1:nrow(error_list)
  error_list = data.frame(error_list,stringsAsFactors = F)
  for(i in 1:ncol(error_list))
    error_list[,i] = trimws(error_list[,i],"both")
  error_list$file = rep(file,nrow(error_list))
  #Return
  tibble::as.tibble(error_list)
}
