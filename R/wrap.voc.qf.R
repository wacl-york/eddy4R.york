##############################################################################################
#' @title Wrapper function: Determination of statistical flags for VOC flux comparison
#' @author
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}

#' @description
#' Wrapper function to generate the statistical flags for sensors (PTR-Qi-TOF and CPEC200) which used in VOC comparison.

#' @param data Required input. A data frame containing the data to be evaluated.
#' @param thshFile The file directory where the threshold table are being saved.

#' @return qfPlau A list of data frame containing the calculated sensors quality flags and statistical flags

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords VOC, qfqm

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Natchaya P-Durden (2019-08-08)
#     original creation
#   Natchaya P-Durden (2019-09-11)
#     remove qfPers and qfStep
##############################################################################################

wrap.qf.voc <- function(
    data,
    thshFile = NULL
){
  #check if threshold file is provided
  if (is.null(thshFile)){

    stop("Threshold directory is missing!")

  }

  #read-in thresholds table
  dfThsh <- utils::read.csv(file = thshFile, header = TRUE,stringsAsFactors = F)

  # select data that has corresponding thresholds table entries
  data_qf = data[,which(names(data) %in% (dfThsh$varName))]
  data_qf = data_qf[,sort(names(data_qf))]
  # sort dfThsh alphabetically by the variable name - now they should always match up, regardless of the order in which the thresh table is written
  dfThsh = dfThsh[order(dfThsh$varName),]

  #Running the statistical (plausibility)test
  qfPlau <- eddy4R.qaqc::def.plau (
    data = data_qf, # a data frame containing the data to be evaluated (do not include the time stamp vector here). Required input.
    # ts = as.POSIXlt(timeRglr20),  # time vector corresponding with the rows in data, in  Class "POSIXlt", which is a named list of vectors representing sec, min, hour,day,mon,year. Defaults to an evenly spaced time vector starting from execution by seconds.
    RngMin = dfThsh$RngMin, # a numeric vector containing the minimum acceptable value for each variable in data, defaults to observed minimums
    RngMax = dfThsh$RngMax, # a numeric vector containing the maximum acceptable value for each variable in data, defaults to observed maximums
    DiffStepMax = dfThsh$DiffStepMax, # a vector containing the maximum acceptable absolute difference between sequential data points for each variable in data
    DiffPersMin = dfThsh$DiffPersMin,#0, # a vector containing the minimum absolute change in value for each variable in data over the interval specified in TintPers. Defaults to a vector of zeros.
    WndwPers = as.difftime(dfThsh$WndwPers, units = "secs"), # a vector of class difftime specifying the time interval for each variable in data over which to test for the minimum absolute change in value specified in DiffPersMin. Defaults to 60 x median observed time difference. Class difftime can be generated using as.difftime.
    TestNull = rep(FALSE,ncol(data_qf)), # apply the null test? A logical vector of [TRUE or FALSE] of length equal to number of variables in data. Defaults to FALSE (no null values are flagged)
    NumGap = rep(length(data_qf[,1])+1,ncol(data_qf)), # an integer greater than 0 specifying the number of consecutive NA values that constitute a gap
    Vrbs = TRUE
  )
  #get name
  #names(qfPlau)
  #add the sensors name to qf
  #qfSensName <- as.vector(sapply(names(qfPlau), function(x) paste0(names(qfPlau[[x]]), paste(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)), sep=""))))
  #combine all qf into one data frame
  #qfSens <- do.call("cbind", qfPlau)
  #add column name
  #names(qfSens) <- qfSensName
  lapply(names(qfPlau), function(x) {
    #remove unuse qf (qfNull and qfGap)
    qfPlau[[x]] <<- data.frame(qfPlau[[x]][,grep("qfNull|qfGap|qfPers|qfStep",names(qfPlau[[x]]),invert = TRUE)])
    colnames(qfPlau[[x]]) <<- "qfRng"
    colnames(qfPlau[[x]]) <<- paste0(names(qfPlau[[x]]), paste(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)), sep=""))
    #add variable names into the flag
  })
  #qfSens <- qfSens[,grep("qfNull|qfGap",names(qfSens),invert = TRUE)]

  #calculate sensors diagnostic flag
  #qfPlau$qfCpec <- data.frame(qfCpec = ifelse(is.na(data$diag_cpec), -1, ifelse(data$diag_cpec > 0, 1, 0)))
  #qfPlau$qfIrga <- data.frame(qfIrga = ifelse(is.na(data$diag_irga), -1, ifelse(data$diag_irga > 0, 1, 0)))
  #qfPlau$qfSoni <- data.frame(qfSoni = ifelse(is.na(data$diag_sonic), -1, ifelse(data$diag_sonic > 0, 1, 0)))

  return(qfPlau)

}
