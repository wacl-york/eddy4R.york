#' Extract Date using Mask
#'
#' By specifiic year = yy or yyyy, month = mm, day = dd, hour = HH, month = MM , second = SS (optional)
#' TODO this might be completely unnecessary as we could just use \code{as.POSIXct(x, format = ...)}
#'
#' @param string string to extract date from
#' @param mask defining where date is in string
#' @param tz timezone code to pass to ymd_hm
#'
#'
#' @author W. S. Drysdale
#'
#' @export


mask_extract_date = function(string, mask,tz = "UTC"){

  if(length(string) != length(mask))
    stop("string and mask are of Differing lengths")

  if(mask %>% stringr::str_detect("yyyy"))
    year_string = "yyyy"
  if(mask %>% stringr::str_detect("yy") & !mask %>% stringr::str_detect("yyyy"))
    year_string = "yy"

  if(stringr::str_count(mask,year_string) != 1)
    stop(paste0("There must be exactly 1 instance of ",year_string," in the mask"))

  if(stringr::str_count(mask,"mm") != 1)
    stop("There must be exactly 1 instance of mm in the mask")

  if(stringr::str_count(mask,"dd") != 1)
    stop("There must be exactly 1 instance of dd in the mask")

  if(stringr::str_count(mask,"HH") != 1)
    stop("There must be exactly 1 instance of HH in the mask")

  if(stringr::str_count(mask,"MM") != 1)
    stop("There must be exactly 1 instance of MM in the mask")

  if(stringr::str_count(mask,"SS") > 1)
    stop("There must be either 0 or 1 instances of SS in the mask")

  myyear  = substr(string,stringr::str_locate(mask,year_string)[1],stringr::str_locate(mask,year_string)[2])
  mymonth = substr(string,stringr::str_locate(mask,"mm")[1],stringr::str_locate(mask,"mm")[2])
  myday   = substr(string,stringr::str_locate(mask,"dd")[1],stringr::str_locate(mask,"dd")[2])
  myhour  = substr(string,stringr::str_locate(mask,"HH")[1],stringr::str_locate(mask,"HH")[2])
  mymin   = substr(string,stringr::str_locate(mask,"MM")[1],stringr::str_locate(mask,"MM")[2])

  mysec = NULL
  if(stringr::str_count(mask,"SS") == 1)
    mysec = substr(string,stringr::str_locate(mask,"SS")[1],stringr::str_locate(mask,"SS")[2])

  if(is.null(mysec))
    date = paste0(myyear,mymonth,myday,"_",myhour,mymin) %>% lubridate::ymd_hm(tz = tz)
  else
    date = paste0(myyear,mymonth,myday,"_",myhour,mymin,mysec) %>% lubridate::ymd_hms(tz = tz)

  #return
  date
}
