#' Process SEA Operations Data
#'
#' This function process passenger, air cargo and air operations monthly data for SEA.
#' The data is based on the Monthly Passenger, Cargo and Operations Summary files.
#' Monthly data is stored on PSRC servers at "X:/DSA/psrc-trends-data/monthly-seatac".
#' Data is pulled monthly from "https://www.portseattle.org/page/airport-statistics".
#' 
#' @param c.yr Current four digit calendar year as integer
#' @param c.mo Current month as integer, no leading zeros
#' @param f.yr First four digit calendar year as integer of data to process - defaults to 2019
#' @return tibble in long form of SeaTac Airport operations data
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' airport.operations <- process_sea_operations_data(c.yr=2022, c.mo=4)
#' 
#' @export
#'
process_sea_operations_data <- function(c.yr, c.mo, f.yr=2019) {
  
  #utils::globalVariables(c("month", "day"))
  
  complete.years <- seq(f.yr, c.yr-1)
  complete.months <- seq(1,12)
  current.months <- seq(1,c.mo)
  
  combine_vectors <- function(c1, c2) {
    c3<- paste0(formatC(c1, width=2, flag="0"),c2)
    return(c3)
  }
  
  # Previous Complete Years
  create_list <- purrr::partial(combine_vectors, c1=complete.months)
  full.yr <- unlist(purrr::map(complete.years,create_list))
  
  # Current Year to Date
  create_list <- purrr::partial(combine_vectors, c1=current.months)
  current.yr <- unlist(purrr::map(c.yr,create_list))
  full.yr <- c(full.yr, current.yr)  
  
  processed <- NULL
  for (yr in full.yr) {
    t <- readxl::read_excel(paste0('X:/DSA/psrc-trends-data/monthly-seatac/',paste0('traf-ops-',yr,'.xls')), range = "A4:C54") %>%
      dplyr::select(1,3) %>%
      stats::setNames(c("Variable","Estimate")) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(year=stringr::str_sub(yr, 3, 6)) %>%
      dplyr::mutate(month=stringr::str_sub(yr, 1, 2)) %>%
      dplyr::mutate(day=paste0(c.yr,"-",.data$month,"-01")) %>%
      dplyr::mutate(day=lubridate::ymd(.data$day))
    
    ifelse(is.null(processed), processed <- t, processed <- dplyr::bind_rows(processed,t))
    rm(t)
  }
  
  return(processed)
}
