#' Summarize Electric Vehicle Registrations by County by Month
#'
#' This function pulls and cleans data from the Washington State Registration Data.
#' Data is originally pulled from https://data.wa.gov/ via the Socrata API
#' 
#' @param st List of States by two digit code to keep - defaults to WA 
#' @param ct List of County Names to keep - defaults to PSRC counties
#' @return tibble of electric vehicle registrations by month and county
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' @importFrom tidyselect all_of
#' 
#' @examples
#' 
#' ev_registration <- get_ev_registration_monthly()
#' 
#' @export
#'
get_ev_registration_monthly <- function(st=c("WA"), ct=c("King","Kitsap","Pierce","Snohomish")) {
  
  numeric_cols <- c("BEV","PHEV","EV","NEV","TOT")
  
  d <- dplyr::as_tibble(RSocrata::read.socrata("https://data.wa.gov/resource/3d5d-sdqb.json", 
                                               app_token = "plpj1IICGOhTVjmebF1kls9Cf")) %>%
    dplyr::mutate(date = lubridate::ymd(.data$date)) %>%
    dplyr::filter(.data$state %in% st & .data$county %in% ct) %>%
    dplyr::select(-.data$percent_electric_vehicles) %>%
    dplyr::rename(BEV=.data$battery_electric_vehicles_bevs_) %>%
    dplyr::rename(PHEV=.data$plug_in_hybrid_electric_vehicles_phevs_) %>%
    dplyr::rename(EV=.data$electric_vehicle_ev_total) %>%
    dplyr::rename(NEV=.data$non_electric_vehicles) %>%
    dplyr::rename(TOT=.data$total_vehicles) %>%
    dplyr::rename(VEH_TYPE=.data$vehicle_primary_use) %>%
    dplyr::mutate_at(all_of(numeric_cols), as.numeric) 
  
  r <- d %>%
    dplyr::group_by(.data$date, .data$VEH_TYPE) %>%
    dplyr::summarise(dplyr::across(all_of(numeric_cols), sum)) %>%
    dplyr::mutate(county="PSRC Region", state="WA")
  
  d <- dplyr::bind_rows(d,r)
  
  t <- d %>%
    dplyr::group_by(.data$date, .data$county, .data$state) %>%
    dplyr::summarise(dplyr::across(all_of(numeric_cols), sum)) %>%
    dplyr::mutate(VEH_TYPE="Electric Vehicle Registrations")
  
  d <- dplyr::bind_rows(d,t) %>%
    dplyr::mutate(share=.data$EV/.data$TOT) %>%
    dplyr::filter(.data$VEH_TYPE=="Electric Vehicle Registrations") %>%
    dplyr::rename(variable=.data$VEH_TYPE, all_vehicles=.data$TOT) %>%
    dplyr::mutate(yr=as.character(lubridate::year(.data$date))) %>%
    dplyr::mutate(mo=lubridate::month(.data$date, label=TRUE, abbr=FALSE)) %>%
    dplyr::select(-.data$BEV, -.data$PHEV, -.data$NEV)
  
  min.date <- d %>% dplyr::select(.data$date) %>% dplyr::pull() %>% min()
  
  d <- d %>%
    dplyr::arrange(.data$county, .data$variable, .data$date) %>%
    dplyr::mutate(delta = .data$EV - dplyr::lag(.data$EV)) %>%
    dplyr::mutate(delta = dplyr::case_when(
      .data$date == min.date ~ 0,
      .data$date != min.date ~ .data$delta))
  
  d <- dplyr::na_if(d,0)
  
  return(d)
  
}

#' Summarize Electric Vehicle Registrations by County by Year to Date
#'
#' This function pulls and cleans data from the Washington State Registration Data.
#' Data is originally pulled from https://data.wa.gov/ via the Socrata API
#' 
#' @param yrs Integer max year if not max from data - default is null
#' @param mos Integer month if not max from data - default is null
#' @param st List of States by two digit code to keep - defaults to WA 
#' @param ct List of County Names to keep - defaults to PSRC counties
#' @return tibble of vehicle registrations by year for selected month
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' ev_registration_ytd_latest <- get_ev_registration_ytd()
#' ev_registration_ytd_2021 <- get_ev_registration_ytd(yr=2021)
#' ev_registration_ytd_april <- get_ev_registration_ytd(mo=4)
#' 
#' @export
#'
get_ev_registration_ytd <- function(yrs=NULL, mos=NULL, st=c("WA"), ct=c("King","Kitsap","Pierce","Snohomish")) {
  
  numeric_cols <- c("BEV","PHEV","EV","NEV","TOT")
  d <- psrctrends::get_ev_registration_monthly(st,ct)
  
  if (is.null(yrs)) {
    yrs <- d %>% 
      dplyr::select(.data$date) %>% 
      dplyr::pull() %>%
      max() %>%
      lubridate::year()
  }
  
  if (is.null(mos)) {
    mos <- d %>% 
      dplyr::select(.data$date) %>% 
      dplyr::pull() %>%
      max() %>%
      lubridate::month()
  }
  
  d <- d %>%
    dplyr::filter(lubridate::month(.data$date)==mos & lubridate::year(.data$date)<=yrs)
  
  min.date <- d %>% dplyr::select(.data$date) %>% dplyr::pull() %>% min()
  
  d <- d %>%
    dplyr::arrange(.data$county, .data$variable, .data$date) %>%
    dplyr::mutate(delta = .data$EV - dplyr::lag(.data$EV)) %>%
    dplyr::mutate(delta = dplyr::case_when(
      .data$date == min.date ~ 0,
      .data$date != min.date ~ .data$delta))
  
  d <- dplyr::na_if(d,0)
  
  return(d)
  
}