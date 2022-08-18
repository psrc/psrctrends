#' Summarize Electric Vehicle Registrations by County & Year
#'
#' This function pulls and cleans data from the Washington State Registration Data.
#' Data is originally pulled from https://data.wa.gov/ via the Socrata API
#' 
#' @param states List of States by two digit code to keep - defaults to WA 
#' @param counties List of County Names to keep - defaults to PSRC counties
#' @return tibble of electric vehicle registrations by year and county
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' ev_registration <- get_ev_registration()
#' 
#' @export
#'
get_ev_registration <- function(states=c("WA"),counties=c("King","Kitsap","Pierce","Snohomish")) {
  
  numeric_cols <- c("BEV","PHEV","EV","NEV","TOT")
  
  d <- dplyr::as_tibble(RSocrata::read.socrata("https://data.wa.gov/resource/3d5d-sdqb.json", 
                                               app_token = "plpj1IICGOhTVjmebF1kls9Cf")) %>%
    dplyr::mutate(date = lubridate::ymd(.data$date)) %>%
    dplyr::filter(.data$state %in% states & .data$county %in% counties) %>%
    dplyr::select(-.data$percent_electric_vehicles) %>%
    dplyr::rename(BEV=.data$battery_electric_vehicles_bevs_) %>%
    dplyr::rename(PHEV=.data$plug_in_hybrid_electric_vehicles_phevs_) %>%
    dplyr::rename(EV=.data$electric_vehicle_ev_total) %>%
    dplyr::rename(NEV=.data$non_electric_vehicles) %>%
    dplyr::rename(TOT=.data$total_vehicles) %>%
    dplyr::rename(VEH_TYPE=.data$vehicle_primary_use) %>%
    dplyr::mutate_at(numeric_cols, as.numeric) 
  
  r <- d %>%
    dplyr::group_by(.data$date, .data$VEH_TYPE) %>%
    dplyr::summarise(dplyr::across(numeric_cols, sum)) %>%
    dplyr::mutate(county="PSRC Region", state="WA")
  
  d <- dplyr::bind_rows(d,r)
  
  t <- d %>%
    dplyr::group_by(.data$date, .data$county, .data$state) %>%
    dplyr::summarise(dplyr::across(numeric_cols, sum)) %>%
    dplyr::mutate(VEH_TYPE="Total")
  
  d <- dplyr::bind_rows(d,t) %>%
    dplyr::mutate(share=.data$EV/.data$TOT)
  
  return(d)
  
}
