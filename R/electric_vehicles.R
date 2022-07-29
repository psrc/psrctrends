#' Summarize Electric Vehicle Registrations by City & Year
#'
#' This function pulls and cleans data from the Washington State Registration Data.
#' Data is originally pulled from https://www.atlasevhub.com/materials/state-ev-registration-data/#data
#' 
#' @param ev.url Location of the registration data from Washington State - defaults to X drive 
#' @return tibble of electric vehicle registrations by year and city
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' ev_city <- get_ev_registrations_city(ev.url = "C:/Users/chelmann/OneDrive - Puget Sound Regional Council/data-downloads/wa_ev_registrations_public.csv")
#' 
#' @export
#'
get_ev_registrations_city <- function (ev.url="X:/DSA/psrc-trends-data/ev-registrations/wa_ev_registrations_public.csv") {
  
  # Electric Vehicle Registrations in Washington State
  url <- ev.url
  ev.regs <- dplyr::as_tibble(data.table::fread(url)) %>%
    stats::setNames(c("vehid","zipcode","registration_date","vin_prefix","vin_model_yr","dmv_id","dmv_id_comp","dmv_snap","exp", "st", "geo","vehicle_name","technology")) %>%
    dplyr::select(.data$vehid, .data$zipcode, .data$registration_date, .data$vehicle_name, .data$technology) %>%
    dplyr::mutate(year = lubridate::year(.data$registration_date))
  
  # Zipcodes
  url <- "X:/DSA/psrc-trends-data/ev-registrations/wa-zipcodes.csv"
  zips <- dplyr::as_tibble(data.table::fread(url)) %>% 
    dplyr::filter(.data$psrc==1) %>%
    dplyr::select(-.data$psrc) %>%
    dplyr::mutate(zipcode=as.character(.data$zipcode))
  
  # Join with zipcodes to get registrations by PSRC Counties
  ev.regs <- dplyr::left_join(ev.regs, zips, by=c("zipcode")) %>%
    tidyr::drop_na() %>%
    dplyr::select(.data$city, .data$technology, .data$year) %>%
    dplyr::mutate(registrations=1) %>%
    dplyr::group_by(.data$city, .data$technology, .data$year) %>%
    dplyr::summarise(registrations=sum(.data$registrations))
  
  return(ev.regs)
  
}

#' Summarize Electric Vehicle Registrations by County & Year
#'
#' This function pulls and cleans data from the Washington State Registration Data.
#' Data is originally pulled from https://www.atlasevhub.com/materials/state-ev-registration-data/#data
#' 
#' @param ev.url Location of the registration data from Washington State - defaults to X drive 
#' @return tibble of electric vehicle registrations by year and county
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' ev_county <- get_ev_registrations_county(ev.url = "C:/Users/chelmann/OneDrive - Puget Sound Regional Council/data-downloads/wa_ev_registrations_public.csv")
#' 
#' @export
#'
get_ev_registrations_county <- function (ev.url="X:/DSA/psrc-trends-data/ev-registrations/wa_ev_registrations_public.csv") {
  
  # Electric Vehicle Registrations in Washington State
  url <- ev.url
  ev.regs <- dplyr::as_tibble(data.table::fread(url)) %>%
    stats::setNames(c("vehid","zipcode","registration_date","vin_prefix","vin_model_yr","dmv_id","dmv_id_comp","dmv_snap","exp", "st", "geo","vehicle_name","technology")) %>%
    dplyr::select(.data$vehid, .data$zipcode, .data$registration_date, .data$vehicle_name, .data$technology) %>%
    dplyr::mutate(year = lubridate::year(.data$registration_date))
  
  # Zipcodes
  url <- "X:/DSA/psrc-trends-data/ev-registrations/wa-zipcodes.csv"
  zips <- dplyr::as_tibble(data.table::fread(url)) %>% 
    dplyr::filter(.data$psrc==1) %>%
    dplyr::select(-.data$psrc) %>%
    dplyr::mutate(zipcode=as.character(.data$zipcode))
  
  # Join with zipcodes to get registrations by PSRC Counties
  ev.regs <- dplyr::left_join(ev.regs, zips, by=c("zipcode")) %>%
    tidyr::drop_na() %>%
    dplyr::select(.data$county, .data$technology, .data$year) %>%
    dplyr::mutate(registrations=1) %>%
    dplyr::group_by(.data$county, .data$technology, .data$year) %>%
    dplyr::summarise(registrations=sum(.data$registrations))
  
  # Create Region Total
  region <- ev.regs %>%
    dplyr::select(-.data$county) %>%
    dplyr::group_by(.data$technology, .data$year) %>%
    dplyr::summarise(registrations=sum(.data$registrations)) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(county="Region")
  
  # Join Region
  ev.regs <- dplyr::bind_rows(ev.regs, region) %>%
    dplyr::mutate(year=as.character(.data$year))
  
  return(ev.regs)
  
}

#' Summarize Electric Vehicle Registrations by County & Year to Date
#'
#' This function pulls and cleans data from the Washington State Registration Data.
#' Data is originally pulled from https://www.atlasevhub.com/materials/state-ev-registration-data/#data
#' 
#' @param ev.url Location of the registration data from Washington State - defaults to X drive 
#' @return tibble of electric vehicle registrations by year to date and county for latest month in dataset
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' ev.county.ytd <- get_ev_registrations_county_ytd(ev.url = "C:/Users/chelmann/OneDrive - Puget Sound Regional Council/data-downloads/wa_ev_registrations_public.csv")
#' 
#' @export
#'
get_ev_registrations_county_ytd <- function (ev.url="X:/DSA/psrc-trends-data/ev-registrations/wa_ev_registrations_public.csv") {
  
  # Electric Vehicle Registrations in Washington State
  url <- ev.url
  ev.regs <- dplyr::as_tibble(data.table::fread(url)) %>%
    stats::setNames(c("vehid","zipcode","registration_date","vin_prefix","vin_model_yr","dmv_id","dmv_id_comp","dmv_snap","exp", "st", "geo","vehicle_name","technology")) %>%
    dplyr::select(.data$vehid, .data$zipcode, .data$registration_date, .data$vehicle_name, .data$technology) %>%
    dplyr::mutate(year = lubridate::year(.data$registration_date)) %>% 
    dplyr::mutate(month = lubridate::month(.data$registration_date))
  
  # Figure out the latest date for Year to Date Calculations
  max.yr <- ev.regs %>% dplyr::select(.data$year) %>% max() 
  max.mo <- ev.regs %>% dplyr::filter(.data$year==max.yr) %>% dplyr::select(.data$month) %>% max()
  ev.regs <- ev.regs %>% dplyr::filter(.data$month <= max.mo)
  
  # Zipcodes
  url <- "X:/DSA/psrc-trends-data/ev-registrations/wa-zipcodes.csv"
  zips <- dplyr::as_tibble(data.table::fread(url)) %>% 
    dplyr::filter(.data$psrc==1) %>%
    dplyr::select(-.data$psrc) %>%
    dplyr::mutate(zipcode=as.character(.data$zipcode))
  
  # Join with zipcodes to get registrations by PSRC Counties
  ev.regs <- dplyr::left_join(ev.regs, zips, by=c("zipcode")) %>%
    tidyr::drop_na() %>%
    dplyr::select(.data$county, .data$technology, .data$year) %>%
    dplyr::mutate(registrations=1) %>%
    dplyr::group_by(.data$county, .data$technology, .data$year) %>%
    dplyr::summarise(registrations=sum(.data$registrations))
  
  # Create Region Total
  region <- ev.regs %>%
    dplyr::select(-.data$county) %>%
    dplyr::group_by(.data$technology, .data$year) %>%
    dplyr::summarise(registrations=sum(.data$registrations)) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(county="Region")
  
  # Join Region
  ev.regs <- dplyr::bind_rows(ev.regs, region) %>%
    dplyr::mutate(year=as.character(.data$year))
  
  return(ev.regs)
  
}
