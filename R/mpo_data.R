#' People of Color by Regional Planning Entity
#'
#' This function calculates people of color by 27 Regional Planning Entities.
#' 
#' @param census.yr Four digit integer for Census year for data - defaults to 2020
#' @return tibble of population and people of color by Regional Entity
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' mpo_poc <- calculate_mpo_people_of_color(census.yr=2018)
#' 
#' @export
#'
calculate_mpo_people_of_color <- function(census.yr=2020) {
  
  mpo.file <- system.file('extdata', 'regional-councils-counties.csv', package='psrctrends')
  
  # Total Population and Non-Hispanic White
  census.variables <- c("B03002_001","B03002_003")
  
  # Load File to figure out what counties are in which MPO
  mpo <- readr::read_csv(mpo.file) %>% 
    dplyr::mutate(COUNTY_FIPS=stringr::str_pad(.data$COUNTY_FIPS, width=3, side=c("left"), pad="0")) %>%
    dplyr::mutate(STATE_FIPS=stringr::str_pad(.data$STATE_FIPS, width=2, side=c("left"), pad="0")) %>%
    dplyr::mutate(GEOID = paste0(.data$STATE_FIPS,.data$COUNTY_FIPS))
  
  states <- mpo %>% dplyr::select(.data$STATE_FIPS) %>% dplyr::distinct() %>% dplyr::pull()
  counties <- mpo %>% dplyr::select(.data$GEOID) %>% dplyr::distinct() %>% dplyr::pull()
  
  # Download Census Data for each county in each MSA
  mpo_county_data <- NULL
  for (st in states) {
    c <- mpo %>% dplyr::filter(.data$STATE_FIPS %in% st) %>% dplyr::select(.data$COUNTY_FIPS) %>% dplyr::pull()
    d <- tidycensus::get_acs(geography = "county", state=st, county=c, variables = census.variables, year = census.yr, survey = "acs5") %>% dplyr::select(-.data$moe)
    ifelse(is.null(mpo_county_data), mpo_county_data <- d, mpo_county_data <- dplyr::bind_rows(mpo_county_data,d))
  }
  
  # Clean up Column Names and Calculate People of Color
  mpo_county_data <- mpo_county_data %>%
    tidyr::pivot_wider(names_from = .data$variable, values_from = .data$estimate) %>%
    dplyr::mutate(People_of_Color=(.data$B03002_001-.data$B03002_003)) %>%
    dplyr::rename(Population=.data$B03002_001, Non_Hispanic_White=.data$B03002_003) %>%
    dplyr::select(.data$GEOID, .data$Population, .data$People_of_Color, .data$Non_Hispanic_White) 
  
  # Aggregate to MPO
  mpo_county_data <- dplyr::left_join(mpo, mpo_county_data, by="GEOID")
  
  mpo_data <- mpo_county_data %>%
    dplyr::select(-.data$MSA_FIPS, -.data$MSA_NAME, -.data$COUNTY_FIPS, -.data$COUNTY_NAME, -.data$STATE_FIPS, -.data$STATE_NAME, -.data$STATE_LONG_NAME, -.data$GEOID) %>%
    dplyr::group_by(.data$MPO_AREA, .data$MPO_FIPS, .data$MPO_NAME) %>%
    dplyr::summarise(dplyr::across(.fns = sum)) %>%
    dplyr::mutate(People_of_Color_Share = .data$People_of_Color/.data$Population) %>%
    dplyr::as_tibble()
  
  # Convert to Long-Form
  l <- mpo_data %>%
    tidyr::pivot_longer(cols = c(-.data$MPO_AREA, -.data$MPO_FIPS, -.data$MPO_NAME), names_to = "variable", values_to = "estimate") %>%
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable,"_"," ")) %>%
    dplyr::mutate(year = census.yr)
  
  return(l)
  
}

#' Non-SOV Mode Share by Regional Planning Entity
#'
#' This function calculates the Non-SOV Mode Share by 27 Regional Planning Entities.
#' 
#' @param census.yr Four digit integer for Census year for data - defaults to 2020
#' @return tibble of commute trips and non-sov trips by Regional Entity
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' mpo_mode_share <- calculate_mpo_nonsov_share(census.yr=2019)
#' 
#' @export
#'
calculate_mpo_nonsov_share <- function(census.yr=2020) {
  
  mpo.file <- system.file('extdata', 'regional-councils-counties.csv', package='psrctrends')
  
  # Total Commute Trips and SOV Trips
  census.variables <- c("B08006_001","B08006_003")
  
  # Load File to figure out what counties are in which MPO
  mpo <- readr::read_csv(mpo.file) %>% 
    dplyr::mutate(COUNTY_FIPS=stringr::str_pad(.data$COUNTY_FIPS, width=3, side=c("left"), pad="0")) %>%
    dplyr::mutate(STATE_FIPS=stringr::str_pad(.data$STATE_FIPS, width=2, side=c("left"), pad="0")) %>%
    dplyr::mutate(GEOID = paste0(.data$STATE_FIPS,.data$COUNTY_FIPS))
  
  states <- mpo %>% dplyr::select(.data$STATE_FIPS) %>% dplyr::distinct() %>% dplyr::pull()
  counties <- mpo %>% dplyr::select(.data$GEOID) %>% dplyr::distinct() %>% dplyr::pull()
  
  # Download Census Data for each county in each MSA
  mpo_county_data <- NULL
  for (st in states) {
    c <- mpo %>% dplyr::filter(.data$STATE_FIPS %in% st) %>% dplyr::select(.data$COUNTY_FIPS) %>% dplyr::pull()
    d <- tidycensus::get_acs(geography = "county", state=st, county=c, variables = census.variables, year = census.yr, survey = "acs5") %>% dplyr::select(-.data$moe)
    ifelse(is.null(mpo_county_data), mpo_county_data <- d, mpo_county_data <- dplyr::bind_rows(mpo_county_data,d))
  }
  
  # Clean up Column Names and Calculate Non-SOV Share
  mpo_county_data <- mpo_county_data %>%
    tidyr::pivot_wider(names_from = .data$variable, values_from = .data$estimate) %>%
    dplyr::mutate(NonSOV_Trips=(.data$B08006_001-.data$B08006_003)) %>%
    dplyr::rename(Commute_Trips=.data$B08006_001, SOV_Trips=.data$B08006_003) %>%
    dplyr::select(.data$GEOID, .data$Commute_Trips, .data$NonSOV_Trips, .data$SOV_Trips) 
  
  # Aggregate to MPO
  mpo_county_data <- dplyr::left_join(mpo, mpo_county_data, by="GEOID")
  
  mpo_data <- mpo_county_data %>%
    dplyr::select(-.data$MSA_FIPS, -.data$MSA_NAME, -.data$COUNTY_FIPS, -.data$COUNTY_NAME, -.data$STATE_FIPS, -.data$STATE_NAME, -.data$STATE_LONG_NAME, -.data$GEOID) %>%
    dplyr::group_by(.data$MPO_AREA, .data$MPO_FIPS, .data$MPO_NAME) %>%
    dplyr::summarise(dplyr::across(.fns = sum)) %>%
    dplyr::mutate(NonSOV_Share = .data$NonSOV_Trips/.data$Commute_Trips) %>%
    dplyr::as_tibble()
  
  # Convert to Long-Form
  l <- mpo_data %>%
    tidyr::pivot_longer(cols = c(-.data$MPO_AREA, -.data$MPO_FIPS, -.data$MPO_NAME), names_to = "variable", values_to = "estimate") %>%
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable,"_"," ")) %>%
    dplyr::mutate(year = census.yr)
  
  return(l)
  
}

