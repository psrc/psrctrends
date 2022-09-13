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
#' mpo_people_of_color <- calculate_mpo_people_of_color(census.yr=2018)
#' 
#' @export
#'
calculate_mpo_people_of_color <- function(census.yr=2020) {
  
  mpo.file <- system.file('extdata', 'regional-councils-counties.csv', package='psrctrends')
  
  # Total Population and Non-Hispanic White
  census.variables <- c("B03002_001","B03002_003")
  
  # Load File to figure out what counties are in which MPO
  mpo <- readr::read_csv(mpo.file, show_col_types = FALSE) %>% 
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
  mpo <- readr::read_csv(mpo.file, show_col_types = FALSE) %>% 
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

#' Hour long commute share by Regional Planning Entity
#'
#' This function calculates the Share of Commute trips that take at least 1 hour by 27 Regional Planning Entities.
#' 
#' @param census.yr Four digit integer for Census year for data - defaults to 2020
#' @return tibble of long commute trips by Regional Entity
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' mpo_hour_commutes <- calculate_mpo_long_tt_share(census.yr=2017)
#' 
#' @export
#'
calculate_mpo_long_tt_share <- function(census.yr=2020) {
  
  mpo.file <- system.file('extdata', 'regional-councils-counties.csv', package='psrctrends')
  
  # Travel Times over 60 minutes Variable
  census.variables <- c("B08303_001","B08303_012","B08303_013")
  
  # Load File to figure out what counties are in which MPO
  mpo <- readr::read_csv(mpo.file, show_col_types = FALSE) %>% 
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
  
  # Clean up Column Names
  mpo_county_data <- mpo_county_data %>%
    tidyr::pivot_wider(names_from = .data$variable, values_from = .data$estimate) %>%
    dplyr::mutate(Over_60_Minutes_Trips =(.data$B08303_012+.data$B08303_013)) %>%
    dplyr::rename(Commute_Trips=.data$B08303_001) %>%
    dplyr::select(.data$GEOID, .data$Commute_Trips, .data$Over_60_Minutes_Trips) 
  
  # Aggregate to MPO
  mpo_county_data <- dplyr::left_join(mpo, mpo_county_data, by="GEOID")
  
  mpo_data <- mpo_county_data %>%
    dplyr::select(-.data$MSA_FIPS, -.data$MSA_NAME, -.data$COUNTY_FIPS, -.data$COUNTY_NAME, -.data$STATE_FIPS, -.data$STATE_NAME, -.data$STATE_LONG_NAME, -.data$GEOID) %>%
    dplyr::group_by(.data$MPO_AREA, .data$MPO_FIPS, .data$MPO_NAME) %>%
    dplyr::summarise(dplyr::across(.fns = sum)) %>%
    dplyr::mutate(Over_60_Minutes_Share = .data$Over_60_Minutes_Trips/.data$Commute_Trips) %>%
    dplyr::as_tibble()
  
  # Convert to Long-Form
  l <- mpo_data %>%
    tidyr::pivot_longer(cols = c(-.data$MPO_AREA, -.data$MPO_FIPS, -.data$MPO_NAME), names_to = "variable", values_to = "estimate") %>%
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable,"_"," ")) %>%
    dplyr::mutate(year = census.yr)
  
  return(l)
  
}

#' Home Ownership by Regional Planning Entity
#'
#' This function calculates the Share of Home Ownership by 27 Regional Planning Entities.
#' 
#' @param census.yr Four digit integer for Census year for data - defaults to 2020
#' @return tibble of home ownership shares by Regional Entity
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' mpo_home_ownership <- calculate_mpo_ownership_share()
#' 
#' @export
#'
calculate_mpo_ownership_share <- function(census.yr=2020) {
  
  mpo.file <- system.file('extdata', 'regional-councils-counties.csv', package='psrctrends')
  
  # Home Ownership
  census.variables <- c("B25003_001","B25003_002")
  
  # Load File to figure out what counties are in which MPO
  mpo <- readr::read_csv(mpo.file, show_col_types = FALSE) %>% 
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
  
  # Clean up Column Names
  mpo_county_data <- mpo_county_data %>%
    tidyr::pivot_wider(names_from = .data$variable, values_from = .data$estimate) %>%
    dplyr::rename(Occupied_Housing_Units=.data$B25003_001, Owner_Occupied_Housing_Units=.data$B25003_002) %>%
    dplyr::select(.data$GEOID, .data$Occupied_Housing_Units, .data$Owner_Occupied_Housing_Units) 
  
  # Aggregate to MPO
  mpo_county_data <- dplyr::left_join(mpo, mpo_county_data, by="GEOID")
  
  mpo_data <- mpo_county_data %>%
    dplyr::select(-.data$MSA_FIPS, -.data$MSA_NAME, -.data$COUNTY_FIPS, -.data$COUNTY_NAME, -.data$STATE_FIPS, -.data$STATE_NAME, -.data$STATE_LONG_NAME, -.data$GEOID) %>%
    dplyr::group_by(.data$MPO_AREA, .data$MPO_FIPS, .data$MPO_NAME) %>%
    dplyr::summarise(dplyr::across(.fns = sum)) %>%
    dplyr::mutate(Owner_Occupied_Housing_Units_Share = .data$Owner_Occupied_Housing_Units/.data$Occupied_Housing_Units) %>%
    dplyr::as_tibble()
  
  # Convert to Long-Form
  l <- mpo_data %>%
    tidyr::pivot_longer(cols = c(-.data$MPO_AREA, -.data$MPO_FIPS, -.data$MPO_NAME), names_to = "variable", values_to = "estimate") %>%
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable,"_"," ")) %>%
    dplyr::mutate(year = census.yr)
  
  return(l)
  
}

#' Transit Performance by Regional Planning Entity
#'
#' This function calculates transit performance from the NTD by 27 Regional Planning Entities.
#' 
#' @param annual yes or no string to determine if the user only wants total annual results - defaults to no
#' @return tibble of transit performance by Regional Entity
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' mpo_transit <- calculate_mpo_transit(annual='yes')
#' 
#' @export
#'
calculate_mpo_transit <- function(annual='no') {
  
  options(dplyr.summarise.inform = FALSE)
  
  # Figure out which Transit Agencies serve which MPO's
  print("Figuring out which Transit agencies are in which Regional Entities.")
  agency.file <- system.file('extdata', 'transit-agency.csv', package='psrctrends')
  
  agencies <- readr::read_csv(agency.file, show_col_types = FALSE) %>%
    dplyr::mutate(NTDID = stringr::str_pad(string=.data$NTDID, width=5, pad="0", side=c("left")))
  
  ntd_ids <- agencies %>% dplyr::select(.data$NTDID) %>% dplyr::distinct() %>% dplyr::pull()
  
  # Get NTD Data for all agencies
  ntd.tabs <- c("UPT","VRM", "VRH")
  
  today <- Sys.Date()
  c.yr <- lubridate::year(today)
  c.dy <- lubridate::day(today)
  
  if(c.dy <= 7) {
    c.mo <- formatC(as.integer(lubridate::month(today))-1, width=2, flag="0")
    d.mo <- month.name[[as.integer(lubridate::month(today)) - 3]]
    
  } else {
    c.mo <- formatC(as.integer(lubridate::month(today)), width=2, flag="0")
    d.mo <- month.name[[as.integer(lubridate::month(today)) - 2]]
  }
  
  print(paste0("Downloading the NTD Monthly Raw Data Release for ", d.mo, " of ", c.yr))
  data.url <- paste0("https://www.transit.dot.gov/sites/fta.dot.gov/files/",c.yr,"-",c.mo,"/",d.mo,"%20",c.yr,"%20Raw%20Database.xlsx")
  utils::download.file(data.url, "working.xlsx", quiet = TRUE, mode = "wb")
  data.file <- paste0(getwd(),"/working.xlsx")
  
  # Determine if it is for Full Year only or Year to Date
  if (annual=='yes' & d.mo!="December") {
    
    c.yr <- c.yr-1
    d.mo <- "December"
    
  }
  
  processed <- NULL
  for (areas in ntd.tabs) {
    
    print(paste0("Working on ",areas))
    
    t <- dplyr::as_tibble(openxlsx::read.xlsx(data.file, sheet = areas, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE)) %>%
      dplyr::filter(.data$Modes!="DR") %>%
      dplyr::select(-.data$`4.digit.NTD.ID`, -.data$Agency, -.data$Active, -.data$Reporter.Type, -.data$Modes, -.data$TOS, -.data$UZA, -.data$UZA.Name) %>%
      dplyr::mutate(`5.digit.NTD.ID` = stringr::str_pad(string=.data$`5.digit.NTD.ID`, width=5, pad="0", side=c("left")))
    
    t[is.na(t)] <- 0
    
    # Group Data by NTD ID and Year  
    t <- t %>%
      dplyr::group_by(.data$`5.digit.NTD.ID`) %>%
      dplyr::summarise(dplyr::across(.fns = sum)) %>%
      dplyr::filter(.data$`5.digit.NTD.ID` %in% ntd_ids) %>%
      tidyr::pivot_longer(cols = -dplyr::contains("5.digit.NTD.ID"), names_to="date", values_to="estimate") %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(year = 2000 + as.integer(stringr::str_sub(.data$date, 4, 5))) %>%
      dplyr::filter(.data$year <= c.yr) %>%
      dplyr::mutate(month = formatC(match(stringr::str_to_title(stringr::str_sub(.data$date, 1, 3)), month.abb), width=2, flag="0")) %>%
      dplyr::mutate(data_day=paste0(.data$year,"-",.data$month,"-01")) %>%
      dplyr::mutate(data_day=lubridate::ymd(.data$data_day)) %>%
      dplyr::select(-.data$date, -.data$month) %>%
      dplyr::filter(lubridate::month(.data$data_day, label = TRUE, abbr = FALSE) <= d.mo) %>%
      dplyr::select(-.data$data_day) %>%
      dplyr::group_by(.data$`5.digit.NTD.ID`, .data$year) %>%
      dplyr::summarise(dplyr::across(.fns = sum)) %>%
      dplyr::as_tibble() 
    
    # Add MPO Data to Transit Agency ID and Group by MPO
    t <- dplyr::left_join(t, agencies, by=c("5.digit.NTD.ID"="NTDID")) %>%
      dplyr::select(.data$MPO_AREA, .data$MPO_FIPS, .data$MPO_NAME, .data$year, .data$estimate) %>%
      dplyr::group_by(.data$MPO_AREA, .data$MPO_FIPS, .data$MPO_NAME, .data$year) %>%
      dplyr::summarise(dplyr::across(.fns = sum)) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(variable=areas) %>%
      dplyr::mutate(variable=dplyr::case_when(
        .data$variable == "UPT" ~ "Total Boardings",
        .data$variable == "VRM" ~ "Total Revenue-Miles",
        .data$variable == "VRH" ~ "Total Revenue-Hours")) %>%
      dplyr::mutate(month=d.mo)
    
    ifelse(is.null(processed), processed <- t, processed <- dplyr::bind_rows(processed,t))
    
  }
  
  print("All done.")
  return(processed)
  
}
