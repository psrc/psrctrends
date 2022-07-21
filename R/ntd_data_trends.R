#' Process NTD Monthly Transit Data
#'
#' This function processes Transit Agency monthly data from the Nation Transit Database.
#' Data is pulled monthly from "https://www.transit.dot.gov/sites/fta.dot.gov/files/".
#' 
#' @return tibble in long form of monthly Boardings, Revenue-Miles and Revenue-Hours by Agency and Region
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' ntd.data <- process_ntd_monthly_data()
#' 
#' @export
#'
process_ntd_monthly_data <- function() {
  
  ntd.tabs <- c("UPT","VRM", "VRH")
  psrc.transit <- c("1","3","5","20","23","29","35","40","54")
  bus.modes <- c("CB","MB","TB")
  ferry.modes <- c("FB")
  light.rail.modes <- c("LR","SR","MG","MO")
  commuter.rail.modes <- c("CR")
  vanpool.modes <- c("VP")
  
  today <- Sys.Date()
  c.yr <- lubridate::year(today)
  c.mo <- formatC(as.integer(lubridate::month(today)), width=2, flag="0")
  d.mo <- month.name[[as.integer(lubridate::month(today)) - 2]]
  
  data.url <- paste0("https://www.transit.dot.gov/sites/fta.dot.gov/files/",c.yr,"-",c.mo,"/",d.mo,"%20",c.yr,"%20Raw%20Database.xlsx")
  
  utils::download.file(data.url, "working.xlsx", quiet = TRUE, mode = "wb")
  data.file <- paste0(getwd(),"/working.xlsx")
  
  processed <- NULL
  for (areas in ntd.tabs) {
    
    t <- dplyr::as_tibble(openxlsx::read.xlsx(data.file, sheet = areas, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE)) %>%
      dplyr::filter(.data$`4.digit.NTD.ID` %in% psrc.transit, .data$Modes!="DR") %>%
      dplyr::mutate(Agency = gsub("Snohomish County Public Transportation Benefit Area Corporation","Community Transit",.data$Agency)) %>%
      dplyr::mutate(Agency = gsub("Central Puget Sound Regional Transit Authority","Sound Transit",.data$Agency)) %>%
      dplyr::mutate(Agency = gsub("King County Department of Metro Transit","King County Metro",.data$Agency)) %>%
      dplyr::mutate(Agency = gsub("Pierce County Transportation Benefit Area Authority","Pierce Transit",.data$Agency)) %>%
      dplyr::mutate(Agency = gsub("City of Everett","Everett Transit",.data$Agency)) %>%
      dplyr::mutate(Agency = gsub("King County Ferry District","King County Metro",.data$Agency)) %>%
      dplyr::select(-.data$`4.digit.NTD.ID`, -.data$`5.digit.NTD.ID`, -.data$Active, -.data$Reporter.Type, -.data$UZA, -.data$UZA.Name, -.data$TOS) %>%
      dplyr::rename(Agency_Modes=.data$Modes) %>%
      tidyr::pivot_longer(cols = -dplyr::contains("Agency"), names_to="date", values_to="estimate") %>%
      dplyr::rename(variable=.data$Agency_Modes) %>%
      dplyr::mutate(concept=areas) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(variable=dplyr::case_when(
        .data$variable %in% commuter.rail.modes ~ "Commuter Rail",
        .data$variable %in% light.rail.modes ~ "Light Rail, Streetcar & Monorail",
        .data$variable %in% bus.modes ~ "Bus",
        .data$variable %in% ferry.modes ~ "Ferry",
        .data$variable %in% vanpool.modes ~ "Vanpool")) %>%
      dplyr::mutate(concept=dplyr::case_when(
        .data$concept == "UPT" ~ "Transit Boardings",
        .data$concept == "VRM" ~ "Transit Revenue-Miles",
        .data$concept == "VRH" ~ "Transit Revenue-Hours")) %>%
      dplyr::mutate(year = as.character(2000 + as.integer(stringr::str_sub(.data$date, 4, 5)))) %>%
      dplyr::mutate(month = formatC(match(stringr::str_to_title(stringr::str_sub(.data$date, 1, 3)), month.abb), width=2, flag="0")) %>%
      dplyr::mutate(data_day=paste0(.data$year,"-",.data$month,"-01")) %>%
      dplyr::mutate(data_day=lubridate::ymd(.data$data_day)) %>%
      dplyr::mutate(equiv_day=paste0(c.yr,"-",.data$month,"-01")) %>%
      dplyr::mutate(equiv_day=lubridate::ymd(.data$equiv_day)) %>%
      dplyr::select(-.data$date) %>%
      dplyr::group_by(.data$Agency,.data$variable,.data$concept,.data$year,.data$month,.data$data_day,.data$equiv_day) %>%
      dplyr::summarize(estimate=sum(.data$estimate)) %>%
      dplyr::as_tibble()
    
    ifelse(is.null(processed), processed <- t, processed <- dplyr::bind_rows(processed,t))
  }
  
  r.mode <- processed %>%
    dplyr::select(-.data$Agency) %>%
    dplyr::group_by(.data$variable,.data$concept,.data$year,.data$month,.data$data_day,.data$equiv_day) %>%
    dplyr::summarize(estimate=sum(.data$estimate)) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(Agency="Region")
  
  r <- r.mode %>%
    dplyr::select(-.data$variable) %>%
    dplyr::group_by(.data$Agency,.data$concept,.data$year,.data$month,.data$data_day,.data$equiv_day) %>%
    dplyr::summarize(estimate=sum(.data$estimate)) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(variable="Total Transit")
  
  processed <- dplyr::bind_rows(list(processed, r.mode, r)) %>% dplyr::rename(geography=.data$Agency)
  file.remove(data.file)
  
  return(processed)
  
}

#' Summarize Transit Data by Urbanized Area
#'
#' This function processes Transit Agency monthly data from the Nation Transit Database and aggregate to the Urbanized Areas.
#' Data is pulled monthly from "https://www.transit.dot.gov/sites/fta.dot.gov/files/".
#' 
#' @param yr Four digit calendar year as string for Summary year
#' @param pop.limit Population threshold to use to filter down Urbanized Areas - defaults to 1,000,000
#' @param census.yr Four digit calendar year as string for Census Year for UZA population data - defaults to "2020"
#' @return tibble in long form of Year to Date (or annual if it is a compelte year) Boardings and Revenue-Hours by Urbanized Area
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' uza.data <- process_ntd_uza_data(yr="2022", census.yr="2020")
#' 
#' @export
#'

process_ntd_uza_data <- function(yr, pop.limit=1000000, census.yr="2020") {
  
  ntd.tabs <- c("UPT", "VRH")
  
  today <- Sys.Date()
  c.yr <- lubridate::year(today)
  c.mo <- formatC(as.integer(lubridate::month(today)), width=2, flag="0")
  d.mo <- month.name[[as.integer(lubridate::month(today)) - 2]]
  
  data.url <- paste0("https://www.transit.dot.gov/sites/fta.dot.gov/files/",c.yr,"-",c.mo,"/",d.mo,"%20",c.yr,"%20Raw%20Database.xlsx")
  
  utils::download.file(data.url, "working.xlsx", quiet = TRUE, mode = "wb")
  data.file <- paste0(getwd(),"/working.xlsx")
  uza.file <- system.file('extdata', 'uaz_ua_codes.xlsx', package='psrctrends')
  
  processed <- NULL
  for (areas in ntd.tabs) {
    
    t <- dplyr::as_tibble(openxlsx::read.xlsx(data.file, sheet = areas, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE)) %>%
      dplyr::filter(.data$Modes!="DR") %>%
      dplyr::select(-.data$`4.digit.NTD.ID`, -.data$`5.digit.NTD.ID`, -.data$Agency, -.data$Active, -.data$Reporter.Type, -.data$Modes, -.data$TOS, -.data$UZA) %>%
      tidyr::pivot_longer(cols = -dplyr::contains("UZA"), names_to="date", values_to="estimate") %>%
      tidyr::drop_na() %>%
      dplyr::group_by(.data$UZA.Name,.data$date) %>%
      dplyr::summarize(estimate=sum(.data$estimate)) %>%
      dplyr::as_tibble() %>%
      dplyr::filter(.data$UZA.Name != "Non-UZA") %>%
      dplyr::mutate(concept=areas) %>%
      dplyr::mutate(variable=dplyr::case_when(
        .data$concept == "UPT" ~ "Total Boardings",
        .data$concept == "VRM" ~ "Total Revenue-Miles",
        .data$concept == "VRH" ~ "Total Revenue-Hours")) %>%
      dplyr::mutate(concept="Transit by Urbanized Area") %>%
      dplyr::mutate(year = as.character(2000 + as.integer(stringr::str_sub(.data$date, 4, 5)))) %>%
      dplyr::mutate(month = formatC(match(stringr::str_to_title(stringr::str_sub(.data$date, 1, 3)), month.abb), width=2, flag="0")) %>%
      dplyr::mutate(data_day=paste0(.data$year,"-",.data$month,"-01")) %>%
      dplyr::mutate(data_day=lubridate::ymd(.data$data_day)) %>%
      dplyr::mutate(equiv_day=paste0(c.yr,"-",.data$month,"-01")) %>%
      dplyr::mutate(equiv_day=lubridate::ymd(.data$equiv_day)) %>%
      dplyr::select(-.data$date) %>%
      dplyr::filter(.data$year == yr)
    
    ifelse(is.null(processed), processed <- t, processed <- dplyr::bind_rows(processed,t))
    rm(t)
  }
  
  # Aggregate Data by UZA for Analysis Year
  processed <- processed %>%
    dplyr::select(-.data$month, -.data$data_day, -.data$equiv_day) %>%
    dplyr::group_by(.data$UZA.Name, .data$variable, .data$concept, .data$year) %>%
    dplyr::summarize(estimate=sum(.data$estimate)) %>%
    dplyr::as_tibble()
  
  # Add in UZA Population for per Capita Metrics
  u <- dplyr::as_tibble(openxlsx::read.xlsx(uza.file, sheet = "UZA_2010", skipEmptyRows = TRUE, startRow = 1, colNames = TRUE))
  
  ua.pop <- tidycensus::get_acs(geography = "urban area", year = as.integer(census.yr), variables = "B01001_001", survey="acs5") %>%
    dplyr::select(.data$GEOID, .data$estimate) %>%
    dplyr::rename(population=.data$estimate) %>%
    dplyr::mutate(GEOID=as.integer(.data$GEOID))
  
  u <- dplyr::left_join(u, ua.pop, by=c("UACE"="GEOID"))
  
  processed <- dplyr::left_join(processed, u , by=c("UZA.Name")) %>%
    dplyr::rename(geography=.data$UZA.Name) %>%
    dplyr::select(-.data$UZA, -.data$UACE) 
  
  boardings <- processed %>% 
    dplyr::filter(.data$variable == "Total Boardings") %>%
    dplyr::filter(.data$population >=pop.limit | .data$geography=="Bremerton, WA")
  
  per.capita <- processed %>% 
    dplyr::filter(.data$variable == "Total Boardings") %>% 
    dplyr::mutate(estimate = .data$estimate/.data$population) %>%
    dplyr::mutate(variable = "Boardings per Capita") %>%
    dplyr::filter(.data$population >=pop.limit | .data$geography=="Bremerton, WA")
  
  revenue.hours <- processed %>% 
    dplyr::filter(.data$variable == "Total Revenue-Hours") %>%
    dplyr::filter(.data$population >=pop.limit | .data$geography=="Bremerton, WA")
  
  hrs <- revenue.hours %>% 
    dplyr::select(.data$geography,.data$estimate) %>%
    dplyr::rename(hours=.data$estimate)
  
  rides.per.hr <- dplyr::left_join(boardings, hrs, by=c("geography")) %>%
    dplyr::mutate(estimate = .data$estimate/.data$hours) %>%
    dplyr::mutate(variable = "Boardings per Revenue Hour") %>%
    dplyr::select(-.data$hours)
  
  final <- dplyr::bind_rows(list(boardings,revenue.hours,per.capita,rides.per.hr))
  
  # Create a plot ID column for ease of use for plotting
  psrc.uza <- c("Seattle, WA","Bremerton, WA")
  sister.uza <- c("Denver-Aurora, CO", "Detroit, MI", "Minneapolis-St. Paul, MN-WI", "Phoenix-Mesa, AZ", "Portland, OR-WA", "San Diego, CA", "San Francisco-Oakland, CA", "Tampa-St. Petersburg, FL")
  big.uza <- c("Atlanta, GA", "Boston, MA-NH-RI","Dallas-Fort Worth-Arlington, TX", "Houston, TX", "Miami, FL", "Philadelphia, PA-NJ-DE-MD", "Washington, DC-VA-MD")
  biggest.uza <- c("Chicago, IL-IN", "Los Angeles-Long Beach-Anaheim, CA", "New York-Newark, NY-NJ-CT")
  
  final <- final %>%
    dplyr::mutate(plot_id = dplyr::case_when(
      .data$geography %in% psrc.uza ~ "PSRC Urban Areas",
      .data$geography %in% sister.uza ~ "Similar Urban Areas",
      .data$geography %in% big.uza ~ "Larger Urban Areas",
      .data$geography %in% biggest.uza ~ "Largest Urban Areas")) %>%
    dplyr::mutate(plot_id = tidyr::replace_na(.data$plot_id, "Other Urban Areas")) %>%
    dplyr::mutate(plot_id = stringr::str_wrap(.data$plot_id, width = 8))
  
  file.remove(data.file)
  
  return(final)
  
}

#' Summarize Year to Date Transit Data by Mode and Operator
#'
#' This function processes Transit Agency monthly data from the Nation Transit Database and aggregate to year to date.
#' Data is pulled monthly from "https://www.transit.dot.gov/sites/fta.dot.gov/files/".
#' 
#' @param c.yr Four digit calendar year as string for last year to analyze
#' @param c.mo Month for analysis in year to date calculations if the latest month isn't desired - defaults to NULL
#' @return tibble in long form of Year to Date (or annual if it is a complete year) of Trnasit Data by Mode and Operator
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' ntd.ytd <- process_ntd_year_to_date_data(c.yr="2022")
#' 
#' @export
#'
process_ntd_year_to_date_data <- function(c.yr, c.mo=NULL) {
  
  # Step 1 - Get monthly data from NTD using package
  monthly <- psrctrends::process_ntd_monthly_data()
  
  if (is.null(c.mo)) {
    
    latest.month <- monthly %>% 
      dplyr::select(.data$data_day) %>% 
      dplyr::filter(lubridate::year(.data$data_day) == c.yr) %>%
      dplyr::filter(lubridate::month(.data$data_day) == max(lubridate::month(.data$data_day))) %>%
      dplyr::distinct() %>%
      dplyr::pull() %>%
      lubridate::month()
    
    ytd <- monthly %>% 
      dplyr::filter(.data$year <= c.yr) %>%
      dplyr::filter(lubridate::month(.data$data_day) <= latest.month) %>%
      dplyr::select(-.data$month, -.data$data_day, -.data$equiv_day) %>%
      dplyr::group_by(.data$geography, .data$variable, .data$concept, .data$year) %>%
      dplyr::summarise(estimate=sum(.data$estimate)) %>%
      dplyr::as_tibble()
    
  } else {
    
    ytd <- monthly %>% 
      dplyr::filter(.data$year <= c.yr) %>%
      dplyr::filter(lubridate::month(.data$data_day) <= c.mo) %>%
      dplyr::select(-.data$month, -.data$data_day, -.data$equiv_day) %>%
      dplyr::group_by(.data$geography, .data$variable, .data$concept, .data$year) %>%
      dplyr::summarise(estimate=sum(.data$estimate)) %>%
      dplyr::as_tibble()
  }
  
  
  return(ytd)
  
}
