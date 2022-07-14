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