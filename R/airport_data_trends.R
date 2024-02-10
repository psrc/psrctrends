#' Download SEA Operations Data
#'
#' This function downloads passenger, air cargo and air operations monthly data for SEA.
#' The data is based on the Monthly Passenger, Cargo and Operations Summary files.
#' Data is pulled monthly from "https://www.portseattle.org/page/airport-statistics".
#' 
#' @param monthyear Six digit month+year date as string - example "012019"
#' @return downloaded file for current month
#' 
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' download_sea_operations_data(monthyear="042020")
#' }
#' @export
#'
download_sea_operations_data <- function(monthyear) {
  
  data_dir <- getwd()
  
  tryCatch(
    {settings <- list(dir = data_dir,
                      file_name = paste0("traf-ops-", monthyear, ".xls"),
                      url ="https://www.portseattle.org/pos/StatisticalReports/Public/"
    )
    
    local_file <- "working.xls"
    url_file <- paste0(settings$url, settings$file_name)
    utils::download.file(url_file, file.path(settings$dir, local_file), mode="wb")
    },
    error=function(cond) {
      message("File does not exist")
    }
  )
  
}

#' Process SEA Operations Data
#'
#' This function process passenger, air cargo and air operations monthly data for SEA.
#' The data is based on the Monthly Passenger, Cargo and Operations Summary files.
#' 
#' @param yr1 First four digit calendar year as integer of data to process - defaults to 2019
#' @return tibble in long form of SeaTac Airport operations data
#' 
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' airport_operations <- process_sea_operations_data(yr1=2022)}
#' 
#' @export
#'
process_sea_operations_data <- function(yr1=2019) {
  
  # Categoires for data cleanup
  summary_categories <- c("Subtotal - Domestic Passengers", "Subtotal - International Passengers", 
                          "Subtotal - Domestic Air Freight", "Subtotal - International Air Freight", 
                          "PASSENGER GRAND TOTAL", "CARGO GRAND TOTAL")
  
  # Data lags two months, adjusting based on which month it is
  if (lubridate::month(Sys.Date()) <= 2) {
    c_yr <- lubridate::year(Sys.Date()) - 1  
  }
  
  if (lubridate::month(Sys.Date()) == 1) {
    c_mo <- 11
  }
  
  if (lubridate::month(Sys.Date()) == 2) {
    c_mo <- 12
  }
  
  if (lubridate::month(Sys.Date()) >= 3) {
    c_mo <- lubridate::month(Sys.Date())
  }
  
  if (c_mo < 12) {
    complete_years <- seq(yr1, c_yr-1)
  } else {
    complete_years <- seq(yr1, c_yr)
  }
  
  complete_months <- seq(1,12)
  current_months <- seq(1,c_mo)
  
  combine_vectors <- function(c1, c2) {
    c3<- paste0(formatC(c1, width=2, flag="0"),c2)
    return(c3)
  }
  
  # Previous Complete Years
  create_list <- purrr::partial(combine_vectors, c1=complete_months)
  full_yr <- unlist(purrr::map(complete_years, create_list))
  
  # Current Year to Date
  create_list <- purrr::partial(combine_vectors, c1=current_months)
  current_yr <- unlist(purrr::map(c_yr,create_list))
  full_yr <- c(full_yr, current_yr)  
  full_yr <- unique(full_yr)
  
  processed <- NULL
  for (yr in full_yr) {
    download_sea_operations_data(yr)
    t <- readxl::read_excel("working.xls", range = "A4:C54") |>
      dplyr::select(1,3) |>
      stats::setNames(c("variable","estimate")) |>
      tidyr::drop_na() |>
      dplyr::mutate(year=stringr::str_sub(yr, 3, 6)) |>
      dplyr::mutate(month=stringr::str_sub(yr, 1, 2)) |>
      dplyr::mutate(date=paste0(.data$year,"-",.data$month,"-01")) |>
      dplyr::mutate(date=lubridate::ymd(.data$date)) |>
      dplyr::mutate(geography="Seattle-Tacoma International Airport") |>
      dplyr::mutate(grouping="Monthly") |>
      dplyr::select(-"month")
    
    ifelse(is.null(processed), processed <- t, processed <- dplyr::bind_rows(processed,t))
    rm(t)
    file.remove("working.xls")
  }
  
  # Final cleanup of Monthly Data
  processed <- processed |> 
    dplyr::filter(.data$variable %in% summary_categories) |>
    dplyr::mutate(variable = stringr::str_remove_all(.data$variable, "Subtotal - ")) |>
    dplyr::mutate(variable = stringr::str_to_title(.data$variable)) |>
    dplyr::mutate(metric = dplyr::case_when(
      stringr::str_detect(.data$variable, "Passenger") ~ "Passengers",
      stringr::str_detect(.data$variable, "Cargo") ~ "Cargo",
      stringr::str_detect(.data$variable, "Freight") ~ "Cargo")) |>
    dplyr::mutate(variable = dplyr::case_when(
      stringr::str_detect(.data$variable, "Domestic ") ~ "Domestic",
      stringr::str_detect(.data$variable, "International ") ~ "International",
      stringr::str_detect(.data$variable, "Grand") ~ "Total")) |>
    dplyr::mutate(geography_type = "Region")
  
  # Create Annual Summary
  annual <- processed |>
    dplyr::filter(.data$year %in% complete_years) |>
    dplyr::group_by(.data$year, .data$variable, .data$metric) |>
    dplyr::summarise(estimate = sum(.data$estimate)) |>
    dplyr::as_tibble() |>
    dplyr::mutate(grouping="Annual", geography="Seattle-Tacoma International Airport", geography_type="Region", date=lubridate::ymd(paste0(.data$year, "-12-01")))
  
  # Create Year to Date Summary
  ytd <- processed |>
    dplyr::filter(lubridate::month(.data$date) <= c_mo) |>
    dplyr::group_by(.data$year, .data$variable, .data$metric) |>
    dplyr::summarise(estimate = sum(.data$estimate)) |>
    dplyr::as_tibble() |>
    dplyr::mutate(grouping="YTD", geography="Seattle-Tacoma International Airport", geography_type="Region", date=lubridate::ymd(paste0(.data$year, "-", c_mo, "-01")))
  
  processed <- dplyr::bind_rows(processed, annual, ytd) |> dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate")
  
  return(processed)
}

#' Process National Passenger Enplanement Data
#'
#' This function processes passenger annual data for FAA.
#' 
#' @param yr1 First four digit calendar year as integer of data to process - defaults to 2019
#' @param metric Either "passengers" or "cargo" - defaults to "passengers"
#' @return tibble in long form of FAA Airport Passenger data
#' 
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' national_airport_passengers <- process_faa_data()}
#' 
#' @export
#'
process_faa_data <- function(yr1=2019, metric="passengers") {
  
  file_path <- paste0("X:/DSA/psrc-trends-data/national-airports/", metric, "/")
  
  processed <- NULL
  for (file in list.files(path = file_path, pattern = ".*.xlsx")) {
    
    # Get year from filename
    y <- paste0("20",stringr::str_sub(file, start=3, end=4))
    
    # Open excel file
    t <- readxl::read_excel(paste0(file_path, file)) 
    
    # Estimate is in a different column for passenger and cargo data
    if (metric == "passengers") {
      t <- t |> dplyr::select(6,9)
    } else {
      t <- t |> dplyr::select(6,10)
    }
    
    # Clean up column names and add dates
    t <- t |>
      stats::setNames(c("geography", "estimate")) |>
      tidyr::drop_na() |>
      dplyr::mutate(year = y, date = lubridate::ymd(paste0(y,"-12-01")), geography_type = "Metro Airports", variable = "Total", metric = stringr::str_to_title(metric), grouping = "Annual")
    
    # Convert cargo from pounds to metric tons
    if (metric == "cargo") {
      t <- t |> dplyr::mutate(estimate = round(.data$estimate * 0.00045359, 0))
    }
    
    # Append to previous years data  
    ifelse(is.null(processed), processed <- t, processed <- dplyr::bind_rows(processed,t))
    rm(t)
    
  }
  
  processed <- processed |> dplyr::filter(.data$year >= yr1) |> dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate")
 
  return(processed) 
}

#' Summarize Airport Data
#'
#' This function summarizes passenger, air cargo and air operations monthly data for SEA.
#' The data is based on the Monthly Passenger, Cargo and Operations Summary files.
#' 
#' @param yr1 First four digit calendar year as integer of data to process - defaults to 2019
#' @return tibble in long form of SeaTac Airport operations data
#' 
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' airport_data <- summarize_airport_data(yr1=2022)}
#' 
#' @export
#'
summarize_airport_data <- function(yr1=2019) {
  
  seatac <- process_sea_operations_data(yr1)
  national_passengers <- process_faa_data(yr1, metric="passengers")
  national_cargo <- process_faa_data(yr1, metric="cargo")
  processed <- dplyr::bind_rows(seatac, national_passengers, national_cargo)

  return(processed)
}
