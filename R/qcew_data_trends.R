#' Process QCEW Monthly MSA Data
#'
#' This function processes MSA monthly data from QCEW.
#' Data is pulled monthly from "https://media.esd.wa.gov/esdwa/Default/ESDWAGOV/labor-market-info/Libraries/Economic-reports/Washington-employment-estimates/".
#' 
#' @param yr Current four digit calendar year as integer
#' @param mo Current month as integer, no leading zeros
#' @return tibble in long form of QCEW Monthly Employment data by MSA+
#' 
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' jobs_data <- process_qcew_monthly_msa(yr=2023, mo=12)}
#' 
#' @export
#'
process_qcew_monthly_msa <- function(yr, mo) {
  
  qcew_areas <- c("Washington State", "Seattle MSA", "Tacoma MSA", "Bremerton MSA")
  goods_producing <- c("Mining and Logging", "Construction", "Manufacturing")
  private_services <- c("Trade, Transportation, and Utilities", "Information", "Financial Activities", "Professional and Business Services","Educational and Health Services", "Leisure and Hospitality", "Other Services")
  government_services <- c("Government")
  job_categories <- c(goods_producing, private_services,government_services)
  data_url <- paste0("https://media.esd.wa.gov/esdwa/Default/ESDWAGOV/labor-market-info/Libraries/Economic-reports/Washington-employment-estimates/WS-QB-historical-SA-all%20areas",mo, yr-2000,".xlsx")
  
  utils::download.file(data_url, "working.xlsx", quiet = TRUE, mode = "wb")
  data_file <- paste0(getwd(),"/working.xlsx")
  
  processed <- NULL
  for (areas in qcew_areas) {
    
    t <- readxl::read_excel(data_file, sheet = areas, skip = 1, col_names = TRUE) |>
      tidyr::pivot_longer(cols=!c(.data$`NAICS CELL`, .data$`NAICS INDUSTRY`), names_to = "date", values_to ="estimate") |>
      dplyr::mutate(date = as.Date(as.numeric(.data$date), origin = "1899-12-30")) |>
      dplyr::mutate(geography=areas) |>
      dplyr::rename(variable="NAICS INDUSTRY") |>
      dplyr::select(-"NAICS CELL") |>
      dplyr::mutate(variable = trimws(.data$variable, "both"))
      
    if (areas == "Bremerton MSA") {
      
      private_total <- t |> dplyr::filter(.data$variable=="Private Service Providing") 
      
      detailed_private <- t |> 
        dplyr::filter(.data$variable %in% private_services) |>
        dplyr::group_by(.data$date) |>
        dplyr::summarize(detailed_jobs=sum(.data$estimate)) |>
        dplyr::as_tibble()
      
      private_total <- dplyr::left_join(private_total, detailed_private, by=c("date")) |>
        dplyr::mutate(estimate = .data$estimate - .data$detailed_jobs) |>
        dplyr::mutate(variable = "Other Services") |>
        dplyr::select(-"detailed_jobs")
      
      t <- dplyr::bind_rows(t, private_total)
      
      t <- t |> dplyr::mutate(variable = gsub("Mining, Logging, and Construction", "Construction", .data$variable))
      rm(detailed_private, private_total)
      
    }
    
    ifelse(is.null(processed), processed <- t, processed <- dplyr::bind_rows(processed,t))
    rm(t)
    
  }
  
  r <- processed |>
    dplyr::filter(!(.data$geography %in% c("Washington State"))) |>
    dplyr::group_by(.data$variable,.data$date) |>
    dplyr::summarize(estimate=sum(.data$estimate)) |>
    dplyr::as_tibble() |>
    dplyr::mutate(geography="Region")
  
  processed <- dplyr::bind_rows(processed,r) |>
    dplyr::mutate(year = lubridate::year(.data$date), month = paste0(lubridate::month(.data$date))) |> 
    dplyr::mutate(year=as.character(.data$year)) |>
    dplyr::mutate(month=as.integer(.data$month)) |>
    dplyr::mutate(month=formatC(.data$month, width=2, flag="0")) |>
    dplyr::mutate(month=as.character(.data$month)) |>
    dplyr::mutate(metric="Wage and Salary Employment", grouping="Monthly")
  
  file.remove(data_file)
  
  # Create Year to Date
  ytd <- processed |>
    dplyr::group_by(.data$variable, .data$geography, .data$year) |>
    dplyr::summarise(estimate = sum(.data$estimate)) |>
    dplyr::as_tibble() |>
    dplyr::mutate(metric="Wage and Salary Employment", grouping="YTD", month=as.character(mo), date=lubridate::ymd(paste0(.data$year, "-", mo, "-01")))
  
  # Create Annual Summary
  ifelse(mo <12, y<-yr-1, y<-yr)
  ann <- processed |>
    dplyr::filter(.data$year <= y) |>
    dplyr::group_by(.data$variable, .data$geography, .data$year) |>
    dplyr::summarise(estimate = sum(.data$estimate)) |>
    dplyr::as_tibble() |>
    dplyr::mutate(metric="Wage and Salary Employment", grouping="Annual", month=as.character(mo), date=lubridate::ymd(paste0(.data$year, "-12-01")))
  
  processed <- dplyr::bind_rows(processed, ytd, ann) |>
    dplyr::mutate(geography_type = dplyr::case_when(
      .data$geography == "Washington State" ~ "State",
      .data$geography %in% c("Seattle MSA", "Tacoma MSA", "Bremerton MSA") ~ "MSA",
      .data$geography == "Region" ~ "Region")) |>
    dplyr::select(-"month") |>
    dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate")
  
  return(processed)
}

#' Total Employment Growth Near High Capacity Transit
#'
#' This function pulls total employment estimates from PSRC's employment database in Elmer.
#' The jobs estimates are already summarized by year in a table in Elmer.
#' VISION 2050 Station Area and RGC buffers are stored in ElmerGeo.
#' 
#' @param start_year The first year of data in the series. Defaults to 2010.
#' @return tibble of total jobs near High Capacity Transit and in the region by calendar year
#' 
#' @importFrom rlang .data
#' 
#' @examples 
#' \dontrun{
#' jobs_hct <- jobs_near_hct()}
#'  
#' @export
#'
jobs_near_hct <- function(start_year = 2010) {
  
  region <- psrcelmer::get_table(schema='employment', tbl_name='hct_station_areas_employment') |>
    dplyr::filter(.data$data_year >= start_year) |>
    dplyr::filter(.data$geo %in% c("Inside HCT Area", "Region")) |>
    tidyr::pivot_wider(names_from = "geo", values_from = "total_emp") |>
    dplyr::rename(`in station area` = "Inside HCT Area") |>
    dplyr::mutate(`not in station area` = .data$Region - .data$`in station area`) |>
    tidyr::pivot_longer(cols = !c("data_year", "Region"), names_to = "variable", values_to = "jobs") |>
    dplyr::select(-"Region") |>
    dplyr::arrange(.data$variable, .data$data_year) |>
    dplyr::group_by(.data$variable) |>
    dplyr::mutate(job_growth = .data$jobs - dplyr::lag(.data$jobs)) |>
    tidyr::pivot_longer(cols = !c("data_year", "variable"), names_to = "metric", values_to = "estimate") |>
    tidyr::drop_na() |>
    dplyr::mutate(grouping = dplyr::case_when(
      stringr::str_detect(.data$metric, "growth") ~ "Change",
      !(stringr::str_detect(.data$metric, "growth")) ~ "Total"))|>
    dplyr::mutate(metric = "Jobs near HCT") |>
    dplyr::rename(year = "data_year")
  
  totals <- region |>
    dplyr::group_by(.data$year, .data$metric, .data$grouping) |>
    dplyr::summarise(total = sum(.data$estimate)) |>
    dplyr::as_tibble()
  
  region <- dplyr::left_join(region, totals, by=c("year", "metric", "grouping")) |>
    dplyr::mutate(share = .data$estimate / .data$total) |>
    dplyr::select(-"total") |>
    dplyr::mutate(year = as.character(.data$year), geography="Region", geography_type="Region", date=lubridate::mdy(paste0("04-01-", .data$year))) |>
    dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate", "share")
  
  return(region)
  
}

#' Total Observed and Forecast Employment Growth
#'
#' This function pulls total employment estimates from PSRC's employment database in Elmer and the Macroforecast.
#' The jobs estimates are already summarized by year in a table in Elmer.
#' 
#' @param forecast_base_yr Four digit integer for Base Year in Forecast Data - defaults to 2018 
#' @param first_year Four digit integer for first year of data to use from Elmer - defaults to 2010
#' @return tibble of total jobs in the region by calendar year
#' 
#' @importFrom rlang .data
#' 
#' @examples 
#' \dontrun{
#' jobs <- processed_covered_employment()}
#'  
#' @export
#'
processed_covered_employment <- function(forecast_base_yr=2018, first_year = 2010){
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  print(stringr::str_glue("Getting Historic Jobs Data"))
  o <- jobs_near_hct(start_year = first_year) |>
    dplyr::group_by(.data$year, .data$date, .data$geography, .data$geography_type, .data$grouping, .data$metric) |>
    dplyr::summarise(estimate = sum(.data$estimate)) |>
    dplyr::as_tibble() |>
    dplyr::mutate(variable="Observed", metric="Covered Employment") |>
    dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate")
  
  # Get Forecast Job Growth from Elmer
  print(stringr::str_glue("Getting Jobs Forecast Data"))
  fj <- psrcelmer::get_table(schema='Macroeconomic', tbl_name='employment_facts') |>
    dplyr::filter(.data$employment_sector_dim_id==8 & .data$data_year >= forecast_base_yr-1) |>
    dplyr::rename(estimate="jobs", year="data_year") |>
    dplyr::mutate(date = lubridate::ymd(paste0(.data$year,"-04-01"))) |>
    dplyr::mutate(geography="Region", geography_type="Region", variable="Forecast", metric="Covered Employment") |>
    dplyr::select("year", "date", "geography", "geography_type", "variable", "metric", "estimate") |>
    dplyr::mutate(change = (.data$estimate- dplyr::lag(.data$estimate))) |>
    tidyr::drop_na() |>
    tidyr::pivot_longer(cols = c("estimate", "change"), names_to = "grouping", values_to = "estimate") |>
    dplyr::mutate(grouping = stringr::str_replace_all(.data$grouping, "change", "Change")) |>
    dplyr::mutate(grouping = stringr::str_replace_all(.data$grouping, "estimate", "Total")) |>
    dplyr::mutate(year = as.character(.data$year)) |>
    dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate")
  
  f <- dplyr::bind_rows(o, fj)
  
  print(stringr::str_glue("All Done"))
  return(f)
}

#' Summarize Jobs Data
#'
#' This function summarizes Covered and Wage & Slary Jobs Data
#' 
#' @param yr Current four digit calendar year as integer
#' @param mo Current month as integer, no leading zeros
#' @return tibble in long form of Jobs data
#' 
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' jobs_data <- summarize_jobs_data(yr=2023, mo=12)}
#' 
#' @export
#'
summarize_jobs_data <- function(yr, mo) {
  
  ws <- process_qcew_monthly_msa(yr, mo)
  hct <- jobs_near_hct()
  ce <- processed_covered_employment()
  processed <- dplyr::bind_rows(ws, hct, ce)
  
  return(processed)
}
