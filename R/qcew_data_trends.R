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
