#' Process QCEW Monthly MSA Data
#'
#' This function processes MSA monthly data from QCEW.
#' Data is pulled monthly from "https://media.esd.wa.gov/esdwa/Default/ESDWAGOV/labor-market-info/Libraries/Economic-reports/Washington-employment-estimates/".
#' 
#' @param c.yr Current four digit calendar year as integer
#' @param c.mo Current month as integer, no leading zeros
#' @return tibble in long form of QCEW Monthly Employment data by MSA+
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' qcew.jobs <- process_qcew_monthly_msa(c.yr=2022, c.mo=5)
#' 
#' @export
#'
process_qcew_monthly_msa <- function(c.yr, c.mo) {
  
  qcew.areas <- c("Washington State", "Seattle MSA", "Tacoma MSA", "Bremerton MSA")
  goods.producing <- c("Mining and Logging", "Construction", "Manufacturing")
  private.services <- c("Trade, Transportation, and Utilities", "Information", "Financial Activities", "Professional and Business Services","Educational and Health Services", "Leisure and Hospitality", "Other Services")
  government.services <- c("Government")
  job.categories <- c(goods.producing, private.services,government.services)
  data.url <- paste0("https://media.esd.wa.gov/esdwa/Default/ESDWAGOV/labor-market-info/Libraries/Economic-reports/Washington-employment-estimates/",c.yr,"%20WAQB/WA-QB-historical-SA-all%20areas",c.mo,"-",c.yr-2000,".xlsx")
  
  utils::download.file(data.url, "working.xlsx", quiet = TRUE, mode = "wb")
  data.file <- paste0(getwd(),"/working.xlsx")
  
  processed <- NULL
  for (areas in qcew.areas) {
    
    t <- dplyr::as_tibble(openxlsx::read.xlsx(data.file, sheet = areas, detectDates = TRUE, skipEmptyRows = TRUE, startRow = 2, colNames = TRUE)) %>%
      tidyr::pivot_longer(cols=dplyr::contains("-"), names_to = "data_day", values_to ="estimate") %>%
      dplyr::mutate(data_day = lubridate::ymd(.data$data_day)) %>%
      dplyr::mutate(geography=areas) %>%
      dplyr::select(-.data$NAICS.CELL) %>%
      dplyr::mutate(NAICS.INDUSTRY = trimws(.data$NAICS.INDUSTRY, "both")) %>%
      dplyr::rename(variable=.data$NAICS.INDUSTRY)
    
    
    if (areas == "Bremerton MSA") {
      
      private.total <- t %>% dplyr::filter(.data$variable=="Private Service Providing") 
      
      detailed.private <- t %>% 
        dplyr::filter(.data$variable %in% private.services) %>%
        dplyr::group_by(.data$data_day) %>%
        dplyr::summarize(detailed_jobs=sum(.data$estimate))
      
      private.total <- dplyr::left_join(private.total,detailed.private,by=c("data_day")) %>%
        dplyr::mutate(estimate = .data$estimate - .data$detailed_jobs) %>%
        dplyr::mutate(variable = "Other Services") %>%
        dplyr::select(-.data$detailed_jobs)
      
      t <- dplyr::bind_rows(t, private.total)
      
      t <- t %>%
        dplyr::mutate(variable = gsub("Mining, Logging, and Construction", "Construction", .data$variable))
      
    }
    
    ifelse(is.null(processed), processed <- t, processed <- dplyr::bind_rows(processed,t))
  }
  
  r <- processed %>%
    dplyr::filter(!(.data$geography %in% c("Washington State"))) %>%
    dplyr::group_by(.data$variable,.data$data_day) %>%
    dplyr::summarize(estimate=sum(.data$estimate)) %>%
    dplyr::mutate(geography="Region")
  
  processed <- dplyr::bind_rows(processed,r) %>% 
    dplyr::mutate(year = lubridate::year(.data$data_day), month = paste0(lubridate::month(.data$data_day))) %>% 
    dplyr::mutate(year=as.character(.data$year)) %>%
    dplyr::mutate(month=as.integer(.data$month)) %>%
    dplyr::mutate(month=formatC(.data$month, width=2, flag="0")) %>%
    dplyr::mutate(month=as.character(.data$month)) %>%
    dplyr::mutate(equiv_day=paste0(c.yr,"-",.data$month,"-01")) %>%
    dplyr::mutate(equiv_day=lubridate::ymd(.data$equiv_day)) %>%
    dplyr::mutate(concept="Wage and Salary Employment")
  
  file.remove(data.file)
  
  return(processed)
}
