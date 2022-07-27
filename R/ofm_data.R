#' Get OFM Intercensal Population Data by Jurisdiction
#'
#' This function pulls Intercensal Population Data by Jurisdiction.
#' 
#' @return tibble of population by jurisdiction
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' ofm.data <- get_ofm_intercensal_population()
#' 
#' @export
#'
get_ofm_intercensal_population <-function () {
  
  # Jurisdictions with Regional Geography
  jurisdictions <- psrctrends::get_elmer_table("Political.jurisdiction_dims")
  
  jurisdictions <- jurisdictions %>%
    dplyr::mutate(juris_name = gsub("Seatac","SeaTac",.data$juris_name)) %>%
    dplyr::mutate(juris_name = gsub("Beau Arts Village","Beaux Arts Village",.data$juris_name)) %>%
    dplyr::select(.data$juris_name, .data$regional_geography, .data$airport_affected) %>%
    dplyr::distinct() %>%
    dplyr::mutate(regional_geography=gsub("HCT","High Capacity Transit Community",.data$regional_geography)) %>%
    dplyr::mutate(regional_geography=gsub("Metro","Metropolitan Cities",.data$regional_geography)) %>%
    dplyr::mutate(regional_geography=gsub("Core","Core Cities",.data$regional_geography)) %>%
    dplyr::mutate(regional_geography=gsub("CitiesTowns","Cities & Towns",.data$regional_geography)) %>%
    dplyr::select(-.data$airport_affected)%>%
    dplyr::mutate(juris_name = gsub("Uninc. King", "King County", .data$juris_name)) %>%
    dplyr::mutate(juris_name = gsub("Uninc. Kitsap", "Kitsap County", .data$juris_name)) %>%
    dplyr::mutate(juris_name = gsub("Uninc. Pierce", "Pierce County", .data$juris_name)) %>%
    dplyr::mutate(juris_name = gsub("Uninc. Snohomish", "Snohomish County", .data$juris_name))
  
  # Population
  
  # Intercensal estimates since 2020 data is now available
  ofm.url.1990 <- "https://ofm.wa.gov/sites/default/files/public/dataresearch/pop/april1/hseries/ofm_april1_intercensal_estimates_1990-2000.xlsx"
  ofm.url.2000 <- "https://ofm.wa.gov/sites/default/files/public/dataresearch/pop/april1/hseries/ofm_april1_intercensal_estimates_2000-2010.xlsx"
  ofm.url.2010 <- "https://ofm.wa.gov/sites/default/files/public/dataresearch/pop/april1/hseries/ofm_april1_intercensal_estimates_2010_2020.xlsx"
  ofm.url.latest <- "https://ofm.wa.gov/sites/default/files/public/dataresearch/pop/april1/ofm_april1_population_final.xlsx"
  
  # 1990 to 2000
  utils::download.file(ofm.url.1990, "working.xlsx", quiet = TRUE, mode = "wb")
  ofm.pop.file <- paste0(getwd(),"/working.xlsx")
  
  ofm.pop.90 <- dplyr::as_tibble(openxlsx::read.xlsx(ofm.pop.file, detectDates = FALSE, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE, sheet = "Total Population ")) %>%
    dplyr::filter(.data$County.Name %in% c("King","Kitsap","Pierce","Snohomish")) %>%
    tidyr::pivot_longer(cols=dplyr::contains("Population"), names_to="Year", values_to="Estimate") %>%
    dplyr::select(-(dplyr::contains("Place.Code")), -.data$Jurisdiction) %>%
    dplyr::mutate(Year = stringr::str_replace(.data$Year, ".Total.Population.*", ""), City.Name = stringr::str_replace(.data$City.Name, " \\(part\\)", "")) %>%
    dplyr::mutate(dplyr::across(c('Filter','Year','Estimate'), as.numeric)) %>%
    dplyr::rename(County=.data$County.Name, Jurisdiction=.data$City.Name) %>%
    dplyr::filter(.data$Year != 2000) %>%
    dplyr::mutate(Jurisdiction = dplyr::case_when(
      .data$Filter == 1 ~ paste0(.data$County," County"),
      .data$Filter == 2 ~ paste0(.data$Jurisdiction," ", .data$County, " County"),
      .data$Filter == 3 ~ paste0(.data$Jurisdiction," ", .data$County," County"),
      .data$Filter == 4 ~ .data$Jurisdiction)) %>%
    dplyr::mutate(Jurisdiction = stringr::str_replace(.data$Jurisdiction, "Beaux Arts", "Beaux Arts Village")) %>%
    dplyr::mutate(Jurisdiction = stringr::str_replace(.data$Jurisdiction, "Du Pont", "DuPont")) %>%
    dplyr::mutate(Jurisdiction = stringr::str_trim(.data$Jurisdiction, side = c("both")))
  
  # 2000 to 2010
  utils::download.file(ofm.url.2000, "working.xlsx", quiet = TRUE, mode = "wb")
  ofm.pop.file <- paste0(getwd(),"/working.xlsx")
  
  ofm.pop.00 <- dplyr::as_tibble(openxlsx::read.xlsx(ofm.pop.file, detectDates = FALSE, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE, sheet = "Total Population")) %>%
    dplyr::filter(.data$County.Name %in% c("King","Kitsap","Pierce","Snohomish")) %>%
    tidyr::pivot_longer(cols=dplyr::contains("Population"), names_to="Year", values_to="Estimate") %>%
    dplyr::select(-(dplyr::contains("Place.Code")), -.data$Jurisdiction) %>%
    dplyr::mutate(Year = stringr::str_replace(.data$Year, ".Total.Population.*", ""), Year = stringr::str_replace(.data$Year, ".Intercensal.*", ""), City.Name = stringr::str_replace(.data$City.Name, " \\(part\\)", "")) %>%
    dplyr::mutate(dplyr::across(c('Filter','Year','Estimate'), as.numeric)) %>%
    dplyr::rename(County=.data$County.Name, Jurisdiction=.data$City.Name)%>%
    dplyr::filter(.data$Year != 2010) %>%
    dplyr::select(-.data$Line) %>%
    dplyr::mutate(Jurisdiction = dplyr::case_when(
      Filter == 1 ~ .data$Jurisdiction,
      Filter == 2 ~ paste0(.data$Jurisdiction," ", .data$County, " County"),
      Filter == 3 ~ paste0(.data$Jurisdiction," ", .data$County, " County"),
      Filter == 4 ~ .data$Jurisdiction)) %>%
    dplyr::mutate(Jurisdiction = stringr::str_trim(.data$Jurisdiction, side = c("both")))
  
  # 2010 to 2020
  utils::download.file(ofm.url.2010, "working.xlsx", quiet = TRUE, mode = "wb")
  ofm.pop.file <- paste0(getwd(),"/working.xlsx")
  
  ofm.pop.10 <- dplyr::as_tibble(openxlsx::read.xlsx(ofm.pop.file, detectDates = FALSE, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE, sheet = "Total Population")) %>%
    dplyr::filter(.data$County.Name %in% c("King","Kitsap","Pierce","Snohomish")) %>%
    tidyr::pivot_longer(cols=dplyr::contains("Population"), names_to="Year", values_to="Estimate") %>%
    dplyr::select(-(dplyr::contains("FIPS.Code")), -.data$Jurisdiction) %>%
    dplyr::mutate(Year = stringr::str_replace(.data$Year, ".Total.Population.*", ""), Year = stringr::str_replace(.data$Year, ".Intercensal.*", ""), City.Name = stringr::str_replace(.data$City.Name, " \\(part\\)", "")) %>%
    dplyr::mutate(dplyr::across(c('Filter','Year','Estimate'), as.numeric)) %>%
    dplyr::rename(County=.data$County.Name, Jurisdiction=.data$City.Name) %>%
    dplyr::select(-.data$Line) %>%
    dplyr::filter(.data$Year != 2020) %>%
    dplyr::mutate(Jurisdiction = dplyr::case_when(
      Filter == 1 ~ .data$Jurisdiction,
      Filter == 2 ~ paste0(.data$Jurisdiction," ", .data$County, " County"),
      Filter == 3 ~ paste0(.data$Jurisdiction," ", .data$County, " County"),
      Filter == 4 ~ .data$Jurisdiction)) %>%
    dplyr::mutate(Jurisdiction = stringr::str_trim(.data$Jurisdiction, side = c("both")))
  
  # 2020 to present
  utils::download.file(ofm.url.latest, "working.xlsx", quiet = TRUE, mode = "wb")
  ofm.pop.file <- paste0(getwd(),"/working.xlsx")
  
  ofm.pop.latest <- dplyr::as_tibble(openxlsx::read.xlsx(ofm.pop.file, detectDates = FALSE, skipEmptyRows = TRUE, startRow = 5, colNames = TRUE, sheet = "Population")) %>%
    dplyr::filter(.data$County %in% c("King","Kitsap","Pierce","Snohomish")) %>%
    tidyr::pivot_longer(cols=dplyr::contains("Population"), names_to="Year", values_to="Estimate") %>%
    dplyr::mutate(Year = stringr::str_replace(.data$Year, ".Population.*", ""), Jurisdiction = stringr::str_replace(.data$Jurisdiction, " \\(part\\)", "")) %>%
    dplyr::mutate(dplyr::across(c('Filter','Year','Estimate'), as.numeric)) %>%
    dplyr::select(-.data$Line) %>%
    dplyr::mutate(Jurisdiction = stringr::str_trim(.data$Jurisdiction, side = c("both")))
  
  # Combine into one tibble and aggregate places that span multiple counties into one
  ofm.pop <- dplyr::bind_rows(list(ofm.pop.90,ofm.pop.00,ofm.pop.10,ofm.pop.latest)) %>%
    dplyr::select(-.data$County) %>%
    dplyr::group_by(.data$Filter, .data$Jurisdiction, .data$Year) %>%
    dplyr::summarize(Estimate = sum(.data$Estimate)) %>%
    dplyr::as_tibble()
  
  # Create a Regional Summary by Filter Type and then Join to Full OFM tibble
  region.pop <- ofm.pop %>%
    dplyr::filter(.data$Filter <= 3) %>%
    dplyr::select(.data$Filter,.data$Year, .data$Estimate) %>%
    dplyr::group_by(.data$Filter,.data$Year) %>%
    dplyr::summarize_all(sum) %>%
    dplyr::mutate(Jurisdiction = "Region") %>%
    dplyr::mutate(Jurisdiction = ifelse(.data$Filter == 2, "Unincorporated Region", .data$Jurisdiction)) %>%
    dplyr::mutate(Jurisdiction = ifelse(.data$Filter == 3, "Incorporated Region", .data$Jurisdiction))
  
  # Add the regional results to the OFM full tibble
  ofm.pop <- dplyr::bind_rows(ofm.pop,region.pop) %>% dplyr::mutate(Variable="Total Population")
  
  # Add Regional Geography to Data
  ofm.pop <- dplyr::left_join(ofm.pop, jurisdictions, by=c("Jurisdiction"="juris_name")) %>%
    dplyr::mutate(regional_geography = dplyr::case_when(
      Filter==1 ~ "County",
      Filter==2 ~ "Unincorporated",
      Filter==3 ~ "Incorporated",
      Filter==4 ~ .data$regional_geography
    ))
  
  file.remove(ofm.pop.file)
  
  return(ofm.pop)
  
}


#' Get OFM Postcensal Housing Data by Jurisdiction
#'
#' This function pulls Postcensal Housing Data by Jurisdiction.
#' 
#' @return tibble of housing units by jurisdiction
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' ofm.data <- get_ofm_postcensal_housing()
#' 
#' @export
#'
get_ofm_postcensal_housing <-function () {
  
  # Jurisdictions with Regional Geography
  jurisdictions <- psrctrends::get_elmer_table("Political.jurisdiction_dims")
  
  jurisdictions <- jurisdictions %>%
    dplyr::mutate(juris_name = gsub("Seatac","SeaTac",.data$juris_name)) %>%
    dplyr::mutate(juris_name = gsub("Beau Arts Village","Beaux Arts Village",.data$juris_name)) %>%
    dplyr::select(.data$juris_name, .data$regional_geography, .data$airport_affected) %>%
    dplyr::distinct() %>%
    dplyr::mutate(regional_geography=gsub("HCT","High Capacity Transit Community",.data$regional_geography)) %>%
    dplyr::mutate(regional_geography=gsub("Metro","Metropolitan Cities",.data$regional_geography)) %>%
    dplyr::mutate(regional_geography=gsub("Core","Core Cities",.data$regional_geography)) %>%
    dplyr::mutate(regional_geography=gsub("CitiesTowns","Cities & Towns",.data$regional_geography)) %>%
    dplyr::select(-.data$airport_affected)%>%
    dplyr::mutate(juris_name = gsub("Uninc. King", "King County", .data$juris_name)) %>%
    dplyr::mutate(juris_name = gsub("Uninc. Kitsap", "Kitsap County", .data$juris_name)) %>%
    dplyr::mutate(juris_name = gsub("Uninc. Pierce", "Pierce County", .data$juris_name)) %>%
    dplyr::mutate(juris_name = gsub("Uninc. Snohomish", "Snohomish County", .data$juris_name))
  
  # Housing
  
  ofm.url <- "https://ofm.wa.gov/sites/default/files/public/dataresearch/pop/april1/hseries/ofm_april1_postcensal_estimates_housing_1980_1990-present.xlsx"
  
  # 1990 to Present
  utils::download.file(ofm.url, "working.xlsx", quiet = TRUE, mode = "wb")
  ofm.pop.file <- paste0(getwd(),"/working.xlsx")
  
  ofm.type <- dplyr::as_tibble(openxlsx::read.xlsx(ofm.pop.file, detectDates = FALSE, skipEmptyRows = TRUE, startRow = 4, colNames = TRUE, sheet = "Housing Units")) %>%
    dplyr::filter(.data$County %in% c("King","Kitsap","Pierce","Snohomish")) %>%
    dplyr::select(-(dplyr::contains("1980"))) %>%
    tidyr::pivot_longer(cols=dplyr::contains("Housing"), names_to="Year", values_to="Estimate") %>%
    dplyr::mutate(Variable= dplyr::case_when(
      stringr::str_detect(.data$Year, "Total") ~ "Total Housing Units",
      stringr::str_detect(.data$Year, "One") ~ "Single-Family",
      stringr::str_detect(.data$Year, "Two") ~ "Multi-Family",
      stringr::str_detect(.data$Year, "Mobile") ~ "Mobile Home")) %>%
    dplyr::mutate(Year = stringr::str_replace(.data$Year, ".Census.*", ""), Year = stringr::str_replace(.data$Year, ".Postcensal.*", ""), Jurisdiction = stringr::str_replace(.data$Jurisdiction, " \\(part\\)", "")) %>%
    dplyr::mutate(dplyr::across(c('Filter','Year','Estimate'), as.numeric)) %>%
    dplyr::select(-.data$Line) %>%
    dplyr::mutate(Jurisdiction = stringr::str_trim(.data$Jurisdiction, side = c("both"))) %>%
    dplyr::select(-.data$County) %>%
    dplyr::group_by(.data$Filter, .data$Jurisdiction, .data$Year, .data$Variable) %>%
    dplyr::summarize(Estimate = sum(.data$Estimate)) %>%
    dplyr::as_tibble()
  
  # Create a Regional Summary by Filter Type and then Join to Full OFM tibble
  region <- ofm.type %>%
    dplyr::filter(.data$Filter <= 3) %>%
    dplyr::select(.data$Filter,.data$Year, .data$Estimate, .data$Variable) %>%
    dplyr::group_by(.data$Filter,.data$Year, .data$Variable) %>%
    dplyr::summarize_all(sum) %>%
    dplyr::mutate(Jurisdiction = "Region") %>%
    dplyr::mutate(Jurisdiction = ifelse(.data$Filter == 2, "Unincorporated Region", .data$Jurisdiction)) %>%
    dplyr::mutate(Jurisdiction = ifelse(.data$Filter == 3, "Incorporated Region", .data$Jurisdiction))
  
  # Add the regional results to the OFM full tibble
  ofm.type <- dplyr::bind_rows(ofm.type,region)
  
  ofm.type <- dplyr::left_join(ofm.type, jurisdictions, by=c("Jurisdiction"="juris_name")) %>%
    dplyr::mutate(regional_geography = dplyr::case_when(
      Filter==1 ~ "County",
      Filter==2 ~ "Unincorporated",
      Filter==3 ~ "Incorporated",
      Filter==4 ~ .data$regional_geography
    ))
  
  file.remove(ofm.pop.file)
  
  return(ofm.type)
}


#' Get OFM Postcensal Population Change by Type by County
#'
#' This function pulls Postcensal Population Change Data by County.
#' 
#' @return tibble of population change by County
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' ofm.pop.chg.data <- get_ofm_postcensal_pop_change()
#' 
#' @export
#'
get_ofm_postcensal_pop_change <-function () {
  
  # Jurisdictions with Regional Geography
  jurisdictions <- psrctrends::get_elmer_table("Political.jurisdiction_dims")
  
  jurisdictions <- jurisdictions %>%
    dplyr::mutate(juris_name = gsub("Seatac","SeaTac",.data$juris_name)) %>%
    dplyr::mutate(juris_name = gsub("Beau Arts Village","Beaux Arts Village",.data$juris_name)) %>%
    dplyr::select(.data$juris_name, .data$regional_geography, .data$airport_affected) %>%
    dplyr::distinct() %>%
    dplyr::mutate(regional_geography=gsub("HCT","High Capacity Transit Community",.data$regional_geography)) %>%
    dplyr::mutate(regional_geography=gsub("Metro","Metropolitan Cities",.data$regional_geography)) %>%
    dplyr::mutate(regional_geography=gsub("Core","Core Cities",.data$regional_geography)) %>%
    dplyr::mutate(regional_geography=gsub("CitiesTowns","Cities & Towns",.data$regional_geography)) %>%
    dplyr::select(-.data$airport_affected)%>%
    dplyr::mutate(juris_name = gsub("Uninc. King", "King County", .data$juris_name)) %>%
    dplyr::mutate(juris_name = gsub("Uninc. Kitsap", "Kitsap County", .data$juris_name)) %>%
    dplyr::mutate(juris_name = gsub("Uninc. Pierce", "Pierce County", .data$juris_name)) %>%
    dplyr::mutate(juris_name = gsub("Uninc. Snohomish", "Snohomish County", .data$juris_name))
  
  # Components of Change
  ofm.url <- "https://ofm.wa.gov/sites/default/files/public/dataresearch/pop/components/ofm_april1_components_of_change_1960_to_present.xlsx"
  
  # 1960 to Present
  utils::download.file(ofm.url, "working.xlsx", quiet = TRUE, mode = "wb")
  ofm.pop.file <- paste0(getwd(),"/working.xlsx")
  
  ofm.natural <- dplyr::as_tibble(openxlsx::read.xlsx(ofm.pop.file, detectDates = FALSE, skipEmptyRows = TRUE, startRow = 4, colNames = TRUE, sheet = "Natural Increase")) %>%
    dplyr::filter(.data$County %in% c("King","Kitsap","Pierce","Snohomish")) %>%
    dplyr::select(-.data$Spacer) %>%
    tidyr::pivot_longer(cols=dplyr::contains("Natural"), names_to="Year", values_to="Estimate") %>%
    dplyr::mutate(Year = stringr::str_replace(.data$Year, "Natural.Increase.", "")) %>%
    dplyr::mutate(Year = stringr::str_extract(.data$Year,"\\d{4}[^\\d]*$")) %>%
    dplyr::mutate(dplyr::across(c('Estimate','Year'), as.numeric)) %>%
    dplyr::mutate(Year = tidyr::replace_na(.data$Year,2021)) %>%
    dplyr::mutate(Jurisdiction = stringr::str_trim(.data$Jurisdiction, side = c("both"))) %>%
    dplyr::mutate(Jurisdiction = dplyr::case_when(
      stringr::str_detect(.data$Jurisdiction, "Pierce") ~ "Pierce County",
      !stringr::str_detect(.data$Jurisdiction, "Pierce") ~ .data$Jurisdiction)) %>%
    dplyr::select(-.data$County) %>%
    dplyr::mutate(Variable="Natural Increase")
  
  ofm.migration <- dplyr::as_tibble(openxlsx::read.xlsx(ofm.pop.file, detectDates = FALSE, skipEmptyRows = TRUE, startRow = 4, colNames = TRUE, sheet = "Residual Net Migration")) %>%
    dplyr::filter(.data$County %in% c("King","Kitsap","Pierce","Snohomish")) %>%
    dplyr::select(-.data$Spacer) %>%
    tidyr::pivot_longer(cols=dplyr::contains("Residual"), names_to="Year", values_to="Estimate") %>%
    dplyr::mutate(Year = stringr::str_replace(.data$Year, "Residual.Net.Migration.", "")) %>%
    dplyr::mutate(Year = stringr::str_extract(.data$Year,"\\d{4}[^\\d]*$")) %>%
    dplyr::mutate(dplyr::across(c('Estimate','Year'), as.numeric)) %>%
    dplyr::mutate(Year = tidyr::replace_na(.data$Year,2021)) %>%
    dplyr::mutate(Jurisdiction = stringr::str_trim(.data$Jurisdiction, side = c("both"))) %>%
    dplyr::mutate(Jurisdiction = dplyr::case_when(
      stringr::str_detect(.data$Jurisdiction, "Pierce") ~ "Pierce County",
      !stringr::str_detect(.data$Jurisdiction, "Pierce") ~ .data$Jurisdiction)) %>%
    dplyr::select(-.data$County) %>%
    dplyr::mutate(Variable="Migration")
  
  ofm.pop.chg <- dplyr::bind_rows(ofm.natural, ofm.migration) %>% dplyr::mutate(Filter="1")
  
  region <- ofm.pop.chg %>%
    dplyr::select(-.data$Jurisdiction) %>%
    dplyr::group_by(.data$Filter, .data$Year, .data$Variable) %>%
    dplyr::summarize_all(sum) %>%
    dplyr::mutate(Jurisdiction = "Region") 
  
  # Add the regional results to the OFM full tibble
  ofm.pop.chg <- dplyr::bind_rows(ofm.pop.chg,region)
  
  ofm.pop.chg <- dplyr::left_join(ofm.pop.chg, jurisdictions, by=c("Jurisdiction"="juris_name")) %>%
    dplyr::mutate(regional_geography = dplyr::case_when(
      Jurisdiction=="Region" ~ "Region",
      Jurisdiction!="Region" ~ "County")) %>%
    dplyr::mutate(Year = as.character(.data$Year))
  
  file.remove(ofm.pop.file)
  
  return(ofm.pop.chg)
}

