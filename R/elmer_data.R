#' Get a Table from Elmer
#'
#' This function pulls a Table from the PSRC Central Database.
#' 
#' @param tbl.nm Name of schema and table to pull from the database such as "equity.indicator_facts" 
#' @param srv.nm Server name, defaults to the current Sockeye instance
#' @param db.nm Name of database, defaults to "Elmer"
#' @return tibble of datatable from the central Database
#' 
#' @importFrom magrittr %<>% %>%
#' 
#' @examples
#' 
#' equity.data <- get_elmer_table("equity.indicator_facts")
#' jurisdictions <- get_elmer_table("Political.jurisdiction_dims")
#' 
#' @export
#'
get_elmer_table <- function(tbl.nm, srv.nm='AWS-PROD-SQL\\SOCKEYE', db.nm='Elmer') {
  
  db.con <- DBI::dbConnect(odbc::odbc(),
                           driver = "SQL Server",
                           server = srv.nm,
                           database = db.nm,
                           trusted_connection = "yes")
  
  df <- dplyr::as_tibble(DBI::dbReadTable(db.con, DBI::SQL(tbl.nm)))
  DBI::dbDisconnect(db.con)
  
  return(df)
  
}
