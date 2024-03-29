% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/electric_vehicles.R
\name{vehicle_registrations_tract}
\alias{vehicle_registrations_tract}
\title{New Vehicle Registrations by Electrification Level and Census Tract}
\usage{
vehicle_registrations_tract(
  data_file = "X:/DSA/rtp-dashboard/DOL/Vehicle_Title_Transactions.csv",
  title_type = "Original Title",
  vehicle_type = "New"
)
}
\arguments{
\item{data_file}{path to downloaded data files https://data.wa.gov/Transportation/Title-Transactions-by-Month/u4cd-bc3x}

\item{title_type}{Either an "Original Title" or a "Transfer Title" - defaults to "Original Title"}

\item{vehicle_type}{Either for "New" or "Used" vehicles  -defaults to "New"}
}
\value{
tibble of new vehicle registrations by electrification level by year to date by 2020 census tract
}
\description{
This cleans data from the Washington State Registration Data vehicle registrations on open data portal.
The data is pre-downloaded due to size from: https://data.wa.gov/Transportation/Title-Transactions-by-Month/u4cd-bc3x
}
\examples{
\dontrun{
new_veh_tract <- vehicle_registrations_tract(title_type=c("Original Title"), 
                                             vehicle_type=c("New"))}

}
