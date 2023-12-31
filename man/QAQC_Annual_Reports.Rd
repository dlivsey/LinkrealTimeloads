% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QAQC_Annual_Reports.R
\name{QAQC_Annual_Reports}
\alias{QAQC_Annual_Reports}
\title{Writes annual QAQC Reports using QAQC_Report() in LinkrealTimeloads to user_data_folder/site}
\usage{
QAQC_Annual_Reports(
  author = NULL,
  user_data_folder = NULL,
  site = NULL,
  site_name = NULL,
  reporting_year = NULL,
  max_points = 100
)
}
\arguments{
\item{author}{Your name (string)}

\item{user_data_folder}{file path to user data folder (string)}

\item{site}{site folder under user_data_folder (string)}

\item{site_name}{name of site for rendering purposes (string)}

\item{reporting_year}{year or years (e.g., 2022 will report Jun 2022 to Jun 2023) (double)}

\item{max_points}{maximum number of points per period in Kt(SCI) (integer)}
}
\value{
file path(s) to html. Also writes html report(s) to user_data_folder/site if provided by user
}
\description{
Report(s) is(are) written in html format to user_data_folder/site folder. Report provides interactive plots to identify anomalies in timeseries and regression data. Plotting over one year of data in QAQC_Report causes excessive delay in interactive plots.
}
\note{
Set reporting_year to NULL to plot all available data in one year increments
}
\examples{
# See use in Example()
}
\references{
Stephen Wallace (2023, DES) provided crucial functions in Import_Channelmaster_Data() to extract data from binary 000 files.

Livsey D.N. (2023). realTimeloads: Analyte Flux and Load from Estimates of Concentration and Discharge_. R package version 1.0.0.

Livsey, D.N. (in review). National Industry Guidelines for hydrometric monitoring–Part 12: Application of acoustic Doppler velocity meters to measure suspended-sediment load. Bureau of Meteorology. Melbourne, Australia
}
\seealso{
\code{\link{Example}} Example of package steps and reporting
}
\author{
Daniel Livsey (2023) ORCID: 0000-0002-2028-6128
}
