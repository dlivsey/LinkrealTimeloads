% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QAQC_Report.R
\name{QAQC_Report}
\alias{QAQC_Report}
\title{Writes QAQC Report using QAQC_Report.Rmd in LinkrealTimeloads/extdata to user_data_folder/site}
\usage{
QAQC_Report(
  author = NULL,
  user_data_folder = NULL,
  output_file = NULL,
  site = NULL,
  site_name = NULL,
  compute_from_time = NULL,
  compute_to_time = NULL,
  max_points = 30
)
}
\arguments{
\item{author}{Your name (string)}

\item{user_data_folder}{file path to user data folder (string)}

\item{output_file}{custom output file name to override default name (string)}

\item{site}{site folder under user_data_folder (string)}

\item{site_name}{name of site for rendering purposes (string)}

\item{compute_from_time}{time (PosixCt or Date)}

\item{compute_to_time}{time (PosixCt or Date)}

\item{max_points}{maximum number of points per period in Kt(SCI) (integer)}
}
\value{
file path to html. Also writes html report to user_data_folder/site if provided by user
}
\description{
Report is written in html format to user_data_folder/site folder. Report provides interactive plots to identify anomalies in timeseries and regression data.
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
