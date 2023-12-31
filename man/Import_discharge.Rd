% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Import_discharge.R
\name{Import_discharge}
\alias{Import_discharge}
\title{Import discharge data}
\usage{
Import_discharge(user_data_folder, site = NULL)
}
\arguments{
\item{user_data_folder}{file path to user data folder}

\item{site}{site folder in user_data_folder}
}
\value{
Discharge.rds in user_data_folder/site/Discharge_data
}
\description{
Imports discharge data from csv file from Stephen Wallace. Change as needed if csv format changes but ensure exported file retains specified format
}
\note{
All functions expect data to be in following folders:

\describe{
\item{1. user_data_folder/site/ADCP_data}{- Place 000 files here}
}
\describe{
\item{2. user_data_folder/site/Analyte_data}{- Place TSS data from EAGLE IO csv export here}
}
\describe{
\item{3. user_data_folder/site/Channel_Geometry}{- Place cross-section survety in xs.RDS file format from Stephen Wallace here}
}
\describe{
\item{4. user_data_folder/site/Discharge_data }{- Place discharge and velocity csv files from Stephen Wallace here}
}
\describe{
\item{5. user_data_folder/site/Height_Offsets}{- Place height offset data from EAGLE IO csv export here}
}
\describe{
\item{6. user_data_folder/site/Sonde_and_Height_data}{- Place TSS data from EAGLE IO csv export here}
}

Where "site" is a specific site (e.g., for Johnstone at Innisfail use site <- "JRI")

See vignette('LinkrealTimeloads') for further instructions
}
\section{Warning}{

Users can change how data is imported but should ensure outputs follow structure in rds file generated by function
}

\examples{
# See vignette('LinkrealTimeloads',package = LinkrealTimeloads)
}
\references{
Stephen Wallace (2023, DES) provided crucial functions in Import_Channelmaster_Data() to extract data from binary 000 files.

Livsey D.N. (2023). realTimeloads: Analyte Flux and Load from Estimates of Concentration and Discharge_. R package version 1.0.0.

Livsey, D.N. (in review). National Industry Guidelines for hydrometric monitoring–Part 12: Application of acoustic Doppler velocity meters to measure suspended-sediment load. Bureau of Meteorology. Melbourne, Australia
}
\seealso{
\code{\link{Link_to_Real_time_loads}} Process data in specified folder structure using realTimeloads package
\code{\link{Predict_TSS_from_SCI}} Estimates TSS from processed acoustic backscatter and turbidit
}
\author{
Daniel Livsey (2023) ORCID: 0000-0002-2028-6128
}
