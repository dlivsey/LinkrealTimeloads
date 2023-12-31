% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initialize_folders_and_move_data_files.R
\name{initialize_folders_and_move_data_files}
\alias{initialize_folders_and_move_data_files}
\title{Initialize folder and copies data in LinkrealTimeloads/extdata to LinkrealTimeloads/extdata/Example}
\usage{
initialize_folders_and_move_data_files(
  user_data_folder,
  site,
  Site_Number = NULL,
  Site_Name = NULL
)
}
\arguments{
\item{user_data_folder}{file path to user_data_folder}

\item{site}{site folder under user_data_folder}

\item{Site_Number}{site number for site}

\item{Site_Name}{site name for plotting purposes}
}
\value{
Writes folders listed in note under user_data_folder, updates Site_List.rds, and copies files from LinkrealTimeloads/extdata to user_data_folder/site
}
\description{
Initialize folder and copies data on example data provided in package. Allows user to follow package workflow in setting up a new site directory.
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
\examples{
# See LinkrealTimeloads::Example()
}
\references{
Stephen Wallace (2023, DES) provided crucial functions in Import_Channelmaster_Data() to extract data from binary 000 files.

Livsey D.N. (2023). realTimeloads: Analyte Flux and Load from Estimates of Concentration and Discharge_. R package version 1.0.0.

Livsey, D.N. (in review). National Industry Guidelines for hydrometric monitoring–Part 12: Application of acoustic Doppler velocity meters to measure suspended-sediment load. Bureau of Meteorology. Melbourne, Australia
}
\seealso{
\code{\link{Link_to_Real_time_loads}} Process data in specified folder structure using realTimeloads package
}
\author{
Daniel Livsey (2023) ORCID: 0000-0002-2028-6128
}
