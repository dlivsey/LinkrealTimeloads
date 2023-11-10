#' Initialize folder and copies data in LinkrealTimeloads/extdata to LinkrealTimeloads/extdata/Example
#'
#' Initialize folder and copies data on example data provided in package. Allows user to follow package workflow in setting up a new site directory.
#'
#' @param user_data_folder file path to user_data_folder
#' @param site site folder under user_data_folder
#' @param Site_Number site number for site
#' @param Site_Name site name for plotting purposes
#'
#' @note
#' All functions expect data to be in following folders:
#'
#' \describe{
#'   \item{1. user_data_folder/site/ADCP_data}{- Place 000 files here}
#'    }
#' \describe{
#'   \item{2. user_data_folder/site/Analyte_data}{- Place TSS data from EAGLE IO csv export here}
#'    }
#'    \describe{
#'   \item{3. user_data_folder/site/Channel_Geometry}{- Place cross-section survety in xs.RDS file format from Stephen Wallace here}
#'    }
#'    \describe{
#'   \item{4. user_data_folder/site/Discharge_data }{- Place discharge and velocity csv files from Stephen Wallace here}
#'    }
#'    \describe{
#'   \item{5. user_data_folder/site/Height_Offsets}{- Place height offset data from EAGLE IO csv export here}
#'    }
#'    \describe{
#'   \item{6. user_data_folder/site/Sonde_and_Height_data}{- Place TSS data from EAGLE IO csv export here}
#'    }
#'
#' Where "site" is a specific site (e.g., for Johnstone at Innisfail use site <- "JRI")
#'
#' See vignette('LinkrealTimeloads') for further instructions
#'
#' @return Writes folders listed in note under user_data_folder, and updates Site_List.rds and Site_Number if provided
#' @seealso
#' \code{\link{Link_to_Real_time_loads}} Process data in specified folder structure using realTimeloads package
#'
#' @author Daniel Livsey (2023) ORCID: 0000-0002-2028-6128
#' @examples
#' # See LinkrealTimeloads::Example()
#' @references
#' Stephen Wallace (2023, DES) provided crucial functions in Import_Channelmaster_Data() to extract data from binary 000 files.
#'
#' Livsey D.N. (2023). realTimeloads: Analyte Flux and Load from Estimates of Concentration and Discharge_. R package version 1.0.0.
#'
#' Livsey, D.N. (in review). National Industry Guidelines for hydrometric monitoring–Part 12: Application of acoustic Doppler velocity meters to measure suspended-sediment load. Bureau of Meteorology. Melbourne, Australia
#'
#' @export
initialize_folders_and_move_data_files <- function(user_data_folder,site,Site_Number = NULL,Site_Name = NULL) {


  # Initialize folders in /LinkrealTimeloads/extdata/Example
  #LinkrealTimeloads::
    initialize_new_site_directories(user_data_folder,site,Site_Number = Site_Number,Site_Name = Site_Name)
  # data files

  files <- system.file("extdata", package = "LinkrealTimeloads") |> list.files()

  # folder paths
  extdata_folder <- system.file("extdata", package = "LinkrealTimeloads")
  site_folder <- paste0(paste0(user_data_folder,'/'),site)
  ADCP <- paste0(paste0(user_data_folder,'/'),paste0(site,'/ADCP_data'))
  Analyte <- paste0(paste0(user_data_folder,'/'),paste0(site,'/Analyte_data'))
  Channel <- paste0(paste0(user_data_folder,'/'),paste0(site,'/Channel_Geometry'))
  Discharge <- paste0(paste0(user_data_folder,'/'),paste0(site,'/Discharge_data'))
  Offsets <-  paste0(paste0(user_data_folder,'/'),paste0(site,'/Height_Offsets'))
  Sonde_and_Height <-  paste0(paste0(user_data_folder,'/'),paste0(site,'/Sonde_and_Height_data'))

  # Move ADCP files
  zfiles <- files[grepl('.000',files)]
  for (i in 1:length(zfiles)) {
    from <- paste0(paste0(extdata_folder,'/'),zfiles[i])
    to <- paste0(paste0(ADCP,'/'),zfiles[i])
    if (file.exists(from) & !file.exists(to)) {
      file.copy(from,to,overwrite = FALSE)
      #file.remove(from) # remove file?
    }
  }

  # Move Analyte data file
  zfiles <- files[grepl('LabData',files)]
  for (i in 1:length(zfiles)) {
    from <- paste0(paste0(extdata_folder,'/'),zfiles[i])
    to <- paste0(paste0(Analyte,'/'),zfiles[i])
    if (file.exists(from) & !file.exists(to)) {
      file.copy(from,to,overwrite = FALSE)
      #file.remove(from) # remove file?
    }
  }

  # Move Channel Survey data file
  zfiles <- files[grepl('xs.RDS',files)]
  for (i in 1:length(zfiles)) {
    from <- paste0(paste0(extdata_folder,'/'),zfiles[i])
    to <- paste0(paste0(Channel,'/'),zfiles[i])
    if (file.exists(from) & !file.exists(to)) {
      file.copy(from,to,overwrite = FALSE)
      #file.remove(from) # remove file?
    }
  }

  # Move Discharge data file
  zfiles <- files[grepl('Q.CSV',files)]
  for (i in 1:length(zfiles)) {
    from <- paste0(paste0(extdata_folder,'/'),zfiles[i])
    to <- paste0(paste0(Discharge,'/'),zfiles[i])
    if (file.exists(from) & !file.exists(to)) {
      file.copy(from,to,overwrite = FALSE)
      #file.remove(from) # remove file?
    }
  }

  # Move Offset data file
  zfiles <- files[grepl('Offset',files)]
  for (i in 1:length(zfiles)) {
    from <- paste0(paste0(extdata_folder,'/'),zfiles[i])
    to <- paste0(paste0(Offsets,'/'),zfiles[i])
    if (file.exists(from) & !file.exists(to)) {
      file.copy(from,to,overwrite = FALSE)
      #file.remove(from) # remove file?
    }
  }

  # Move Sonde and Height data files
  zfiles <- files[grepl('Logger.csv',files)]
  for (i in 1:length(zfiles)) {
    from <- paste0(paste0(extdata_folder,'/'),zfiles[i])
    to <- paste0(paste0(Sonde_and_Height,'/'),zfiles[i])
    if (file.exists(from) & !file.exists(to)) {
      file.copy(from,to,overwrite = FALSE)
      #file.remove(from) # remove file?
    }
  }


}
