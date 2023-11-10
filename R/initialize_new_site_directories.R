#' Writes directories expected by LinkrealTimeloads and updates Site_List
#'
#' Writes folders listed in note below and will update Site_List.rds if Site_Number is provided
#'
#' @param user_data_folder file path to user data folder
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
#' @return Writes folders listed in note below and will update Site_List.rds if Site_Number is provided
#' @seealso
#' \code{\link{Link_to_Real_time_loads}} Process data in specified folder structure using realTimeloads package
#'
#' @author Daniel Livsey (2023) ORCID: 0000-0002-2028-6128
#' @examples
#' # See vignette('LinkrealTimeloads',package = LinkrealTimeloads)
#' @references
#' Stephen Wallace (2023, DES) provided crucial functions in Import_Channelmaster_Data() to extract data from binary 000 files.
#'
#' Livsey D.N. (2023). realTimeloads: Analyte Flux and Load from Estimates of Concentration and Discharge_. R package version 1.0.0.
#'
#' Livsey, D.N. (in review). National Industry Guidelines for hydrometric monitoring–Part 12: Application of acoustic Doppler velocity meters to measure suspended-sediment load. Bureau of Meteorology. Melbourne, Australia
#'
#' @export
initialize_new_site_directories <- function(user_data_folder,site,Site_Number = NULL,Site_Name=NULL) {
#site <- 'MRD'
#user_data_folder <- "C:/Users/livseyd/OneDrive - Queensland University of Technology/Documents/R/LoadDashboard/user_data_test"



site_folder <- paste0(paste0(user_data_folder,'/'),site)
ADCP <- paste0(paste0(user_data_folder,'/'),paste0(site,'/ADCP_data'))
Analyte <- paste0(paste0(user_data_folder,'/'),paste0(site,'/Analyte_data'))
Channel <- paste0(paste0(user_data_folder,'/'),paste0(site,'/Channel_Geometry'))
Discharge <- paste0(paste0(user_data_folder,'/'),paste0(site,'/Discharge_data'))
Offsets <-  paste0(paste0(user_data_folder,'/'),paste0(site,'/Height_Offsets'))
Sonde_and_Height <-  paste0(paste0(user_data_folder,'/'),paste0(site,'/Sonde_and_Height_data'))

# write directories only if not present
ifelse(!dir.exists(user_data_folder), dir.create(user_data_folder), "user_data_folder exists already")
ifelse(!dir.exists(site_folder), dir.create(site_folder), "user_data_folder/site exists already")
ifelse(!dir.exists(ADCP), dir.create(ADCP), "site/ADCP_data exists already")
ifelse(!dir.exists(Analyte), dir.create(Analyte), "site/Analyte_data exists already")
ifelse(!dir.exists(Channel), dir.create(Channel), "site/Channel_Geometry exists already")
ifelse(!dir.exists(Discharge), dir.create(Discharge), "site/Discharge_data exists already")
ifelse(!dir.exists(Offsets), dir.create(Offsets), "site/Height_Offsets exists already")
ifelse(!dir.exists(Sonde_and_Height), dir.create(Sonde_and_Height), "site/ADCP_data exists already")

# update Site_List if Site_Number is provided
if (!is.null(Site_Number)) {
Site_list_update(user_data_folder,Site_Folder = site,Site_Number,Site_Name)
}

return(invisible(NULL))

}

