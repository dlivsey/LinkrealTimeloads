#' Executes code provided in vignette('LinkrealTimeloads')
#'
#' Writes folders and files using code in vignette('LinkrealTimeloads') to folder path specified by user_data_folder. Uses example data files provided package folder LinkrealTimeloads/extdata
#'
#' @param user_data_folder file path to user data folder
#' @note All functions expect data to be in following folders:
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
#' Where "site" is a specific site (e.g., for Johnstone at Innisfail use site <-
#' "JRI")
#'
#' See vignette('LinkrealTimeloads') for further instructions
#'
#' @return List with suspended-sediment load estimates at Johnstone River at Innisfail saved to user_data_folder/site/Loads.rds
#' @author Daniel Livsey (2023) ORCID: 0000-0002-2028-6128
#' @examples
#' \dontrun{
#' # Writes to user's R library, e.g., /R/library/../LinkrealTimeloads/extdata/Example
#' user_data_folder <- paste0(system.file("extdata", package = "LinkrealTimeloads"),'/Example')
#' Output <- Example(user_data_folder)
#' }
#' @references Stephen Wallace (2023, DES) provided crucial functions in
#' Import_Channelmaster_Data() to extract data from binary 000 files.
#'
#' Livsey D.N. (2023). realTimeloads: Analyte Flux and Load from Estimates of
#' Concentration and Discharge_. R package version 1.0.0.
#'
#' Livsey, D.N. (in review). National Industry Guidelines for hydrometric
#' monitoring–Part 12: Application of acoustic Doppler velocity meters to
#' measure suspended-sediment load. Bureau of Meteorology. Melbourne, Australia
#'
#' @export
Example <- function(user_data_folder) {

# Acronym for "Johnstone River at Innisfail"
site <- 'JRI'

# Write folders to user_data_folder/site using initialize_new_site_directories() and copy files to appropriate folders
LinkrealTimeloads::initialize_folders_and_move_data_files(user_data_folder,site)

# Call Link_to_Real_time_loads() to run package functions in appropriate order.
LinkrealTimeloads::Link_to_Real_time_loads(user_data_folder,site)

Output <- LinkrealTimeloads::Compute_load(user_data_folder,site)

# Save list with discharge, TSS, and loads for reporting and plotting
save(Output,file = paste0(paste0(paste0(user_data_folder,'/'),site),'/Loads.rds'))

return(Output)

}
