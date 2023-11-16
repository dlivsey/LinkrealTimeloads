#' Processes all data in site folders and compute load
#'
#' Processes data in site folders and computes load after user has manually placed files in site folder. If wait_for_user = TRUE initialize_new_site_directories() writes directories expected by package code and provides dialogue box to wait for user to indicate when data files have been placed into site folders
#'
#' @param user_data_folder file path to user data folder
#' @param site site folder under user_data_folder
#' @param Site_Number site number for site
#' @param Site_Name site name for plotting purposes
#' @param wait_for_user Boolean to wait for user to place files
#' @param write_QAQC_report Boolean to call QAQC_Report()
#' @param QAQC_Parameters list of parameters needed for QAQC_Report()
#'
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
#' @return List with suspended-sediment load estimates and saved to user_data_folder/site/Loads.rds
#' @author Daniel Livsey (2023) ORCID: 0000-0002-2028-6128
#' @examples
#' \dontrun{
#' Output <- Process_data_and_compute_load(user_data_folder,site,wait_for_user = FALSE)
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
Process_data_and_compute_load <- function(user_data_folder,site,Site_Number = NULL,Site_Name = NULL,wait_for_user = FALSE,write_QAQC_report = FALSE,QAQC_Parameters = NULL) {

  # Write default Site List
  Site_list_update(user_data_folder)
  # Read-in Site_List
  Site_List <- readRDS(paste0(user_data_folder,"/Site_List.rds"))

  # Ensure Site_Number and Site_Name are given if new site or not in current Site_List
  if (sum((Site_List$Site_Folder==site))==0 & is.null(Site_Number) | sum((Site_List$Site_Folder==site))==0 & is.null(Site_Name)) {
  stop(paste0('When site is not in current Site_List at ',user_data_folder,'/Site_List.rds',' Site_Number and Site_Name must be provided'))
  }

  answer <- 'temp'
  if (wait_for_user) {
  # Write folders to user_data_folder/site using initialize_new_site_directories()
  initialize_new_site_directories(user_data_folder,site,Site_Number,Site_Name)

  answer<-utils::winDialog("okcancel","Waiting for user to place files into site folder. Select OK to continue or Cancel to stop code execution")
  if (answer!='OK') {
    stop('User must place all files into site folders, please run and inspect outputs from LinkrealTimeLoads::Example()')
  }
  }

  if (answer=='OK'| !wait_for_user) {

  # Call Link_to_Real_time_loads() to run package functions in appropriate order.
  LinkrealTimeloads::Link_to_Real_time_loads(user_data_folder,site)

  Output <- LinkrealTimeloads::Compute_load(user_data_folder,site)

  # Save list with discharge, TSS, and loads for reporting and plotting
  saveRDS(Output,file = paste0(paste0(paste0(user_data_folder,'/'),site),'/Loads.rds'))

  # Write QAQC report
  if (write_QAQC_report & !is.null(QAQC_Parameters)) {

    # Write QAQC report
    author <- QAQC_Parameters$author
    site_name <- QAQC_Parameters$site_name
    compute_from_time <- QAQC_Parameters$compute_from_time
    compute_to_time <- QAQC_Parameters$compute_to_time
    max_points <- QAQC_Parameters$max_points

    QAQC_Report(author,user_data_folder,site,site_name,compute_from_time,compute_to_time,max_points)

  }

  return(Output)
  }

}
