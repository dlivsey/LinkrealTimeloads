#' Concatenates processed acoustic backscatter data from Process_Backscatter()
#'
#' Concatenates processed acoustic backscatter data from Process_Backscatter() for subsequent analysis in Predict_TSS_from_SCI()
#'
#' @param user_data_folder file path to user data folder
#' @param site site folder under user_data_folder
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
#' @section Warning:
#' Process_Backscatter() should be run prior to this function
#'
#' @return Processed_Backscatter_data concatenated from all 000 rds files. Output written to site/ADCP_data/Processed_Backscatter_data.rds
#'
#' @seealso
#' \code{\link{Link_to_Real_time_loads}} Process data in specified folder structure using realTimeloads package
#'
#' \code{\link{Process_Backscatter}} Process acoustic backscatter data
#'
#' @author Daniel Livsey (2023) ORCID: 0000-0002-2028-6128
#' @examples
#' # See vignette('LinkrealTimeloads',package = LinkrealTimeloads)
#' @references
#' Stephen Wallace (2023, DES) provided crucial functions in Import_Channelmaster_Data() to extract data from binary 000 files.
#'
#' Livsey D.N. (2023). realTimeloads: Analyte Flux and Load from Estimates of Concentration and Discharge. R package version 1.0.0.
#'
#' Livsey, D.N. (in review). National Industry Guidelines for hydrometric monitoring–Part 12: Application of acoustic Doppler velocity meters to measure suspended-sediment load. Bureau of Meteorology. Melbourne, Australia
#'
#' @export
Concatenate_Backscatter <- function(user_data_folder,site=NULL) {


all_site_folders <- list.dirs(user_data_folder,recursive = "FALSE")

# get specific site folder if site is provided
if (!is.null(site)) {
  all_site_folders <- all_site_folders[grepl(site,all_site_folders)]
}

s <- strsplit(all_site_folders,'/')

number_of_sites <- length(all_site_folders)

for (k in 1:number_of_sites) {
  site <- s[[k]][length(s[[k]])]
  Site <-readRDS(paste0(user_data_folder,'/Site_List.rds'))
  Site_Number <- Site$Site_Number[is.element(Site$Site_Folder,site)]
  # folder path
  site_folder <- paste0(paste0(user_data_folder,"/"),site)

  #files <- list.files(path = paste0(site_folder,'/ADCP_data'), pattern = "*\\.000", recursive = TRUE,full.names = TRUE,include.dirs = TRUE)
  #files <- gsub('.000','.rds',files)

  files <- list.files(path = paste0(site_folder,'/ADCP_data'), pattern = "*\\.000", recursive = TRUE,full.names = TRUE,include.dirs = TRUE)
  files <- gsub('.000','.rds',files)
  filesRDS <- list.files(path = paste0(site_folder,'/ADCP_data'), pattern = "*\\.rds", recursive = TRUE,full.names = TRUE,include.dirs = TRUE)
  files <- files[is.element(files,filesRDS)] # only use rds files from 000 files

  # read-in each rds file generated from a 000 file and check for processed backscatter data
  list_item <- 1
  data <- list()
  vars <- c('Site_number','time','mean_sediment_corrected_backscatter_beam_1_dB','mean_sediment_corrected_backscatter_beam_2_dB','attenuation_of_sound_due_to_sediment_beam_1_dB_per_m','attenuation_of_sound_due_to_sediment_beam_2_dB_per_m')
  vars2 <- c('mean_sediment_corrected_backscatter_beam_1_dB','mean_sediment_corrected_backscatter_beam_2_dB')

  for (i in 1:length(files)) {
    z000 <- readRDS(files[i])
    df <- z000$data[[1]]
    if (is.element('mean_sediment_corrected_backscatter_beam_1_dB',names(df))) {
    data <- rbind(data,df[,vars])
    }
  }


  ibad <- as.double(is.finite(data[,vars2[1]])) + as.double(is.finite(data[,vars2[2]])) == 0 | data$time < "2005-01-01 00:00:00 AEST"

  data <- data[!ibad,] # remove rows w/ no data or when ADCP time was not set properly
  data <- data[order(data$time),] # order by time
  data <- data[!duplicated(data$time),] # remove duplicate times, at this time no way to know which duplicate data is "correct" simply removing duplicate time punch, should add QA/QC to determine between readings if reading has a duplicate time and has different reading

  fileRDS <- paste0(site_folder,'/ADCP_data/Processed_Backscatter_data.rds')
  #print(fileRDS)
  # Check if any new data was imported if rds file present
  if (file.exists(fileRDS)) {
    do <- readRDS(fileRDS)
    inew <- !is.element(do$time,data$time)
    Output <- rbind(data,do[inew,]) # combine
    Output <- Output[order(Output$time),] # order time
    Output <- Output[!duplicated(Output[,c('time')]),] # remove duplicate time
    data <- Output
  }
  saveRDS(data,fileRDS)

}

}
