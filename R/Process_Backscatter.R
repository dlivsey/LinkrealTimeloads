#' Process acoustic backscatter and save to ADCP (i.e., 000) rds file
#'
#' Process acoustic backscatter using realTimeloads with methods detailed in Livsey (in review)
#'
#' @param user_data_folder file path to user data folder
#' @param site name of site folder under user_data_folder
#' @param overwrite overwrite previous outputs
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
#' Import_Channelmaster_Data() should be run first
#'
#' @return Saves processed acoustic backscatter to each 000-related rds file in site/ADCP_data
#'
#' \code{\link{Link_to_Real_time_loads}} Process data in specified folder structure using realTimeloads package
#' \code{\link{Import_Channelmaster_Data}} Imports ADCP data from 000 files
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
Process_Backscatter <- function(user_data_folder,site = NULL,overwrite=FALSE) {
# user_data_folder where all site folders are located
# site: site folder:
# process data in specific site folder if desired, otherwise processes all data by site in user_data_folder

 #user_data_folder <- "C:/Users/livseyd/OneDrive - Queensland University of Technology/Documents/R/LoadDashboard/user_data"
 # site <- "MRD" # Mullgrave River at Deeral

all_site_folders <- list.dirs(user_data_folder,recursive = "FALSE")

# get specific site folder if site is provided
if (!is.null(site)) {
  all_site_folders <- all_site_folders[grepl(site,all_site_folders)]
}

number_of_sites <- length(all_site_folders)
Site_List <- readRDS(paste0(user_data_folder,'/Site_List.rds'))

for (k in 1:number_of_sites) {
folder <- get_folders_b(user_data_folder,all_site_folders[k])

allADCP <- readRDS(paste0(folder$ADCP,'/All_ADCP_Programming_Data.rds'))

# load imputed files if present, otherwise load original data files
if (file.exists(paste0(folder$Sonde_and_Height,'/Height_Imputed.rds'))) {
Height <- readRDS(paste0(folder$Sonde_and_Height,'/Height_Imputed.rds'))$Imputed_data
}
if (!file.exists(paste0(folder$Sonde_and_Height,'/Height_Imputed.rds'))) {
  Height <- readRDS(paste0(folder$Sonde_and_Height,'/Height.rds'))
}
# load imputed files if present
if (file.exists(paste0(folder$Sonde_and_Height,'/Sonde_Imputed.rds'))) {
Sonde <- readRDS(paste0(folder$Sonde_and_Height,'/Sonde_Imputed.rds'))$Imputed_data
}
if (!file.exists(paste0(folder$Sonde_and_Height,'/Sonde_Imputed.rds'))) {
  Sonde <- readRDS(paste0(folder$Sonde_and_Height,'/Sonde.rds'))
}

# Format Sonde data frame for acoustic_backscatter_processing()
# set sonde temp to 25 b/c "Conductance" from Eagle IO is actually specific conductance relative to 25 deg C
colnames(Sonde)["Specific_conductance_uS_per_cm_at_25_deg_C"==colnames(Sonde)] <- "Conductivity_uS_per_cm" # Real Time Loads expects "Conductivity_uS_per_cm"
Sonde$Water_Temperature_degC <- 25
Sonde$Pressure_dBar <- 0 # Sondes are usually in cells, dbar is just used for computation of salinity.

# at JRI Sonde contains data from JRI and JRC, prefer JRI when available
if (length(unique(Sonde$Site_number))>1 & sum(grepl('1120053',unique(Sonde$Site_number)))==1) {
JRIs <- min(Sonde$time[grepl("1120053",Sonde$Site_number)])
Sonde <- rbind(Sonde[Sonde$time<JRIs,],Sonde[Sonde$time>=JRIs & grepl("1120053",Sonde$Site_number),])
}

files <- list.files(path = folder$ADCP, pattern = "*\\.000", recursive = TRUE,full.names = TRUE,include.dirs = TRUE)
files <- gsub('.000','.rds',files)
filesRDS <- list.files(path = folder$ADCP, pattern = "*\\.rds", recursive = TRUE,full.names = TRUE,include.dirs = TRUE)
files <- files[is.element(files,filesRDS)] # only use rds files from 000 files

# process each rds file generated from a 000 file
for (i in 1:length(files)) {
  z000 <- readRDS(files[i])
  df <- z000$data[[1]]

  # list and no mean_sediment_corrected_backscatter_beam_1_dB computed
  c1 <- typeof(df)=="list" & !is.element('mean_sediment_corrected_backscatter_beam_1_dB',colnames(df))
  # list and  overwrite == TRUE
  c2 <- typeof(df)=="list" & overwrite==TRUE

  if (c1 | c2) {
  # try to process 000 file, if fails will move to next file
  try(process_file(files,i,z000,df,allADCP,Height,Sonde),silent = TRUE)
  } # c1 or c2 met
} # i files

} # k folders

msg <- 'Process_Backscatter.R complete'

return(invisible(msg))

} # end of function

# Internal functions
process_file <- function(files,i,z000,df,allADCP,Height,Sonde) {
  print(paste(paste(paste('Processing file',i),'of'),length(files)))
  print(files[i])
  # ensured code can accommodate change in cell settings w/in a 000 file
  # CHM3627880080.000 has a change in cell setting w/in the file
  # i<-grep('CHM3627880080.rds',files) # "CHM3627880080.rds" has change w/in the 000 file

  # pre-allocate variables
  df$mean_sediment_corrected_backscatter_beam_1_dB <- NA
  df$mean_sediment_corrected_backscatter_beam_2_dB <- NA
  df$attenuation_of_sound_due_to_sediment_beam_1_dB_per_m <- NA
  df$attenuation_of_sound_due_to_sediment_beam_2_dB_per_m <- NA

  # check for change in cell settings within the 000 file data
  #acoustic_backscatter_processing requires Blanking_distance_m, Number_of_Cells, and
  #Bin_Size_m to be constant
  z2 <- unique(df[c("Blanking_distance_m","Number_of_Cells","Bin_Size_m","Instrument_serial_number","CPU_serial_number")])
  ind <- list() # indices of unique cell settings in df
  # process data in blocks identified in z2
  for (j in 1:nrow(z2)) {
    ind[[j]] <- df$Blanking_distance_m==z2$Blanking_distance_m[j] & df$Number_of_Cells==z2$Number_of_Cells[j] & df$Bin_Size_m==z2$Bin_Size_m[j]
    # ADCP data frame for acoustic_backscatter_processing()
    ADCP <- df[ind[[j]],]
    # 2004 is default time of ADCP do not run block if before 2006
    if (max(ADCP$time)>"2006-01-01 00:00:00 AEST") {

      # Format Echo Instensity to dataframe for realTimeloads
      EIa <- data.frame(ADCP$Site_number,ADCP$time,do.call(rbind,ADCP$Echo_Intensity_Beam_1))
      EIb <- data.frame(ADCP$Site_number,ADCP$time,do.call(rbind,ADCP$Echo_Intensity_Beam_2))
      colnames(EIa)[1]<-'Site_number'
      colnames(EIb)[1]<-'Site_number'
      colnames(EIa)[2]<-'time'
      colnames(EIb)[2]<-'time'
      colnames(EIa)[grepl('X',colnames(EIa))] <- gsub('X','Echo_Intensity_Counts_cell_',colnames(EIa)[grepl('X',colnames(EIa))])
      colnames(EIb)[grepl('X',colnames(EIb))] <- gsub('X','Echo_Intensity_Counts_cell_',colnames(EIb)[grepl('X',colnames(EIb))])
      # get all data recorded by specific adcp and check for any ambient noise level data to estimate Instrument Noise Level, Instrument Noise Level is used if no ambient noise level data are available in 000 file
      ab1 <- allADCP[is.element(allADCP$Site_number,ADCP$Site_number)&is.element(allADCP$Instrument_serial_number,ADCP$Instrument_serial_number)&is.element(allADCP$CPU_serial_number,ADCP$CPU_serial_number),"Ambient_Noise_Level_Beam_1_Counts"]
      ab2 <- allADCP[is.element(allADCP$Site_number,ADCP$Site_number)&is.element(allADCP$Instrument_serial_number,ADCP$Instrument_serial_number)&is.element(allADCP$CPU_serial_number,ADCP$CPU_serial_number),"Ambient_Noise_Level_Beam_1_Counts"]
      if (sum(is.finite(ab1))==0 & sum(is.finite(ab2))==0) {
        #See Table 2 of https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2016WR019695 # for ambient noise level of 1200, 600, and 300 kHz RDI ADCP
        # ambient noise level for RDI ADCP in Table 2 row "NL by Record Minimum (Counts)" ranges from ~ 30 to 40, setting nominal Instrument_Noise_Level to 35 counts if no ambient noise level data are available
        Instrument_Noise_Level <- 35 # Counts,
      }
      if (sum(is.finite(ab1))>0 | sum(is.finite(ab2))>0) {
        # if any ambient noise level data are present set Instrument_Noise_Level to min value of all available data assuming that lowest ambient noise level measurments is nearest to actual Instrument_Noise_Level
        Instrument_Noise_Level <- min(rbind(ab1,ab2),na.rm = TRUE) # Counts,
      }

      # Format site dataframe for realTimeloads
      # note b/c ADCP data have ADCP$Range_to_bed_of_acoustic_beams_m and ADCP$Range_to_bed_of_acoustic_beams_m and ADCP$Range_to_water_surface_of_acoustic_beams_m, data in Site is not used in acoustic_backscatter_processing() but is needed to ensure acoustic_backscatter_processing() still runs
      Site_number <- ADCP$Site_number[1]
      ADCP_elevation_above_gauge_datum_m <- ADCP$ADCP_elevation_above_gauge_datum_m[1]
      Distance_of_gauge_datum_below_thalweg_m <- mean(ADCP$Thalweg_relative_to_gauge_datum_m)
      ADCP_elevation_above_bed_m <- ADCP$ADCP_elevation_above_gauge_datum_m[1]-Distance_of_gauge_datum_below_thalweg_m
      Site <- data.frame(Site_number,ADCP_elevation_above_bed_m,Distance_of_gauge_datum_below_thalweg_m,ADCP_elevation_above_gauge_datum_m)


      Output <- realTimeloads::acoustic_backscatter_processing(
        Site,
        ADCP,
        Height,
        Sonde,
        Echo_Intensity_Beam_1 = EIa,
        Echo_Intensity_Beam_2 = EIb,
        Instrument_Noise_Level,
        Include_Rayleigh = TRUE,
        Include_near_field_correction = TRUE
      )

      # Store processed backcatter data to df of RDS file
      df$mean_sediment_corrected_backscatter_beam_1_dB[ind[[j]]] <- Output$mean_sediment_corrected_backscatter_beam_1_dB
      df$mean_sediment_corrected_backscatter_beam_2_dB[ind[[j]]] <- Output$mean_sediment_corrected_backscatter_beam_2_dB
      df$attenuation_of_sound_due_to_sediment_beam_1_dB_per_m[ind[[j]]] <- Output$attenuation_of_sound_due_to_sediment_beam_1_dB_per_m
      df$attenuation_of_sound_due_to_sediment_beam_2_dB_per_m[ind[[j]]] <- Output$attenuation_of_sound_due_to_sediment_beam_2_dB_per_m
    }
  } # j blocks

  # save processed backscatter data in df back in RDS file
  z000$data[[1]] <- df
  saveRDS(z000,files[i])
}

# function to get folder paths
get_folders_b <- function(user_data_folder,site) {
  ADCP <- paste0(site,'/ADCP_data')
  Analyte <- paste0(site,'/Analyte_data')
  Channel <- paste0(site,'/Channel_Geometry')
  Discharge <- paste0(site,'/Discharge_data')
  Offsets <- paste0(site,'/Height_Offsets')
  Sonde_and_Height <- paste0(site,'/Sonde_and_Height_data')
  folders <- data.frame(user_data_folder,site,ADCP,Analyte,Channel,Discharge,Offsets,Sonde_and_Height)
  return(folders)
}
