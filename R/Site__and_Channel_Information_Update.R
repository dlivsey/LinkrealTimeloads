#' Adds metadata to *.xs.RDS files from Stephen Wallace. Called in Link_to_Real_time_loads()
#'
#' Adds ADCP_distance_from_right_bank_or_chainage_m and ADCP bank location to *.xs.RDS files from Stephen Wallace, these values must be manually added for new sites
#'
#' @param user_data_folder file path to user data folder
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
#' .xs.RDS must be updated when new cross-sections are surveyed Original *.xs.RDS files from Stephen Wallace
#'
#' ADCP_distance_from_right_bank_or_chainage_m and ADCP_bank_location must be manually indicated for each site below.
#'
#' Site_list_update() and Site__and_Channel_Information_Update() must be manually updated when adding new sites
#'
#' @return Metadata updated to *xs.RDS files in site/Channel_Geometry
#' \code{\link{Link_to_Real_time_loads}} Process data in specified folder structure using realTimeloads package
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
Site__and_Channel_Information_Update <- function(user_data_folder) {
# *.xs.RDS must be updated when new cross-sections are  surveyed
# Original *.xs.RDS files from Stephen Wallace
# ADCP_distance_from_right_bank_or_chainage_m and ADCP_bank_location must be manually indicated for each site below.

### MRD "Mullgrave at Deeral" --------------
file <- paste0(user_data_folder,"/MRD/Channel_Geometry/mrdxs.RDS")
file2 <- paste0(user_data_folder,"/MRD/Height_Offsets/Height_Offsets.RDS")
if (file.exists(file)) {
z<- readRDS(file)
z2 <- readRDS(file2)
# Adding ADCP location data frame
PeriodStart <- z$Periods$PeriodStart
TableNum <- z$Periods$TableNum
ADCP_location <-data.frame(PeriodStart)
ADCP_location$ADCP_distance_from_right_bank_or_chainage_m <- 1474.97 # 1474.969, reported by Stephen Wallace via Teams
ADCP_location$ADCP_bank_location <- 'left'
interpolate <- approx(z2$time,z2$ADCP_elevation_above_gauge_datum_m,PeriodStart,rule = 2,method = "constant")
ADCP_location$ADCP_elevation_above_gauge_datum_m <- interpolate$y
ADCP_location$TableNum <- TableNum
# check for changes in height that may have occurred since last survey and merge to ADCP_location if needed
# assumes no change in ADCP cross-channel location
if (max(z2$time)>max(PeriodStart)) {
  un <- unique(z2$ADCP_elevation_above_gauge_datum_m)
  if (length(un)>1) {
  tnew <- PeriodStart[1]
  hnew <- c()
  for (j in 2:length(un)) {
    ind <- z2$ADCP_elevation_above_gauge_datum_m==un[j]
    if  (min(z2$time[ind])>max(PeriodStart)) {
      tnew[j-1] <- min(z2$time[ind])
      hnew[j-1] <- min(z2$ADCP_elevation_above_gauge_datum_m[ind])
    }
  }
  ADCP_distance_from_right_bank_or_chainage_m <- rep(1,length(tnew))*ADCP_location$ADCP_distance_from_right_bank_or_chainage_m[1]
  df <- data.frame(tnew,ADCP_distance_from_right_bank_or_chainage_m)
  colnames(df)<- c("PeriodStart","ADCP_distance_from_right_bank_or_chainage_m")
  df$ADCP_bank_location <- ADCP_location$ADCP_bank_location[1] # assume no change in bank location
  df$ADCP_elevation_above_gauge_datum_m <- hnew
  df$TableNum <- "No_Survey_But_Change_In_ADCP_Mount_Height"
  ADCP_location <- rbind(ADCP_location,df)
  }
}
z[['ADCP_Location']] <- ADCP_location
saveRDS(z,file)
}

### JRI "Johnstone at Innisfail" --------------
file <- paste0(user_data_folder,"/JRI/Channel_Geometry/jrixs.RDS")
file2 <- paste0(user_data_folder,"/JRI/Height_Offsets/Height_Offsets.RDS")
if (file.exists(file)) {
z<- readRDS(file)
z2 <- readRDS(file2)
# Adding ADCP location data frame
PeriodStart <- z$Periods$PeriodStart
TableNum <- z$Periods$TableNum
ADCP_location <-data.frame(PeriodStart)
ADCP_location$ADCP_distance_from_right_bank_or_chainage_m <- 401.3 # Reported by Stephen Wallace via Teams
ADCP_location$ADCP_bank_location <- 'right'
interpolate <- approx(z2$time,z2$ADCP_elevation_above_gauge_datum_m,PeriodStart,rule = 2,method = "constant")
ADCP_location$ADCP_elevation_above_gauge_datum_m <- interpolate$y
ADCP_location$TableNum <- TableNum
# check for changes in height that may have occurred since last survey and merge to ADCP_location if needed
# assumes no change in ADCP cross-channel location
if (max(z2$time)>max(PeriodStart)) {
  un <- unique(z2$ADCP_elevation_above_gauge_datum_m)
  if (length(un)>1) {
    tnew <- PeriodStart[1]
    hnew <- c()
    for (j in 2:length(un)) {
      ind <- z2$ADCP_elevation_above_gauge_datum_m==un[j]
      if  (min(z2$time[ind])>max(PeriodStart)) {
        tnew[j-1] <- min(z2$time[ind])
        hnew[j-1] <- min(z2$ADCP_elevation_above_gauge_datum_m[ind])
      }
    }
    ADCP_distance_from_right_bank_or_chainage_m <- rep(1,length(tnew))*ADCP_location$ADCP_distance_from_right_bank_or_chainage_m[1]
    df <- data.frame(tnew,ADCP_distance_from_right_bank_or_chainage_m)
    colnames(df)<- c("PeriodStart","ADCP_distance_from_right_bank_or_chainage_m")
    df$ADCP_bank_location <- ADCP_location$ADCP_bank_location[1] # assume no change in bank location
    df$ADCP_elevation_above_gauge_datum_m <- hnew
    df$TableNum <- "No_Survey_But_Change_In_ADCP_Mount_Height"
    ADCP_location <- rbind(ADCP_location,df)
  }
}
z[['ADCP_Location']] <- ADCP_location
saveRDS(z,file)
}
}









