#' Infills missing height, discharge, and conductivity data using realTimeloads::impute_data()
#'
#' Code will impute data if days since last imputation threshold since "to_time" is reached
#'
#' @param user_data_folder file path to user data folder
#' @param site site folder under user_data_folder
#' @param from_time date to start imputation
#' @param to_time date to end imputation
#' @param overwrite logical to overwrite all previous imputation data
#' @param MC number of Monte Carlo simulations used in uncertainty estimation in realTimeloads::impute_data
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
#' Users should ensure imputation is reasonable and adjust predictors as needed.
#' @return Imputed data in rds files *_Imputed.rds written to site/Sonde_and_Height_data and site/Discharge_data
#'
#' @seealso
#' \code{\link{Link_to_Real_time_loads}} Process data in specified folder structure using realTimeloads package
#'
#' \code{\link{Predict_TSS_from_SCI}} Estimates TSS from processed acoustic backscatter and turbidity
#'
#' @author Daniel Livsey (2023) ORCID: 0000-0002-2028-6128
#' @examples
#' # See vignette('LinkrealTimeloads',package = LinkrealTimeloads)
#' @references
#' Stephen Wallace (2023, DES) provided crucial functions in Import_Channelmaster_Data() to extract data from binary 000 files.
#'
#' Livsey D.N. (2023). realTimeloads: Analyte Flux and Load from Estimates of Concentration and Discharge_. R package version 1.0.0.
#'
#' @export
Impute_height_discharge_velocity_and_conductivity <- function(user_data_folder,site,from_time,to_time,overwrite = FALSE,MC=1) {

  #library(realTimeloads)

  #setwd("C:/Users/livseyd/OneDrive - Queensland University of Technology/Documents/R/LoadDashboard")
  #user_data_folder <- "C:/Users/livseyd/OneDrive - Queensland University of Technology/Documents/R/LoadDashboard/user_data"
  #site <- 'MRD'
  #from_time <- as.POSIXct("2017-07-01 00:00:00 AEST")
  #to_time <- as.POSIXct("2023-09-01 00:00:00 AEST")
  #ti <- seq(from_time,from_time+60*60*24*100,by = "10 min") # use 100 days for testing
  #overwrite = F

  ### uniform timestep for imputation ----
  ti <- seq(from_time,to_time,by = "60 min")

  ### Load data ----
  Height <- readRDS(paste0(paste0(user_data_folder,paste0('/',site)),"/Sonde_and_Height_data/Height.rds"))
  Offsets <- readRDS(paste0(paste0(user_data_folder,paste0('/',site)),"/Height_Offsets/Height_Offsets.rds"))
  Discharge <- readRDS(paste0(paste0(user_data_folder,paste0('/',site)),"/Discharge_data/Discharge.rds"))
  ADCP <- readRDS(paste0(paste0(user_data_folder,paste0('/',site)),"/ADCP_data/All_ADCP_Programming_Data.rds"))
  Sonde <- readRDS(paste0(paste0(user_data_folder,paste0('/',site)),"/Sonde_and_Height_data/Sonde.rds"))

  iPressure <- grepl("Pressure_line_elevation_above_gauge_datum_m",colnames(Offsets))
  # At JRI no level for pressure sensor is reported, assume values is minimum of height observed, use dH below to ensure one is not using bad data
  if (sum(iPressure)==0) {
    Offsets$Pressure_line_elevation_above_gauge_datum_m <- min(Height$Height_m)
  }

  ### Check time in imputed height file if present ----
  days_since_last_H_imputation <- Inf
  imputed_Height_file <- paste0(paste0(user_data_folder,paste0('/',site)),"/Sonde_and_Height_data/Height_Imputed.rds")
  if (file.exists(imputed_Height_file)) {
    tio <- readRDS(imputed_Height_file)$Imputed_data$time
    days_since_last_H_imputation <- as.double(difftime(to_time,max(tio),units = 'days'))
    print(days_since_last_H_imputation)
  }


  ### Check time in imputed discharge file if present ----
  days_since_last_D_imputation <- Inf
  imputed_Discharge_file <- paste0(paste0(user_data_folder,paste0('/',site)),"/Discharge_data/Discharge_Imputed.rds")
  if (file.exists(imputed_Discharge_file)) {
    tio <- readRDS(imputed_Discharge_file)$Imputed_data$time
    days_since_last_D_imputation <- as.double(difftime(to_time,max(tio),units = 'days'))
    print(days_since_last_D_imputation)
  }


  ### Check time in imputed velocity file if present ----
  days_since_last_U_imputation <- Inf
  imputed_Velocity_file <- paste0(paste0(user_data_folder,paste0('/',site)),"/Discharge_data/Velocity_Imputed.rds")
  if (file.exists(imputed_Velocity_file)) {
    tio <- readRDS(imputed_Velocity_file)$Imputed_data$time
    days_since_last_U_imputation <- as.double(difftime(to_time,max(tio),units = 'days'))
    print(days_since_last_U_imputation)
  }

  ### Check time in imputed sonde file if present ----
  days_since_last_S_imputation <- Inf
  imputed_Sonde_file <- paste0(paste0(user_data_folder,paste0('/',site)),"/Sonde_and_Height_data/Sonde_Imputed.rds")
  if (file.exists(imputed_Sonde_file)) {
    tio <- readRDS(imputed_Sonde_file)$Imputed_data$time
    days_since_last_S_imputation <- as.double(difftime(to_time,max(tio),units = 'days'))
    print(days_since_last_S_imputation)
  }

  # Delete files if overwriting
  if (overwrite) {
    if (file.exists(imputed_Height_file)) file.remove(imputed_Height_file)
    if (file.exists(imputed_Discharge_file)) file.remove(imputed_Discharge_file)
    if (file.exists(imputed_Velocity_file)) file.remove(imputed_Velocity_file)
    if (file.exists(imputed_Sonde_file)) file.remove(imputed_Sonde_file)
    days_since_last_H_imputation <- Inf
    days_since_last_D_imputation <- Inf
    days_since_last_U_imputation <- Inf
    days_since_last_S_imputation <- Inf
  }

  ### Impute height on desired intervals (e.g, monthly)  ----
  if (days_since_last_H_imputation>30) {
    # Get timeseries of height of pressure line for QA/QC
    Sensor_Height <- approx(Offsets$time,Offsets$Pressure_line_elevation_above_gauge_datum_m,Height$time,method = "constant",rule = 2)$y
    dH <- 0.1 # some threshold for minimum depth above sensor
    t <- Height$time
    H <- Height$Height_m
    H[H<Sensor_Height+dH] <- NA

    # (time,x,Xreg = NULL,ti = NULL,hfit = NULL,harmonic=FALSE,only_use_Xreg=FALSE,MC = 1,ptrain = 1)
    Output <- realTimeloads::impute_data(time=t,x=H,ti=ti,harmonic = TRUE,MC=MC,ptrain = 0.9)

    # add uncertainty column names from impute_data() if no uncertainty estimates, ensures rbind() works if user changes arguments to impute_data() w/ time
    uncertainty_cols <- c('x_at_minus_two_sigma_confidence','x_at_minus_one_sigma_confidence','x_at_median_confidence','x_at_plus_one_sigma_confidence','x_at_plus_two_sigma_confidence')
    if (sum(grepl('x_at',colnames(Output$Imputed_data)))==0) {
      df <- data.frame(matrix(NA,nrow = nrow(Output$Imputed_data),ncol=5))
      colnames(df) <-uncertainty_cols
      Output$Imputed_data <-  cbind(Output$Imputed_data,df)
    }

    colnames(Output$Imputed_data)[colnames(Output$Imputed_data)=='x'] <- 'Height_m'
    colnames(Output$Imputed_data)<-gsub('x_at','Height_m_at',colnames(Output$Imputed_data))

    # Add new data to existing imputed_Height_file
    if (file.exists(imputed_Height_file)) {
      iHo <- readRDS(imputed_Height_file)
      inew <- !is.element(iHo$Imputed_data$time,ti)
      Output$Imputed_data <- rbind(Output$Imputed_data,iHo$Imputed_data[inew,])
      Output$Imputed_data <- Output$Imputed_data[order(Output$Imputed_data$time),]
      Output$Imputed_data <- Output$Imputed_data[!duplicated(Output$Imputed_data [,c('time')]),]
    }
    saveRDS(Output,imputed_Height_file)
  }

  ### Impute discharge on desired intervals (e.g, monthly)  ----
  if (days_since_last_D_imputation>30) {

    t <- Discharge$time
    D <- Discharge$Discharge_m_cubed_per_s


    # use non-tidal stage and lagged dHdt as surrogate for discharge via known relationship w/ velocity
    Height_Imputed <- readRDS(imputed_Height_file)$Imputed_data

    # Height interpolated to ti
    Hi <- realTimeloads::linear_interpolation_with_time_limit(Height_Imputed$time,Height_Imputed$Height_m,ti,2)$x_interpolated

    # Non-tidal stage from Stephen Wallace's ccInterp
    # returns daily timestep need to get back to ti
    #Hnt <- ccInterp::ccInterpFilter(data.frame(ti,Hi),24)
    #Hnt <- linear_interpolation_with_time_limit(Hnt$Date,Hnt$avg,ti,2)$x_interpolated
    #readings_per_hour <- round(60/as.double(difftime(ti[2],ti[1],'mins')))
    #Hnt <- imputeTS::na_ma(Hnt, k = readings_per_hour,weighting = "simple")
    # decided not to use ccInterp since results to butterworth are similar overall

    # Non-tidal stage using USGS butterworth
    readings_per_hour <- round(60/as.double(difftime(ti[2],ti[1],'mins')))
    Hnt <- realTimeloads::butterworth_tidal_filter(ti,Hi)
    Hnt <- imputeTS::na_ma(Hnt, k = readings_per_hour*24,weighting = "simple") # fill gaps at start and end of record

    dHdt <- c(NA,diff(Hi))
    dHdt[1] <- dHdt[2]

    ## Since ccf does not accept NA, imputed discharge w/out dHdt and then used imputed data to determine peak correlation between lagged dHdt and discharge
    # for MRD, ccf peaks at -6, 10 min reads, i.e, dHdt leads discharge by one hr
    #cc$lag[min(cc$acf)==cc$acf] # negative value indicates x leads y
    #cc<-ccf(dHdt, Discharge_Imputed$Discharge_m_cubed_per_s, lag.max = 24)
    # assuming dt = 10 mins, n = c(3,6,9) will give dHdt_lagged at 30 mins, 1 hr, and 1.5 hrs
    dHdt_lagged <-data.table::shift(dHdt, n = c(3,6,9), fill=NA, type="lag", give.names=TRUE)

    dHdt_lagged <- data.frame(dHdt_lagged)
    for (j in 1:length(dHdt_lagged)) {
      dHdt_lagged[,j] <- imputeTS::na_ma(dHdt_lagged[,j],weighting = 'simple')
    }

    Output <- realTimeloads::impute_data(time=t,x=D,ti=ti,Xreg=cbind(Hnt,dHdt_lagged),harmonic = TRUE,MC=MC,ptrain = 0.9)
    # add uncertainty column names from impute_data() if no uncertainty estimates, ensures rbind() works if user changes arguments to impute_data() w/ time
    uncertainty_cols <- c('x_at_minus_two_sigma_confidence','x_at_minus_one_sigma_confidence','x_at_median_confidence','x_at_plus_one_sigma_confidence','x_at_plus_two_sigma_confidence')
    if (sum(grepl('x_at',colnames(Output$Imputed_data)))==0) {
      df <- data.frame(matrix(NA,nrow = nrow(Output$Imputed_data),ncol=5))
      colnames(df) <-uncertainty_cols
      Output$Imputed_data <-  cbind(Output$Imputed_data,df)
    }


    colnames(Output$Imputed_data)[colnames(Output$Imputed_data)=='x'] <- 'Discharge_m_cubed_per_s'
    colnames(Output$Imputed_data)<-gsub('x_at','Discharge_m_cubed_per_s_at',colnames(Output$Imputed_data))



    # plot(ti,Output$Imputed_data$Discharge_m_cubed_per_s,type='l',xlim = c(ti[3000],ti[3500]))
    # points(ti[Output$Imputed_data$imputed],Output$Imputed_data$Discharge_m_cubed_per_s[Output$Imputed_data$imputed],col='red')
    # points(ti[!Output$Imputed_data$imputed],Output$Imputed_data$Discharge_m_cubed_per_s[!Output$Imputed_data$imputed],col='black')
    # lines(ti,Output$Imputed_data$Discharge_m_cubed_per_s_at_minus_two_sigma_confidence,col='green')
    # lines(ti,Output$Imputed_data$Discharge_m_cubed_per_s_at_plus_two_sigma_confidence,col='green')

    # Add new data to existing imputed_Height_file
    if (file.exists(imputed_Discharge_file)) {
      iDo <- readRDS(imputed_Discharge_file)
      inew <- !is.element(iDo$Imputed_data$time,ti)
      Output$Imputed_data <- rbind(Output$Imputed_data,iDo$Imputed_data[inew,])
      Output$Imputed_data <- Output$Imputed_data[order(Output$Imputed_data$time),]
      Output$Imputed_data <- Output$Imputed_data[!duplicated(Output$Imputed_data [,c('time')]),]
    }
    saveRDS(Output,imputed_Discharge_file)

  }

  ### Impute velocity data on desired intervals (e.g, monthly)  ----
  if (days_since_last_U_imputation>30) {

    # duplicate times in ADCP is causing issues in code belw
    # ADCP time in 2004 is defualt time of HADCP and indicates time was not set correctly
    ig <- !duplicated(ADCP$time) & ADCP$time>"2005-01-01 00:00:00 AEST"
    ADCP <- ADCP[ig,]

    # unsorted time causes TideHarmonics::ftide to crease
    ADCP <- ADCP[order(ADCP$time),]


    t <- ADCP$time
    U <- ADCP$Velocity_x_m_per_s_1




    # profiling range relative to distance to bin 1
    r<- apply(data.frame(ADCP$Range_to_bed_of_acoustic_beams_m,ADCP$Range_to_water_surface_of_acoustic_beams_m), 1, FUN = min)/ADCP$Distance_to_Bin_1_mid_point_m
    U[r<1.5] <- NaN # only use U if profiling range is 1.5 times of the bin 1 distance

    # use non-tidal stage and lagged dHdt as surrogate for velocity
    Height_Imputed <- readRDS(imputed_Height_file)$Imputed_data
    # Height interpolated to ti
    Hi <- realTimeloads::linear_interpolation_with_time_limit(Height_Imputed$time,Height_Imputed$Height_m,ti,2)$x_interpolated
    # Non-tidal stage using USGS butterworth
    readings_per_hour <- round(60/as.double(difftime(ti[2],ti[1],'mins')))
    Hnt <- realTimeloads::butterworth_tidal_filter(ti,Hi)
    Hnt <- imputeTS::na_ma(Hnt, k = readings_per_hour*24,weighting = "simple") # fill gaps at start and end of record
    dHdt <- c(NA,diff(Hi))
    dHdt[1] <- dHdt[2]
    dHdt_lagged <-data.table::shift(dHdt, n = c(3,6,9), fill=NA, type="lag", give.names=TRUE)

    dHdt_lagged <- data.frame(dHdt_lagged)
    for (j in 1:length(dHdt_lagged)) {
      dHdt_lagged[,j] <- imputeTS::na_ma(dHdt_lagged[,j],weighting = 'simple')
    }

    Output <- realTimeloads::impute_data(time=t,x=U,ti=ti,Xreg=cbind(Hnt,dHdt_lagged),harmonic = TRUE,MC=MC,ptrain = 0.9)

    # add uncertainty column names from impute_data() if no uncertainty estimates, ensures rbind() works if user changes arguments to impute_data() w/ time
    uncertainty_cols <- c('x_at_minus_two_sigma_confidence','x_at_minus_one_sigma_confidence','x_at_median_confidence','x_at_plus_one_sigma_confidence','x_at_plus_two_sigma_confidence')
    if (sum(grepl('x_at',colnames(Output$Imputed_data)))==0) {
      df <- data.frame(matrix(NA,nrow = nrow(Output$Imputed_data),ncol=5))
      colnames(df) <-uncertainty_cols
      Output$Imputed_data <-  cbind(Output$Imputed_data,df)
    }

    colnames(Output$Imputed_data)[colnames(Output$Imputed_data)=='x'] <- 'Velocity_x_m_per_s'
    colnames(Output$Imputed_data)<-gsub('x_at','Velocity_x_m_per_s_at',colnames(Output$Imputed_data))


    # plot(ti,Output$Imputed_data$Velocity_x_m_per_s,type='l',xlim = c(ti[1],ti[1000]))
    # points(ti[Output$Imputed_data$imputed],Output$Imputed_data$Velocity_x_m_per_s[Output$Imputed_data$imputed],col='red')
    # points(ti[!Output$Imputed_data$imputed],Output$Imputed_data$Velocity_x_m_per_s[!Output$Imputed_data$imputed],col='black')
    # lines(ti,Output$Imputed_data$Velocity_x_m_per_s_at_minus_two_sigma_confidence,col='green')
    # lines(ti,Output$Imputed_data$Velocity_x_m_per_s_at_plus_two_sigma_confidence,col='green')

    # Add new data to existing imputed_Height_file
    if (file.exists(imputed_Velocity_file)) {
      iUo <- readRDS(imputed_Velocity_file)
      inew <- !is.element(iUo$Imputed_data$time,ti)
      Output$Imputed_data <- rbind(Output$Imputed_data,iUo$Imputed_data[inew,])
      Output$Imputed_data <- Output$Imputed_data[order(Output$Imputed_data$time),]
      Output$Imputed_data <- Output$Imputed_data[!duplicated(Output$Imputed_data [,c('time')]),]
    }
    saveRDS(Output,imputed_Velocity_file)

  }

  ### Impute sonde conductivity data on desired intervals (e.g, monthly)  ----
  if (days_since_last_S_imputation>30) {

    t <- Sonde$time
    # "Conductivity_uS_per_cm" on Eagle IO is actually Specific Conductance us/cm @ 25 deg C
    SpC <- Sonde$Conductivity_uS_per_cm

    SpC[SpC<0] <- NA


    # use non-tidal stage and lagged dHdt as surrogate for velocity
    Height_Imputed <- readRDS(imputed_Height_file)$Imputed_data
    # Height interpolated to ti
    Hi <- realTimeloads::linear_interpolation_with_time_limit(Height_Imputed$time,Height_Imputed$Height_m,ti,2)$x_interpolated
    # Non-tidal stage using USGS butterworth
    readings_per_hour <- round(60/as.double(difftime(ti[2],ti[1],'mins')))
    Hnt <- realTimeloads::butterworth_tidal_filter(ti,Hi)
    Hnt <- imputeTS::na_ma(Hnt, k = readings_per_hour*24,weighting = "simple") # fill gaps at start and end of record
    dHdt <- c(NA,diff(Hi))
    dHdt[1] <- dHdt[2]
    dHdt_lagged <-data.table::shift(dHdt, n = c(3,6,9), fill=NA, type="lag", give.names=TRUE)

    dHdt_lagged <- data.frame(dHdt_lagged)
    for (j in 1:length(dHdt_lagged)) {
      dHdt_lagged[,j] <- imputeTS::na_ma(dHdt_lagged[,j],weighting = 'simple')
    }

    if (length(unique(Sonde$Site_number))==1) {
    # took log10(SpC) given possible range from 0  to 50,000
    Output <- realTimeloads::impute_data(time=t,x=log10(SpC),ti=ti,Xreg=cbind(Hnt,dHdt_lagged),harmonic = TRUE,MC=MC,ptrain = 0.9)

    # add uncertainty column names from impute_data() if no uncertainty estimates, ensures rbind() works if user changes arguments to impute_data() w/ time
    uncertainty_cols <- c('x_at_minus_two_sigma_confidence','x_at_minus_one_sigma_confidence','x_at_median_confidence','x_at_plus_one_sigma_confidence','x_at_plus_two_sigma_confidence')
    if (sum(grepl('x_at',colnames(Output$Imputed_data)))==0) {
      df <- data.frame(matrix(NA,nrow = nrow(Output$Imputed_data),ncol=5))
      colnames(df) <-uncertainty_cols
      Output$Imputed_data <-  cbind(Output$Imputed_data,df)
    }

    # "Conductivity_uS_per_cm" on Eagle IO is actually Specific Conductance us/cm @ 25 deg C
    colnames(Output$Imputed_data)[colnames(Output$Imputed_data)=='x'] <- 'Specific_conductance_uS_per_cm_at_25_deg_C'
    colnames(Output$Imputed_data)<-gsub('x_at','Specific_conductance_uS_per_cm_at_25_deg_C_at',colnames(Output$Imputed_data))
    Output$Imputed_data$Site_number <- unique(Sonde$Site_number)
    }

    if (length(unique(Sonde$Site_number))==2) {
      ind <- grepl(Sonde$Site_number[1],Sonde$Site_number)
      ind2 <- ti>=min(Sonde$time[!ind])
      Output_1 <- realTimeloads::impute_data(time=t[ind],x=log10(SpC[ind]),ti=ti,Xreg=cbind(Hnt,dHdt_lagged),harmonic = TRUE,MC=MC,ptrain = 0.9)
      Output_2 <- realTimeloads::impute_data(time=t[!ind],x=log10(SpC[!ind]),ti=ti[ind2],Xreg=cbind(Hnt[ind2],dHdt_lagged[ind2,]),harmonic = TRUE,MC=MC,ptrain = 0.9)
      # add uncertainty column names from impute_data() if no uncertainty estimates, ensures rbind() works if user changes arguments to impute_data() w/ time
      uncertainty_cols <- c('x_at_minus_two_sigma_confidence','x_at_minus_one_sigma_confidence','x_at_median_confidence','x_at_plus_one_sigma_confidence','x_at_plus_two_sigma_confidence')
      if (sum(grepl('x_at',colnames(Output_1$Imputed_data)))==0) {
        df <- data.frame(matrix(NA,nrow = nrow(Output_1$Imputed_data),ncol=5))
        colnames(df) <-uncertainty_cols
        Output_1$Imputed_data <-  cbind(Output_1$Imputed_data,df)
      }
      if (sum(grepl('x_at',colnames(Output_2$Imputed_data)))==0) {
        df <- data.frame(matrix(NA,nrow = nrow(Output_2$Imputed_data),ncol=5))
        colnames(df) <-uncertainty_cols
        Output_2$Imputed_data <-  cbind(Output_2$Imputed_data,df)
      }

      Output<- list()
      df1<- data.frame(Output_1$Imputed_data)
      df1$Site_number <- unique(Sonde$Site_number[ind])
      df2<- data.frame(Output_2$Imputed_data)
      df2$Site_number <- unique(Sonde$Site_number[!ind])
      Output$Imputed_data <- rbind(df1,df2)
      Output$Validation_statistics_1 <- Output_1$Validation_statistics
      Output$Validation_statistics_1$Site_number <- unique(Sonde$Site_number[ind])
      Output$Validation_statistics_2 <- Output_2$Validation_statistics
      Output$Validation_statistics_2$Site_number <- unique(Sonde$Site_number[!ind])
      Output$predictors <- Output_1$predictors
      Output$imputation_code <- Output_1$imputation_code

      # "Conductivity_uS_per_cm" on Eagle IO is actually Specific Conductance us/cm @ 25 deg C
      colnames(Output$Imputed_data)[colnames(Output$Imputed_data)=='x'] <- 'Specific_conductance_uS_per_cm_at_25_deg_C'
      colnames(Output$Imputed_data)<-gsub('x_at','Specific_conductance_uS_per_cm_at_25_deg_C_at',colnames(Output$Imputed_data))
      }


    # transform back to linear units
    Output$Imputed_data$Specific_conductance_uS_per_cm_at_25_deg_C <- 10^Output$Imputed_data$Specific_conductance_uS_per_cm_at_25_deg_C
    Output$Imputed_data[,grepl('C_at_',colnames(Output$Imputed_data))] <- 10^Output$Imputed_data[,grepl('C_at_',colnames(Output$Imputed_data))]


    #plot(ti,Output$Imputed_data$Specific_conductance_uS_per_cm_at_25_deg_C,type='l',xlim = c(ti[1],ti[100]),ylim = c(0,1500))
    #points(ti[Output$Imputed_data$imputed],Output$Imputed_data$Specific_conductance_uS_per_cm_at_25_deg_C[Output$Imputed_data$imputed],col='red')
    #points(ti[!Output$Imputed_data$imputed],Output$Imputed_data$Specific_conductance_uS_per_cm_at_25_deg_C[!Output$Imputed_data$imputed],col='black')
    #lines(ti,Output$Imputed_data$Specific_conductance_uS_per_cm_at_25_deg_C_at_minus_two_sigma_confidence,col='green')
    #lines(ti,Output$Imputed_data$Specific_conductance_uS_per_cm_at_25_deg_C_at_plus_two_sigma_confidence,col='green')

    # Add new data to existing imputed_Height_file
    if (file.exists(imputed_Sonde_file)) {
      iSo <- readRDS(imputed_Sonde_file)
      zlist <- list()
      z<-rbind(iSo$Imputed_data,Output$Imputed_data)  # combine
      zsites <- unique(z$Site_number)
      for (j in 1:length(zsites)) {
        zlist[[j]] <- z[grepl(zsites[j],z$Site_number),]
        zlist[[j]] <- zlist[[j]][!duplicated( zlist[[j]][,c('time')]),] # remove duplicate time at site
        zlist[[j]] <- zlist[[j]][order(zlist[[j]]$time),]  # order time at site
      }
      Output$Imputed_data <- do.call(rbind,zlist)
    }

    saveRDS(Output,imputed_Sonde_file)

  }

  ### Return value(s)  ----
  msg <- 'Imputation.R complete'
  return(msg)
}
