#' Predict TSS from processed acoustic backscatter and turbidity using Livsey et al (2023)
#'
#' Code expects outputs from Link_to_Real_time_loads. Conducts regression of Kt(SCI,period) to predict TSS from acoustic backscatter using realTimeloads package code.
#'
#' @param user_data_folder file path to user data folder
#' @param site site folder under user_data_folder
#' @param overwrite logical to overwrite all previous TSS estimates
#' @param retain_data_for_load_uncertainty logical to keep estimates of Kt for later load uncertainty calculations
#' @note
#'
#' When retain_data_for_load_uncertainty = TRUE, exported rds file can be very large
#'
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
#' Link_to_Real_time_loads should be run first
#'
#' When retain_data_for_load_uncertainty = TRUE, exported rds file can be very large.
#' For load uncertainty calculations one must store 2000 Monte Carlo simulations per
#' time step (see realTimeloads documentation and references therein)
#'
#' @return TSS in mg/l from Kt(SCI). Output written to site/Analyte_data/TSS_estimated_from_SCI_with_metadata.rds
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
#' Livsey, D. N., Turner, R. D. R., & Grace, P. R. (2023). Combining Optical and Acoustic Backscatter Measurements for Monitoring of Fine Suspended‐Sediment Concentration Under Changes in Particle Size and Density. Water Resources Research, 59(8), e2022WR033982.
#'
#' Livsey, D.N. (in review). National Industry Guidelines for hydrometric monitoring–Part 12: Application of acoustic Doppler velocity meters to measure suspended-sediment load. Bureau of Meteorology. Melbourne, Australia
#'
#' @export
Predict_TSS_from_SCI <- function(user_data_folder,site = NULL,overwrite = FALSE,retain_data_for_load_uncertainty = FALSE) {
  #library(realTimeloads)

  #user_data_folder <- "C:/Users/livseyd/OneDrive - Queensland University of Technology/Documents/R/LoadDashboard/user_data"
  #site <- 'JRI'

  all_site_folders <- list.dirs(user_data_folder,recursive = "FALSE")

  # get specific site folder if site is provided
  if (!is.null(site)) {
    all_site_folders <- all_site_folders[grepl(site,all_site_folders)]
  }

  s <- strsplit(all_site_folders,'/')

  number_of_sites <- length(all_site_folders)

  for (k in 1:number_of_sites) {

  site <- s[[k]][length(s[[k]])]


  folders <- get_folders_a(user_data_folder,site)

  Periods <- readRDS(paste0(folders$ADCP,'/ADCP_Setting_Summary_Table.rds'))
  Analyte <- readRDS(paste0(folders$Analyte,'/Sediment_Samples.rds'))
  ABS <- readRDS(paste0(folders$ADCP,'/Processed_Backscatter_data.rds')) # Acoustic backscatter
  Sonde <- readRDS(paste0(folders$Sonde_and_Height,'/Sonde.rds')) # Acoustic backscatter

  if (site=='JRI') {
  # at this time we do not sample TSS at JRI, use JRC TSS and turbidity to estimate TSS at JRI
  ind <- Sonde$Site_number== "1120054" # JRC
  Sonde$Turbidity_FNU[Sonde$Turbidity_FNU<0] <- NA
  Turbidity_FNU <- realTimeloads::linear_interpolation_with_time_limit(Sonde$time[ind],Sonde$Turbidity_FNU[ind],Analyte$time,0.25)$x_interpolated
  time <- Analyte$time
  Site_number <- Analyte$Site_number
  SSCpt_mg_per_liter <- Analyte$SSCpt_mg_per_liter

  # estimate C from OBS and use estimated C(OBS) as Analyte data for JRI
  calibration <- data.frame(time,Site_number,Turbidity_FNU,SSCpt_mg_per_liter)
  fit_eq <- 'SSCpt_mg_per_liter~Turbidity_FNU'
  fit_eq <- 'log10(SSCpt_mg_per_liter)~log10(Turbidity_FNU)'
  boot <- realTimeloads::bootstrap_regression(calibration,fit_eq)
  Surrogate <- Sonde[!ind,names(Sonde)=='time'|names(Sonde)=='Turbidity_FNU']
  colnames(Surrogate) <- c('time','Turbidity_FNU')
  Cp <- realTimeloads::estimate_timeseries(Surrogate,boot)
  Surrogate$SSCpt_mg_per_liter <- Cp$estimated_timeseries_quantiles$median_confidence
  Surrogate$Site_number <- '1120053'
  Analyte <- Surrogate # use estimated C(OBS) as Analyte data for JRI
  Analyte <- Analyte[Analyte$SSCpt_mg_per_liter>0,]
  }

  # mean dB of both beams
  ABS$dB <- rowMeans(ABS[,c('mean_sediment_corrected_backscatter_beam_1_dB','mean_sediment_corrected_backscatter_beam_2_dB')],na.rm=TRUE)
  # mean as of both beams, at this time suggest not using attenuation_of_sound since I am skeptical if attenuation_of_sound is reasonable given low frequency (order < 1 MHz) and relatively low TSS (order < 1 g/l)
  #ABS$as <- rowMeans(ABS[,c('attenuation_of_sound_due_to_sediment_beam_1_dB_per_m','attenuation_of_sound_due_to_sediment_beam_2_dB_per_m')])
  #ABS$as[ABS$as<0] <- NA

  # for each period get calibration data
  calibration_data <- list()
  n <- c()
  # compute loads using TSS(dB,OBS)
  threshold_of_interpolation_hrs <- 0.25
  for (i in 1:nrow(Periods)) {
    ind <- Analyte$time>=Periods$Start_time[i] & Analyte$time<=Periods$End_time[i]
    ind2 <- ABS$time>=Periods$Start_time[i] & ABS$time<=Periods$End_time[i]
    ind3 <- Sonde$Site_number==unique(Analyte$Site_number)

    a <- Analyte[ind,]
    abs <- realTimeloads::linear_interpolation_with_time_limit(ABS$time,ABS$dB,a$time,threshold_of_interpolation_hrs)$x_interpolated
    obs <- realTimeloads::linear_interpolation_with_time_limit(Sonde$time[ind3],Sonde$Turbidity_FNU[ind3],a$time,threshold_of_interpolation_hrs)$x_interpolated
    obs[obs<0] <- NA

    if (sum( grepl('Turbidity_FNU',colnames(a)))) {
      obs <-  a$Turbidity_FNU
    }

    sci <- abs - 10*log10(obs)
    kt <- abs - 10*log10(a$SSCpt_mg_per_liter)
    period<-rep(i,length(kt),1)
    Period_Start_time <- Periods$Start_time[i]
    Period_End_time <- Periods$End_time[i]
    n[i] <- sum(is.finite(sci) & is.finite(kt))
    if (nrow(a)>=2) { # require 2 data points to store calibration data frame
      calibration_data[[i]] <- data.frame(a,abs,obs,sci,kt,period,Period_Start_time,Period_End_time)
      calibration_data[[i]]$period <- rep(factor(i),nrow(calibration_data[[i]]),1)
    }

  }


  # Get periods where calibration data is available
  cal <- do.call(rbind,calibration_data)
  if (sum( grepl('Turbidity_FNU',colnames(Analyte)))) {
    cal <- cal[colnames(cal)!='Turbidity_FNU'] # prevent two columns with redundant data
  }
  colnames(cal)[colnames(cal)=='obs'] = 'Turbidity_FNU'
  colnames(cal)[colnames(cal)=='abs'] = 'mean_sediment_corrected_backscatter_beams_1_and_2_dB'

  igSCI <- is.finite(cal$sci)
  #igOBS <- is.finite(cal$Turbidity_FNU)
  #igABS <- is.finite(cal$mean_sediment_corrected_backscatter_beams_1_and_2_dB)

  # move kt to last column for realTimeloads::bootstrap_regression()
  calibration <- cal[igSCI,names(cal)!='kt']
  calibration$kt <- cal$kt[igSCI]

  # use period as categorical variable in glm() to combine data sets from periods
  fit_eq <- 'kt~sci:period'
  # period is a factor variable, use glm()
  boot <- realTimeloads::bootstrap_regression(calibration,fit_eq,fit_glm = T)
  # plot(boot$fit) # insect regression diagnostic plots

  # inspect fit of Kt(SCI)
  # y <- calibration$kt
  # yp <- predict(boot$fit,calibration)
  # plot(yp,y,xlim = c(min(yp),max(yp)),ylim = c(min(y),max(y)))
  # lines(c(min(y),max(y)),c(min(y),max(y))) # 1:1 line
  # Compute stats on fit
  # fit <- lm('yp~y+0',data.frame(y,yp))
  # summary(fit)

  # compute concentration estimates
  t <- ABS$time[ABS$time>=min(calibration$Period_Start_time)]
  ti <- seq(as.POSIXct(paste0(format(min(calibration$Period_Start_time),format = '%Y-%m-%d %H'),':00'),tz = 'Australia/Brisbane'),max(calibration$Period_End_time),by = '10 min')
  # interpolate OBS measurements to dB if OBS measurements occur within 1 hour of ABS measurment
  ind <- Sonde$Site_number==unique(Analyte$Site_number)
  Sonde$Turbidity_FNU[Sonde$Turbidity_FNU<0] <- NA
  obs <- realTimeloads::linear_interpolation_with_time_limit(Sonde$time[ind],Sonde$Turbidity_FNU[ind],t,1)$x_interpolated
  sci <- ABS$dB[ABS$time>=min(calibration$Period_Start_time)]-10*log10(obs)
  period <- factor(rep(NA,length(t)),levels = levels(calibration$period))
  np<-length(unique(calibration$period))
  p <- unique(calibration$period)
  for (i in 1:np) {
    ind <- t>=calibration$Period_Start_time[calibration$period==p[i]][1] & t <= calibration$Period_End_time[calibration$period==p[i]][1]
    period[ind] <- p[i]
  }
  Surrogate <- data.frame(t,sci,period)
  Surrogate <- Surrogate[is.finite(sci)&is.finite(period),]
  colnames(Surrogate)[1] <- 'time'
  # kt with uncertainty
  Ktp <- realTimeloads::estimate_timeseries(Surrogate,boot)
  # TSS w/ uncertainty
  dB <- ABS$dB[is.element(ABS$time,Surrogate$time)]
  C <- t(10^(0.1*dB - 0.1*t(Ktp$estimated_timeseries)))

  quants <- c(0.0527, 0.1587, 0.5, 0.8414, 0.9473) # +/- 1 and 2 sigma and median (i.e., reported) estimate

  C <- data.frame(t(apply(C , 2 , quantile , probs = quants , na.rm = TRUE )))
  colnames(C) = c('minus_two_sigma_confidence','minus_one_sigma_confidence','median_confidence','plus_one_sigma_confidence','plus_two_sigma_confidence')
  colnames(C) <- paste0('TSS_mg_per_liter_',colnames(C))


  time <- Surrogate$time
  Site_number <- rep(calibration$Site_number[1],length(time))
  C <- cbind(time,Site_number,C)
  if (!retain_data_for_load_uncertainty) {
  Ktp <- Ktp[names(Ktp)!="estimated_timeseries"] # remove large matrix? One can use for load uncertainty calculations
  }

  #plot(C$TSS_mg_per_liter_median_confidence,xlim = c(17000,18000))
  #lines(C$TSS_mg_per_liter_minus_one_sigma_confidence,col='red')
  #lines(C$TSS_mg_per_liter_plus_one_sigma_confidence,col='red')

  # # inspect timeseries of predicted Kt
  # ind <- which(is.finite(Ktp$estimated_timeseries$median_confidence))[1:100]
  # plot(Ktp$estimated_timeseries$median_confidence,xlim = c(min(ind),max(ind)))
  # lines(Ktp$estimated_timeseries$minus_two_sigma_confidence,col='red')
  # lines(Ktp$estimated_timeseries$plus_two_sigma_confidence,col='red')
  #
  # # inspect timeseries of predicted C
  # ind <- which(is.finite(C$median_confidence))[5000:15000]
  # plot(t,C$median_confidence,xlim = c(as.POSIXct("2020-02-20 00:00:00 AEST"),as.POSIXct("2020-04-01 00:00:00 AEST")))
  # lines(t,C$minus_two_sigma_confidence,col='red')
  # lines(t,C$plus_two_sigma_confidence,col='red')
  # points(Analyte$time,Analyte$SSCpt_mg_per_liter,col='green',pch = c(10),cex = 3)


  fileRDS <-paste0(folders$Analyte,'/TSS_estimated_from_SCI_with_metadata.rds')

  # Check if any new data was imported when rds file is already present
  if (file.exists(fileRDS) & !overwrite) {
    Co <- readRDS(fileRDS)
    inew <- !is.element(Co$TSS_in_mg_per_liter_estimated_from_SCI$time,C$time)
    Output <- rbind(C,Co$TSS_in_mg_per_liter_estimated_from_SCI[inew,]) # combine
    Output <- Output[order(Output$time),] # order time
    Output <- Output[!duplicated(Output[,c('time')]),] # remove duplicate time
    C <- Output
    Output <- list('TSS_in_mg_per_liter_estimated_from_SCI' = C,'Kt_predicted_from_SCI' = Ktp)
    saveRDS(Output,fileRDS)
    message(paste('Saved to:',fileRDS))
  }

  # write file if does not exist or overwrite all data if overwrite = TRUE
  if (!file.exists(fileRDS) | overwrite) {
    Output <- list('TSS_in_mg_per_liter_estimated_from_SCI' = C,'Kt_predicted_from_SCI' = Ktp)
    saveRDS(Output,fileRDS)
    message(paste('Saved to:',fileRDS))
  }



  return('Predicted_TSS_from_SCI.R complete')

  }

}

get_folders_a <- function(user_data_folder,site) {
  ADCP <- paste0(paste0(user_data_folder,'/'),paste0(site,'/ADCP_data'))
  Analyte <- paste0(paste0(user_data_folder,'/'),paste0(site,'/Analyte_data'))
  Channel <- paste0(paste0(user_data_folder,'/'),paste0(site,'/Channel_Geometry'))
  Discharge <- paste0(paste0(user_data_folder,'/'),paste0(site,'/Discharge_data'))
  Offsets <-  paste0(paste0(user_data_folder,'/'),paste0(site,'/Height_Offsets'))
  Sonde_and_Height <-  paste0(paste0(user_data_folder,'/'),paste0(site,'/Sonde_and_Height_data'))
  site_folder <- paste0(paste0(user_data_folder,'/'),site)
  folders <- data.frame(site,user_data_folder,site_folder,ADCP,Analyte,Channel,Discharge,Offsets,Sonde_and_Height)
  return(folders)
}


