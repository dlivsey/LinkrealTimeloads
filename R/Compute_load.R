#' Computes load using LinkrealTimeloads outputs
#'
#' Uses outputs from Link_to_Real_time_loads() to compute loads with or without uncertainty. See references in realTimeloads for uncertainty estimation method documentation
#'
#' @param user_data_folder file path to user data folder
#' @param site site folder under user_data_folder
#' @param from_time start time to compute total load
#' @param to_time end time to compute total load
#' @param compute_uncertainty logical to estimate uncertainty in load estimate
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
#' Link_to_Real_time_loads() must be run prior to this function
#'
#' @return List with discharge, TSS, and loads for reporting and plotting.
#'
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
Compute_load <- function(user_data_folder,site,from_time = NULL,to_time = NULL,compute_uncertainty=FALSE) {
all_site_folders <- list.dirs(user_data_folder,recursive = "FALSE",full.names = TRUE)

# get specific site folder if site is provided
if (!is.null(site)) {
  all_site_folders <- all_site_folders[grepl(site,all_site_folders)]
}
number_of_sites <- length(all_site_folders)
Site_List <- readRDS(paste0(user_data_folder,'/Site_List.rds'))

for (k in 1:number_of_sites) {
  Qfile <- paste0(all_site_folders[k],'/Discharge_data/Discharge_Imputed.rds')
  ABSfile <- paste0(all_site_folders[k],'/ADCP_data/Processed_Backscatter_data.rds')
  TSSfile <- paste0(all_site_folders[k],'/Analyte_data/TSS_estimated_from_SCI_with_metadata.rds')
  TSSImputedfile <- paste0(all_site_folders[k],'/Analyte_data/TSS_imputed.rds')

  Output <- NULL
  if (file.exists(Qfile)&file.exists(TSSImputedfile)) {
    TSS <- readRDS(TSSImputedfile)
    Discharge <- readRDS(Qfile)
    time <- TSS$Imputed_data$time

    if (is.null(from_time)) {
      from_time <- min(time,na.rm = TRUE)
    }
    if (is.null(to_time)) {
      to_time <- max(time,na.rm = TRUE)
    }

    Q <- realTimeloads::linear_interpolation_with_time_limit(Discharge$Imputed_data$time,Discharge$Imputed_data$Discharge_m_cubed_per_s,time,threshold = 1)
    # determine if data was imputed or not
    Qimputed <- approx(Discharge$Imputed_data$time,Discharge$Imputed_data$imputed,time,method = 'constant')$y
    Qimputed[is.na(Qimputed)] <- 1
    Qimputed[Q$ibad] <- 1
    Qimputed <- Qimputed==1 # convert to logical
    Q <- Q$x_interpolated

    # determine if TSS data was imputed or not (not needed since using time from TSS_Imputed file )
    # C_imputed <- realTimeloads::linear_interpolation_with_time_limit(TSS$Imputed_data$time,TSS$Imputed_data$x,time,threshold = 1)
    # Cimputed <- approx(TSS$Imputed_data$time,TSS$Imputed_data$imputed,time,method = 'constant')$y
    # Cimputed[is.na(Cimputed)] <- 1
    # Cimputed[C_imputed$ibad] <- 1
    # Cimputed <- Cimputed==1 # convert to logical

    C_imputed <- TSS$Imputed_data$x
    Cimputed <- TSS$Imputed_data$imputed

    # io <-  Discharge$Imputed_data$imputed
    # x <- c(Discharge$Imputed_data$time[500],Discharge$Imputed_data$time[600])
    # plot(Discharge$Imputed_data$time[io],Discharge$Imputed_data$Discharge_m_cubed_per_s[io],xlim = x)
    # lines(time[Qimputed],Q[Qimputed],col='red')
    # points(time[!Qimputed],Q[!Qimputed],col='green')
    # points(Discharge$Imputed_data$time[!io],Discharge$Imputed_data$Discharge_m_cubed_per_s[!io],col='blue')

    # compute dt (second) for load integration
    dt =  c()
    dt[2:length(time)] <- as.numeric(difftime(time[2:length(time)],time[1:length(time)-1],units = "secs"))
    dt[1] = median(dt,na.rm=TRUE) # assume time step 1 using median

    if (!compute_uncertainty) {
      C <- TSS$Imputed_data$x
      Qs <- C*Q*dt*1e-9




      ind <- time>= from_time & time <= to_time

      Co <- data.frame(matrix(nrow=length(time),ncol=5))
      Qs_kt <- data.frame(matrix(nrow=length(time),ncol=5))

      Total_load_kt <- data.frame(matrix(nrow=1,ncol=5))
      colnames(Total_load_kt) = c('minus_two_sigma_confidence','minus_one_sigma_confidence','median_confidence','plus_one_sigma_confidence','plus_two_sigma_confidence')
      colnames(Co) = colnames(Total_load_kt)
      colnames(Qs_kt) = colnames(Total_load_kt)

      Co$median_confidence <- C
      Qs_kt$median_confidence <- Qs
      Total_load_kt$median_confidence <- sum(Qs[ind],na.rm=TRUE)

      #Output <- list('time'=time,'Discharge_m3_per_sec'=Q,'TSS_mg_per_liter' = Co,'Load_kt' = Qs_kt,'Total_load_kt'=Total_load_kt,'folder'= all_site_folders[k],'Total_load_time_period' = c(from_time,to_time))

      Output <- list('time'=time,'Discharge_m3_per_sec'=Q,'Imputed_discharge'= Qimputed,'TSS_mg_per_liter' = Co,'Imputed_TSS' = Cimputed,'Load_kt' = Qs_kt,'Total_load_kt'=Total_load_kt,'folder'= all_site_folders[k],'Total_load_time_period' = c(from_time,to_time))

    }

    # Load with uncertainty
    if (compute_uncertainty) {
    # uncertainty on TSS from ADCP
    # TSS for load uncertainty, must recompute Ktp w/ Monte Carlo simulated data, at this time Monte Carlo simulated data is not stored b/c this will increase TSSfile file size by ~ 30 times
      Analyte <- readRDS(TSSfile)
      ABS <- readRDS(ABSfile)
      ABS$dB <- rowMeans(ABS[,c('mean_sediment_corrected_backscatter_beam_1_dB','mean_sediment_corrected_backscatter_beam_2_dB')],na.rm=TRUE)

    Ktp <- realTimeloads::estimate_timeseries(Analyte$Kt_predicted_from_SCI$surrogate,Analyte$Kt_predicted_from_SCI$regression_data)
    dB <- ABS$dB[is.element(ABS$time,Ktp$time)]
    Ci <- t(10^(0.1*dB - 0.1*t(Ktp$estimated_timeseries)))

    # uncertainty on imputed data
    # use validation residuals of imputed data to estimate uncertainty on imputed data below
    res <- TSS$Validation_data_and_statistics$validation_data$validation_data-TSS$Validation_data_and_statistics$validation_data$validation_data_predictions

    C <- matrix(nrow = nrow(Ci),ncol=length(time))
    Qsi =  matrix(NA, nrow = nrow(Ci), ncol = length(time))

    for (i in 1:nrow(Ci)) {
    #print(i)
    # uncertainty on data estimated by ADCP
      C[i,] <- realTimeloads::linear_interpolation_with_time_limit(ABS$time,Ci[i,],time,threshold = 1)$x_interpolated
      # uncertainty on imputed data
      C[i,is.na(C[i,])] <- C_imputed[is.na(C[i,])] + sample(res,sum(is.na(C[i,])),replace=TRUE)

      # Ensure uncertainty does not go below 0
      C[i,C[i,]<0] <- 0

      # compute load (i.e, load = C*Q*dt) from flux Qs (flux = C*Q)
      Qsi[i,] <- C[i,]*Q*dt*1e-9 # assumes concentration is mg/l and discharge is cubic meters per sec, dt is in seconds

    }

    # Compute uncertainty on TSS estimates
    quants <- c(0.0527, 0.1587, 0.5, 0.8414, 0.9473) # +/- 1 and 2 sigma and median (i.e., reported) estimate
    Co <- data.frame(t(apply(C , 2 , quantile , probs = quants , na.rm = TRUE )))
    colnames(Co) = c('minus_two_sigma_confidence','minus_one_sigma_confidence','median_confidence','plus_one_sigma_confidence','plus_two_sigma_confidence')

    # Compute load with uncertainty
    Qs_kt <- data.frame(t(apply(Qsi , 2 , quantile , probs = quants , na.rm = TRUE )))
    colnames(Qs_kt) = c('minus_two_sigma_confidence','minus_one_sigma_confidence','median_confidence','plus_one_sigma_confidence','plus_two_sigma_confidence')

    ind <- time>= from_time & time <= to_time

    Total_load_kt <- data.frame(t(quantile(rowSums(Qsi[,ind],na.rm=TRUE),probs = quants)))
    colnames(Total_load_kt) = c('minus_two_sigma_confidence','minus_one_sigma_confidence','median_confidence','plus_one_sigma_confidence','plus_two_sigma_confidence')

    Output <- list('time'=time,'Discharge_m3_per_sec'=Q,'Imputed_discharge'= Qimputed,'TSS_mg_per_liter' = Co,'Imputed_TSS' = Cimputed,'Load_kt' = Qs_kt,'Total_load_kt'=Total_load_kt,'folder'= all_site_folders[k],'Total_load_time_period' = c(from_time,to_time))

    }

  }



}
return(Output)

}


