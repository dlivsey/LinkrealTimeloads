#' Infills missing TSS data using forecast::impute_data()
#'
#' Imputes TSS using adaptation of realTimeloads::impute_data_with_arima(). In https://aha.net.au/wp-content/uploads/AHA-Journal-Spring-2022_HR.pdf I show TSS imputed using the Froude number, tidally filtered discharge , and a nonlinear auto regressive exogenous model (NARX) model. A NARX model is similar to the Bidirectional RNN in keras see https://keras.io/guides/working_with_rnns/. For future work I suggest using a RNN in keras.
#'
#' @param user_data_folder file path to user data folder
#' @param site site folder under user_data_folder
#' @param ptrain proportion of data to use in training of decision trees in realTimeloads::impute_data()
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
#'
#'
#' @return Imputed data in /Analyte_data/TSS_Imputed.rds
#'
#' @seealso
#' \code{\link{Link_to_Real_time_loads}} Process data in specified folder structure using realTimeloads package
#'
#' @author Daniel Livsey (2023) ORCID: 0000-0002-2028-6128
#' @examples
#' # See use in Example()
#' @references
#' Stephen Wallace (2023, DES) provided crucial functions in Import_Channelmaster_Data() to extract data from binary 000 files.
#'
#' Livsey D.N. (2023). realTimeloads: Analyte Flux and Load from Estimates of Concentration and Discharge_. R package version 1.0.0.
#'
#'Hyndman R, Athanasopoulos G, Bergmeir C, Caceres G, Chhay L, O'Hara-Wild M, Petropoulos F, Razbash S, Wang E, Yasmeen F (2023). forecast: Forecasting functions for time series and linear models. R package version 8.21.1, https://pkg.robjhyndman.com/forecast/.
#'
#'Hyndman RJ, Khandakar Y (2008). “Automatic time series forecasting: the forecast package for R.” Journal of Statistical Software, 26(3), 1–22. doi:10.18637/jss.v027.i03.
#'
#' @export
#'
Impute_TSS <- function(user_data_folder,site = NULL,MC = 1,ptrain = 0.9) {


all_site_folders <- list.dirs(user_data_folder,recursive = "FALSE",full.names = TRUE)

# get specific site folder if site is provided
if (!is.null(site)) {
  all_site_folders <- all_site_folders[grepl(site,all_site_folders)]
}
number_of_sites <- length(all_site_folders)
Site_List <- readRDS(paste0(user_data_folder,'/Site_List.rds'))
#k<-1

for (k in 1:number_of_sites) {
  Qfile <- paste0(all_site_folders[k],'/Discharge_data/Discharge_Imputed.rds')

  # at this time require Discharge_Imputed.rds
  #if (!file.exists(Qfile)) { # No imputed discharge data, check for discharge data file
  #Qfile <- paste0(all_site_folders[k],'/Discharge_data/Discharge.rds')
  #}

  TSSfile <- paste0(all_site_folders[k],'/Analyte_data/TSS_estimated_from_SCI_with_metadata.rds')

  #ABSfile <- paste0(all_site_folders[k],'/ADCP_data/Processed_Backscatter_data.rds')

  if (file.exists(TSSfile)) {
    Analyte <- readRDS(TSSfile)
    Discharge <- readRDS(Qfile)$Imputed_data

    # time
    time <- Analyte$TSS_in_mg_per_liter_estimated_from_SCI$time
    ti <- seq(min(time),max(time),by = "10 min")

    # TSS
    TSS <- Analyte$TSS_in_mg_per_liter_estimated_from_SCI$TSS_mg_per_liter_median_confidence

    # Tidally filtered flow
     Qbar <- realTimeloads::butterworth_tidal_filter(Discharge$time,Discharge$Discharge_m_cubed_per_s)
     Qbar <- realTimeloads::linear_interpolation_with_time_limit(Discharge$time,Qbar,ti,threshold = Inf)$x_interpolated

    ### Froude number ----
    ADCPfile <- paste0(all_site_folders[k],'/ADCP_data/All_ADCP_Programming_Data.rds')
    Hfile <- paste0(all_site_folders[k],'/Sonde_and_Height_data/Height_Imputed.rds')
    Ufile <- paste0(all_site_folders[k],'/Discharge_data/Velocity_Imputed.rds')
    ADCP <- readRDS(ADCPfile)
    Height <- readRDS(Hfile)
    Velocity <- readRDS(Ufile)
    datum <- realTimeloads::linear_interpolation_with_time_limit(ADCP$time,ADCP$Thalweg_relative_to_gauge_datum_m,Height$Imputed_data$time,threshold = Inf)$x_interpolated
    H <- Height$Imputed_data$Height_m-datum
    U <- Velocity$Imputed_data$Velocity_x_m_per_s
    Fr <- U/sqrt(9.81*H) # dimensionless, note Fr>0 for most applications (i.e., will take abs(U) below), harmonic imputation keep Fr < 0
    # Impute Froude number using harmonic model onto ti
    Fri <- realTimeloads::impute_data(Height$Imputed_data$time,Fr,ti=ti,harmonic = TRUE)
    Fr <- abs(Fri$Imputed_data$x) # take abs(U), improved validation R^2 when useing abs(Fr)

    # considered phase of tide as predictor but decreased validation R^2
    #phase_of_tide <- factor(Fri$Imputed_data$x>0) # get phase of tide categorical predictor

    ### below I tried various predictors for imputation of TSS ----
    # run two iterations sampling 80% of the observed data, use mean predicted t.s. as predictor for final imputation

    # tried lags of Fr as predictors as well, works well using NARX in matlab
     readings_per_hour <- 1/as.double(difftime(ti[2],ti[1],units='hours'))
    # lagFr <- as.matrix(data.frame(data.table::shift(x=Fr, n = c(readings_per_hour,readings_per_hour*2,readings_per_hour*3), fill=NA, type="lag",give.names=TRUE)))
    # lagFr <- imputeTS::na_interpolation(lagFr)

    #tidalTSS <- realTimeloads::impute_data(ti,zTSS,ti=ti,harmonic=TRUE,MC=3,ptrain = 0.5)

    ### tidal amplitudes and prediction ----

    # y <- realTimeloads::linear_interpolation_with_time_limit(time,TSS,ti,1)$x_interpolated
    # readings_per_hour <- 1/as.double(difftime(ti[2],ti[1],units='hours'))
    #
    # igood <- is.finite(TSS)
    # dt <- as.double(difftime(ti[2],ti[1],units = 'hours'))
    # hfit <- TideHarmonics::ftide(TSS[igood],time[igood],TideHarmonics::hc7)
    # # sum of harmonic amplitudes and msl
    # xp <- predict(hfit,from = min(ti),to = max(ti),by = dt)
    # # individual harmonic amplitudes can be used if desired
    # xph <- data.frame(t(predict(hfit,from = min(ti),to = max(ti),by = dt,split=TRUE)))
    #
    # # non-tidal component
    # #xii <- 10^imputeTS::na_kalman(log10(y),maxgap = readings_per_hour*24*7,model ="auto.arima",xreg = xp,stepwise=FALSE,approximation=FALSE,num.cores=1,parallel=TRUE) # infill data to prevent filter ringing
    # xii <- imputeTS::na_interpolation(y,maxgap = readings_per_hour*24)
    # xnt <- realTimeloads::butterworth_tidal_filter(ti,xii)  # Non-tidal stage using USGS butterworth
    # xnt <- imputeTS::na_interpolation(xnt,maxgap = readings_per_hour*24*7)
    # xnt[!is.finite(xnt)] <- median(xnt,na.rm=TRUE)

    ### trying loess-predicted TSS ----
   #  df <- data.frame(yarima,Fr,Qbar,y)
   #  df <- df[1:5000,]
   #  ig <- which(is.finite(df$y))
   #  iters <- 10
   #  predictions <- matrix(nrow=iters,ncol=nrow(df))
   #  for (i in 1:nrow(predictions)) {
   #  # testing loess and smoothing spline
   #  dfi <- df
   #  # exclude some proportion of finite data
   #
   #  dfi$y[sample(ig,round(length(ig)*0.1))] <- NA
   #  zfit <- loess(log10(y) ~ log10(Fr):yarima, dfi,control = loess.control(surface = "direct"),span = 0.2)
   #  predictions[i,] <- predict(zfit,df)
   #  }
   #  quants <- c(0.0527, 0.1587, 0.5, 0.8414, 0.9473) # +/- 1 and 2 sigma and median (i.e., reported) estimate
   #  tse <-data.frame(t(apply(predictions, 2 , quantile , probs = quants , na.rm = TRUE )))
   # colnames(tse) = c('x_at_minus_two_sigma_confidence','x_at_minus_one_sigma_confidence','x_at_median_confidence','x_at_plus_one_sigma_confidence','x_at_plus_two_sigma_confidence')
   #
   #  plot(df$y,xlim = c(1,4000))
   #  points(10^tse$x_at_plus_two_sigma_confidence,col='green')
   #  points(10^tse$x_at_median_confidence,col='red')
   #  points(10^tse$x_at_minus_two_sigma_confidence,col='blue')

    ### ARIMA prediction  ----
     y <- realTimeloads::linear_interpolation_with_time_limit(time,TSS,ti,1)$x_interpolated
     xreg <- cbind(log10(Fr),Qbar)
     #ptrain <- 0.8
     nit <- -1
     predictions <- matrix(nrow=length(y),ncol = MC)
     validation_data <- list()
     ylog <- log10(y)
     # run MC to get validation residuals
     for (i in 1:MC) {
     #print(i)
     ival <- is.element(y,sample(y[is.finite(y)],round(sum(is.finite(y))*(1-ptrain))))
     ytrain <- ylog
     ytrain[ival] <- NA
     # train validation model
     mod <- forecast::auto.arima(ytrain,xreg = xreg,stepwise=FALSE,approximation=FALSE,num.cores=1,parallel=TRUE)$model
     # get validation data
     kal <- stats::KalmanSmooth(ytrain, mod, nit = -1)
     erg <- kal$smooth
     karima <- erg[,,drop = FALSE] %*% as.matrix(mod$Z)

     validation_data[[i]] <- data.frame(validation_predictions = 10^karima[ival],validation_data = y[ival])

    # get predictions for all data
     kal <- stats::KalmanSmooth(ylog, mod, nit = -1)
     erg <- kal$smooth
     karima <- erg[,,drop = FALSE] %*% as.matrix(mod$Z)

     predictions[,i] <- 10^karima

    # # rm() temporary variables
     rm(kal)
     rm(erg)
     rm(karima)
     }

     validation_data <- do.call(rbind,validation_data)
     #plot(validation_data$validation_predictions,validation_data$validation_data)

     res <- validation_data$validation_data-validation_data$validation_predictions
     iters <- 2000
     yp <- rowMeans(predictions)

     quants <- c(0.0527, 0.1587, 0.5, 0.8414, 0.9473) # +/- 1 and 2 sigma and median (i.e., reported) estimate
     tse <- data.frame(matrix(nrow = length(y),ncol=length(quants)))
     colnames(tse) = c('x_at_minus_two_sigma_confidence','x_at_minus_one_sigma_confidence','x_at_median_confidence','x_at_plus_one_sigma_confidence','x_at_plus_two_sigma_confidence')

     for (i in 1:length(yp)) {
       tse[i,] <- quantile(yp[i]+sample(res,iters,replace = TRUE),probs = quants , na.rm = TRUE)
     }


      # format outouts to match realTimeloads::impute_data

      # find data gaps exceeding one day
      zy <- imputeTS::na_interpolation(y,maxgap = readings_per_hour*24)
      tse[is.na(zy),] <- NA

      x = tse$x_at_median_confidence
      x[is.finite(y)] <- y[is.finite(y)]

      Imputed_data = data.frame(time = ti,x,imputed = !is.finite(y),data_gap_exceeds_one_day = is.na(zy),tse)

      colnames(validation_data) <- c("validation_data","validation_data_predictions")
      fit_summary <- summary(lm('validation_data~validation_data_predictions',validation_data))
      validation_r_squared <- fit_summary$adj.r.squared
      # root- squared error (units of validation_data)
      validation_rmse <- sqrt(mean(fit_summary$residuals^2))
      # Model standard percentage error (MSPE) (Rasmussen et al., 2009) in units of validation_data
      validation_mspe <- validation_rmse/mean(validation_data$validation_data)*100

      Validation_data_and_statistics <- list(validation_data=validation_data,validation_r_squared=validation_r_squared,validation_rmse=validation_rmse,validation_mspe=validation_mspe,proportion_of_data_held_out_for_validation = 1-ptrain,number_of_Monte_Carlo_simulations_for_uncertainty_estimation = MC)

      predictors <- c("Fr","Qbar")

      imputation_code <- "LinkrealTimeloads::Impute_TSS, Livsey (2023)"


      TSSi <- list(Imputed_data=Imputed_data,Validation_data_and_statistics=Validation_data_and_statistics,predictors=predictors,imputation_code=imputation_code)

      # Interactive plot
      #zts <- xts::xts(tse,order.by = ti)
      #p <- dygraphs::dygraph(zts, group = "timeseries")
      #p

    ### After trying various models decided to Fit TSS using ARIMA ----

    # this is the tidal model fit assuming Xreg <- cbind(xp,xnt)
      #TSSi <- realTimeloads::impute_data(time,TSS,ti=ti,harmonic=FALSE,Xreg = Xreg,MC= MC,ptrain = ptrain)


    fileRDS <- paste0(all_site_folders[k],'/Analyte_data/TSS_imputed.rds')




    saveRDS(TSSi,fileRDS)

    return(invisible(fileRDS))

  }

}


}
