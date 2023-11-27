#' Predict the fraction of TSS/SSC that is less than 20 um by mass using water velocity, turbidity, and salinity
#'
#' Predict the fraction of TSS/SSC that is less than 20 um by mass using decision tree algorithms following Livsey et al (2022). Uncertainty on predictions is estimated using methods adopted from Rustomji and Wilkinson (2008). When multiplied by TSS, outputs can be used to estimate the total load that is less than or greater than 20 um
#'
#' @param Water_velocity_in_m_per_s cross-section stream velocity (m/s)
#' @param Total_depth_in_m stream depth (m)
#' @param Salinity_in_PSU Salinity (PSU)
#' @param Turbidity_in_FNU Turbidity (FNU)
#' @param Sample_depth_in_m Depth of water sample or monitoring point (m)
#' @param ptrain Fraction of particle size training data to use (-)
#' @param MC Number of Monte Carlo simulations (-)
#'
#' @note
#' Conductivity can be converted into Salinity in PSU using realTimeloads::ctd2sal()
#'
#' @returns estimates of the fraction of sediment less than 20 um by mass
#'
#' @examples
#' #  idealized gradient in turbidity offshore
#' n <- 10
#' U <- rep(1,n)
#' H <- rep(10,n)
#' PSU <- pracma::linspace(0,32,n)
#' FNU <- pracma::linspace(50,1,n)
#' SD <- 0.33*H
#' f20 = predict_f20(U,H,PSU,FNU,SD)
#' @references
#' Rustomji, P., & Wilkinson, S. N. (2008). Applying bootstrap resampling to quantify uncertainty in fluvial suspended sediment loads estimated using rating curves. Water resources research, 44(9).https://doi.org/10.1029/2007WR006088
#'
#' Livsey, D. N., Crosswell, J. R., Turner, R. D. R., Steven, A. D. L., & Grace, P. R. (2022). Flocculation of Riverine Sediment Draining to the Great Barrier Reef, Implications for Monitoring and Modeling of Sediment Dispersal Across Continental Shelves. Journal of Geophysical Research: Oceans, 127(7), https://doi.org/10.1029/2021JC017988
#'
#' @author Daniel Livsey (2023) ORCID: 0000-0002-2028-6128
#' @export
#'
predict_f20 <- function(Water_velocity_in_m_per_s,Total_depth_in_m,Salinity_in_PSU,Turbidity_in_FNU,Sample_depth_in_m = NULL,ptrain = 0.8,MC=1) {



  # Load particle size data from Livsey et al 2022
  Training_data <- readRDS(paste0(system.file("extdata", package = "LinkrealTimeloads"),'/Training_data_for_particle_size_prediction.rds'))

  # only select rows with finite training data
  # one sample has fraction 20 um = 0, this is likely an eroneous sample from error in LISST measurments.
  igood <- which(is.finite(Training_data$data$Shear_rate) & is.finite(Training_data$data$Salinity) & is.finite(Training_data$data$Turbidity) & Training_data$data$Percent_less_than_20_um_by_mass>0)

  input_data <- Training_data$data[igood,c('Shear_rate','Salinity','Turbidity','Percent_less_than_20_um_by_mass')]


  # compute G from user-provided current velocity using eq. 1 of Livsey et al 2022 at  https://doi.org/10.1029/2021JC017988.
  if (is.null(Sample_depth_in_m)) {
    Sample_depth_in_m <- 0.33*Total_depth_in_m # if SD is not given assume 0.33 of H for best estimate of TSSxs (see Livsey et al 2020, Estuaries and Coasts)
  }
  z <- Total_depth_in_m-Sample_depth_in_m # elevation of observation point above the bed in m

  Cd = 0.002; # drag coefficient
  k = 0.41 # Von Karman constant
  ustar <- sqrt(Cd*Water_velocity_in_m_per_s^2/2) # shear stress
  e <- ustar^3/k*z # dissipation rate
  v = 1e-6; # kinematic viscosity of water (m2/s) x 10-6
  G = sqrt(e/v)

  user_data <- data.frame(Shear_rate=G,Salinity = Salinity_in_PSU,Turbidity = Turbidity_in_FNU)

  # additive log-ratio transformation of Aitchison (1982) should be used on compositional data (i.e., data constrained from 0 to 1), see comments at: https://doi.org/10.1029/2022WR033982
  input_data$logratio <- log((input_data$Percent_less_than_20_um_by_mass/100)/(1-(input_data$Percent_less_than_20_um_by_mass/100)))

  n <- nrow(input_data)
  ind <- 1:n
  ntrain = round(n*ptrain)
  models <- list()
  prediction_in_log_ratio <- list()
  used_in_training <- list()
  residuals <- list()
  predicted_f20_in_lr <- matrix(NA,nrow = MC,ncol= nrow(user_data))


  for (i in 1:MC) {

    # split into train/test and validation data
    itrain <- is.element(ind,sample(ind,ntrain)) # training/testing data
    #ival <- !itrain # validation data

    # fit tree
    tree <- rpart::rpart(logratio ~ Shear_rate + Salinity + Turbidity, data = input_data[itrain,],control=rpart::rpart.control(cp=0.000001))

    # pruned tree based on cp
    pruned_tree <- rpart::prune(tree, cp=tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])

    # predict log-ratio for all input_data
    lr <- predict(pruned_tree,input_data)

    # predict log-ratio for user-input data
    lr_new <- predict(pruned_tree,user_data)

    # store outputs
    models[[i]] <- pruned_tree
    used_in_training[[i]] <- itrain
    prediction_in_log_ratio[[i]] <- lr
    residuals[[i]] <- input_data$logratio[!itrain] - lr[!itrain]
    predicted_f20_in_lr[i,] <- lr_new

    # Summary stats should be computed on log-ratio data

    # training stats
    #df <- data.frame(predicted = lr[itrain],observed = input_data$logratio[itrain])
    #tfit <- lm(predicted~observed,df)
    #summary(tfit)

    # validation stats
    #df <- data.frame(predicted = lr[!itrain],observed = input_data$logratio[!itrain])
    #vfit <- lm(predicted~observed,df)
    #summary(vfit)

  }

  res <-  unlist(residuals)
  iters <- 2000
  yp <- colMeans(predicted_f20_in_lr)

  quants <- c(0.0527, 0.1587, 0.5, 0.8414, 0.9473) # +/- 1 and 2 sigma and median (i.e., reported) estimate
  tse <- data.frame(matrix(nrow = nrow(user_data),ncol=length(quants)))
  colnames(tse) = c('x_at_minus_two_sigma_confidence','x_at_minus_one_sigma_confidence','x_at_median_confidence','x_at_plus_one_sigma_confidence','x_at_plus_two_sigma_confidence')

  for (i in 1:nrow(user_data)) {
    zzz <- yp[i]+sample(res,iters,replace = TRUE)
    tse[i,] <- quantile(zzz,probs = quants , na.rm = TRUE)
  }
  rm(zzz)


  # Convert back to fraction
  f20 = exp(tse[])/(exp(tse[])+1);

  return(f20)

}

