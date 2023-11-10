na_kalman_DNL <- function (x, model = "StructTS", smooth = TRUE, nit = -1, maxgap = Inf,
          ...)
{
  data <- x
  if (!is.null(dim(data)[2]) && dim(data)[2] > 1) {
    for (i in 1:dim(data)[2]) {
      if (!anyNA(data[, i])) {
        next
      }
      tryCatch(data[, i] <- na_kalman(data[, i], model,
                                      smooth, nit, maxgap, ...), error = function(cond) {
                                        warning(paste("na_kalman: No imputation performed for column",
                                                      i, "of the input dataset.\n                Reason:",
                                                      cond[1]), call. = FALSE)
                                      })
    }
    return(data)
  }
  else {
    missindx <- is.na(data)
    if (!anyNA(data)) {
      return(x)
    }
    if (any(class(data) == "tbl")) {
      data <- as.vector(as.data.frame(data)[, 1])
    }
    if (sum(!missindx) < 3) {
      stop("At least 3 non-NA data points required in the time series to apply na_kalman.")
    }
    if (!is.null(dim(data)[2]) && !dim(data)[2] == 1) {
      stop("Wrong input type for parameter x.")
    }
    if (!is.null(dim(data)[2])) {
      data <- data[, 1]
    }
    if (!is.numeric(data)) {
      stop("Input x is not numeric.")
    }
    if (!is.logical(smooth)) {
      stop("Parameter smooth must be of type logical ( TRUE / FALSE).")
    }
    data[1:length(data)] <- as.numeric(data)
    if (is.character(model) && model == "StructTS" && length(unique(as.vector(data))) ==
        2) {
      return(na_interpolation(x))
    }
    if (model[1] == "auto.arima") {
      mod <- forecast::auto.arima(data, ...)$model
    }
    else if (model[1] == "StructTS") {
      if (is.na(data[1])) {
        data[1] <- data[which.min(is.na(data))]
      }
      mod <- stats::StructTS(data, ...)$model0
    }
    else {
      mod <- model
      if (length(mod) < 7) {
        stop("Parameter model has either to be \"StructTS\"/\"auto.arima\" or a user supplied model in\n            form of a list with at least components T, Z, h , V, a, P, Pn specified.")
      }
      if (is.null(mod$Z)) {
        stop("Something is wrong with the user supplied model. Either choose \"auto.arima\" or \"StructTS\"\n             or supply a state space model with at least components T, Z, h , V, a, P, Pn as specified\n             under Details on help page for KalmanLike.")
      }
    }
    if (smooth == TRUE) {
      kal <- stats::KalmanSmooth(data, mod, nit)
      erg <- kal$smooth
    }
    else {
      kal <- stats::KalmanRun(data, mod, nit)
      erg <- kal$states
    }
    if (dim(erg)[2] != length(mod$Z)) {
      stop("Error with number of components $Z.")
    }
    karima <- erg[missindx, , drop = FALSE] %*% as.matrix(mod$Z)
    #data[missindx] <- karima
    if (is.finite(maxgap) && maxgap >= 0) {
      rlencoding <- rle(is.na(x))
      rlencoding$values[rlencoding$lengths <= maxgap] <- FALSE
      en <- inverse.rle(rlencoding)
      data[en == TRUE] <- NA
    }
    if (!is.null(dim(x)[2])) {
      x[, 1] <- data
      return(x)
    }
    return(data)
  }
}
