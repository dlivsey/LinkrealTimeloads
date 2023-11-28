#' Tools to compute suspended-sediment load from DES WQI GBRCLMP ADCP data using the R package "realTimeloads"
#'
#' LinkrealTimeloads provides code to compute suspended-sediment load using RDI ADCP files, Eagle IO csv files, discharge, and cross-section surveys. Stephen Wallace provided crucial discussion and the functions in Import_Channelmaster_Data() that import data from RDI 000 files. Package code imports, processes, and computes suspended-sediment load using the CRAN package "realTimeloads". TSS is predicted from acoustic backscatter, turbidity, and time-period following Livsey et al (2023). Package code is designed to allow WQI staff to setup directories for other sites and compute suspended-sediment loads using ADCP.
#'
#'@section Functions:
#'
#'\code{\link{ADCP_Setting_Summary_Table}} Writes table with rows corresponding to time periods when ADCP instrument, setting, and elevation are constant
#'
#'\code{\link{Compute_load}} Compute sediment load using LinkrealTimeloads outputs
#'
#'\code{\link{Concatenate_Backscatter}} Concatenate acoustic backscatter output from Process_Backscatter
#'
#'\code{\link{Example}} Write package output using example data to user-specified file path
#'
#'\code{\link{Import_Channelmaster_Data}} Import RDI ADCP data from 000 files into R
#'
#'\code{\link{Import_discharge}} Import discharge from csv file
#'
#'\code{\link{Import_EAGLE_IO_csv_data}} Import sonde, height, and offset data from EAGLE IO csv exports
#'
#'\code{\link{Impute_height_discharge_velocity_and_conductivity}} Infill missing data in height, discharge, velocity, and conductivity records using realTimeloads::impute_data()
#'
#'\code{\link{Impute_TSS}} Impute estimated TSS using realTimeloads::impute_data()
#'
#' \code{\link{initialize_folders_and_move_data_files}} Used in Example(), writes folder to local directory and moves package example data to folders
#'
#' \code{\link{initialize_new_site_directories}} Writes folders expected by package code for a specified site and file path location
#'
#'\code{\link{Link_to_Real_time_loads}} Calls package functions in appropriate order to process data in one or more site folders under specified folder path
#'
#'\code{\link{predict_f20}} Predict the fraction of sediment < 20 um by mass per Livsey et al (2022)
#'
#'\code{\link{Predict_TSS_from_SCI}} Estimates TSS from processed acoustic backscatter data and turbidity per Livsey et al (2023)
#'
#'\code{\link{Process_Backscatter}} Process acoustic backscatter from ADCP for attenuation due to water and sediment using functions from realTimeloads
#'
#'\code{\link{Process_data_and_compute_load}} Process data, compute, and report loads for data in site folder
#'
#'\code{\link{QAQC_Report}} Write html report with interactive plots for QAQC of results
#'
#'\code{\link{QAQC_Annual_Reports}} Generates one or more annual QAQC_Reports for user-provided site
#'
#'\code{\link{Site__and_Channel_Information_Update}} Add ADCP bank location variables to xs.rds file from Stephen Wallace
#'
#'\code{\link{Site_list_update}} Writes or update site information to Site_List.rds
#'
#' @references
#' Livsey, D. N., Turner, R. D. R., & Grace, P. R. (2023). Combining Optical and Acoustic Backscatter Measurements for Monitoring of Fine Suspended‐Sediment Concentration Under Changes in Particle Size and Density. Water Resources Research, 59(8), e2022WR033982
#'
#' Livsey, D.N. (in review). National Industry Guidelines for hydrometric monitoring–Part 12: Application of acoustic Doppler velocity meters to measure suspended-sediment load. Bureau of Meteorology. Melbourne, Australia.
#'
#'@author Daniel Livsey (2023) ORCID: 0000-0002-2028-6128
#'
#'@section Acknowledgements:
#' Funding for this research was provided by an Advance Queensland Industry Research Fellowship
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom data.table data.table
#' @importFrom data.table setkey
#' @importFrom data.table shift
#' @importFrom dygraphs dyAxis
#' @importFrom dygraphs dyCrosshair
#' @importFrom dygraphs dygraph
#' @importFrom dygraphs dyHighlight
#' @importFrom dygraphs dyOptions
#' @importFrom dygraphs dyRangeSelector
#' @importFrom dygraphs dySeries
#' @importFrom forecast auto.arima
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 ggplot
#' @importFrom graphics abline
#' @importFrom graphics par
#' @importFrom graphics points
#' @importFrom graphics segments
#' @importFrom imputeTS na_interpolation
#' @importFrom imputeTS na_kalman
#' @importFrom magrittr %>%
#' @importFrom mice mice.impute.cart
#' @importFrom plotly ggplotly
#' @importFrom pracma linspace
#' @importFrom realTimeloads acoustic_backscatter_processing
#' @importFrom realTimeloads attenuation_of_sound_by_water
#' @importFrom realTimeloads bootstrap_regression
#' @importFrom realTimeloads butterworth_tidal_filter
#' @importFrom realTimeloads compute_load
#' @importFrom realTimeloads ctd2sal
#' @importFrom realTimeloads hADCPLoads
#' @importFrom realTimeloads impute_data
#' @importFrom realTimeloads linear_interpolation_with_time_limit
#' @importFrom realTimeloads near_field_correction
#' @importFrom realTimeloads speed_of_sound
#' @importFrom realTimeloads surrogate_to_analyte_interpolation
#' @importFrom rpart prune
#' @importFrom rpart rpart
#' @importFrom signal butter
#' @importFrom signal buttord
#' @importFrom signal filtfilt
#' @importFrom stats approx
#' @importFrom stats approxfun
#' @importFrom stats complete.cases
#' @importFrom stats filter
#' @importFrom stats glm
#' @importFrom stats KalmanSmooth
#' @importFrom stats lm
#' @importFrom stats median
#' @importFrom stats pf
#' @importFrom stats predict
#' @importFrom stats quantile
#' @importFrom stats time
#' @importFrom stats uniroot
#' @importFrom TideHarmonics ftide
#' @importFrom utils browseURL
#' @importFrom utils data
#' @importFrom utils globalVariables
#' @importFrom utils read.csv
#' @importFrom utils winDialog
#' @importFrom xts xts
## usethis namespace: end
NULL
