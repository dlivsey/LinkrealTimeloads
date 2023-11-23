#' Writes annual QAQC Reports using QAQC_Report() in LinkrealTimeloads to user_data_folder/site
#'
#' Report(s) is(are) written in html format to user_data_folder/site folder. Report provides interactive plots to identify anomalies in timeseries and regression data. Plotting over one year of data in QAQC_Report causes excessive delay in interactive plots.
#'
#' @param author Your name (string)
#' @param user_data_folder file path to user data folder (string)
#' @param site site folder under user_data_folder (string)
#' @param site_name name of site for rendering purposes (string)
#' @param reporting_year year or years (e.g., 2022 will report Jun 2022 to Jun 2023) (double)
#' @param max_points maximum number of points per period in Kt(SCI) (integer)
#'
#' @return file path(s) to html. Also writes html report(s) to user_data_folder/site if provided by user
#' @note
#' Set reporting_year to NULL to plot all available data in one year increments
#' @seealso
#' \code{\link{Example}} Example of package steps and reporting
#'
#' @author Daniel Livsey (2023) ORCID: 0000-0002-2028-6128
#' @examples
#' # See use in Example()
#' @references
#' Stephen Wallace (2023, DES) provided crucial functions in Import_Channelmaster_Data() to extract data from binary 000 files.
#'
#' Livsey D.N. (2023). realTimeloads: Analyte Flux and Load from Estimates of Concentration and Discharge_. R package version 1.0.0.
#'
#' Livsey, D.N. (in review). National Industry Guidelines for hydrometric monitoring–Part 12: Application of acoustic Doppler velocity meters to measure suspended-sediment load. Bureau of Meteorology. Melbourne, Australia
#'
#'@export
QAQC_Annual_Reports <- function(author = NULL,user_data_folder = NULL,site = NULL,site_name = NULL,reporting_year = NULL,max_points = 100) {


  # debugging block
  #author <- 'Daniel Livsey'
  #path <- file.path(path.expand('~'))
  #dpath <- gsub('Documents','Desktop',path)
  #user_data_folder <- paste0(dpath,'/LinkreaTimeloads_Output')
  #site <- 'MRD'
  #site_name <- 'Mulgrave River at Deeral'
  #reporting_year <- NULL
  #smax_points <- 100

  if (!is.null(reporting_year)) {
  min_year <- min(reporting_year)
  max_year <- max(reporting_year)
  }

  if (is.null(reporting_year)) {
    min_year <- NULL
    max_year <- NULL
  }


  # Load time vector used to compute Loads
  time <- readRDS(paste0(user_data_folder,'/',site,'/Loads.rds'))$time

  if (is.null(min_year)) {
    min_year <- as.double(format(min(time,na.rm=TRUE),format="%Y"))
  }
  if (is.null(max_year)) {
    max_year <- as.double(format(max(time,na.rm=TRUE),format="%Y"))
  }

  all_reporting_years <- seq(min_year,max_year,1)
  paths <- c()

  for (i in 1:length(all_reporting_years)) {

    compute_from_time <- paste0(all_reporting_years[i],"-06-01 00:00:00 AEST")
    compute_to_time <- paste0(all_reporting_years[i]+1,"-06-30 23:59:59 AEST")
    from_time <- format(as.POSIXct(compute_from_time),'%b_%Y')
    to_time <- format(as.POSIXct(compute_to_time),'%b_%Y')

    if (all_reporting_years[i]==min_year & is.null(reporting_year)) {
      compute_from_time <- min(time,na.rm=TRUE)
      from_time <- format(compute_from_time,'%b_%Y')
      compute_from_time <- paste(as.character(compute_from_time),"AEST")
    }
    if (all_reporting_years[i]==max_year & is.null(reporting_year)) {
      compute_to_time <- max(time,na.rm=TRUE)
      to_time <- format(compute_to_time,'%b_%Y')
      compute_to_time <- paste(as.character(compute_to_time),"AEST")
    }

    output_file <- paste0(gsub(" ","_",site_name),'_QAQC_Report_',from_time,"_to_",to_time)


    paths[i] <- paste0(user_data_folder,'/',site,'/',output_file)

    QAQC_Report(author,user_data_folder,output_file,site,site_name,compute_from_time,compute_to_time,max_points)


  }

  return(paths)

}




