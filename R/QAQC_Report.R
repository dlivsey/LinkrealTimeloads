#' Writes QAQC Report using QAQC_Report.Rmd in LinkrealTimeloads/extdata to user_data_folder/site
#'
#' Report is written in html format to user_data_folder/site folder. Report provides interactive plots to identify anomalies in timeseries and regression data.
#'
#' @param author Your name (string)
#' @param user_data_folder file path to user data folder (string)
#' @param site site folder under user_data_folder (string)
#' @param site_name name of site for rendering purposes (string)
#' @param user_data_folder file path to user data folder (string)
#' @param compute_from_time time (PosixCt or Date)
#' @param compute_to_time time (PosixCt or Date)
#' @param max_points maximum number of points per period in Kt(SCI) (integer)
#'
#'
#' @return file path to html. Also writes html report to user_data_folder/site if provided by user
#'
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
QAQC_Report <- function(author = NULL,user_data_folder = NULL,site = NULL,site_name = NULL,compute_from_time = NULL,compute_to_time = NULL,max_points = 30) {


  # Assumes RMD file is in extdata of R package
  Rmdfile <- paste0(system.file("extdata", package = "LinkrealTimeloads"),'/QAQC_Report.Rmd')

  if (!is.null(user_data_folder) & !is.null(site)) {

    file <- paste0("QA_QC_report_",site, "_", format(Sys.Date(),'%Y_%B_%d'), ".html")

    rmarkdown::render(Rmdfile,
                      params = list(
                        author = author,
                        user_data_folder = user_data_folder,
                        site_folder = site,
                        site_name = site_name,
                        compute_from_time = compute_from_time,
                        compute_to_time = compute_to_time,
                        max_points = max_points
                      ),
                      output_dir = paste0(user_data_folder,'/',site),
                      output_file = file
    )
  }

  # Launch gui inputs if user_data_folder or site is not provided
  if (is.null(user_data_folder) | is.null(site)) {

    file <- rmarkdown::render(Rmdfile,
                      params = "ask",
                      output_dir = NULL,
                      output_file = NULL)

  }

  browseURL(file)

  return(invisible(file))


}
