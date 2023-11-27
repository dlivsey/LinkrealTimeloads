#' Load in-situ particle size data in In_situ_data.csv from Livsey et al 2022
#'
#' Code used to import In_situ_data.csv from Livsey et al 2022. These data are used to train the algorithm for prediction of sediment loads delineated by particle size.
#'
#' @param filepath filepath to In_situ_data.csv downloaded from https://doi.org/10.5281/zenodo.6788303
#'
#' @returns list
#' @examples
#' # Download In_situ_data.csv from https://doi.org/10.5281/zenodo.6788303
#' # get file path to 'In_situ_data.csv' on local computer
#' # input file path to import_Livsey_et_al_2022_data()
#' # Output <- import_Livsey_et_al_2022_data('In_situ_data.csv')
#' @references
#' Daniel Livsey, Ryan Turner, Peter Grace, Joseph Crosswell, & Andy Steven. (2022). Field and laboratory measurements of suspended-sediment particle size and concentration from nine rivers draining to the Great Barrier Reef. https://doi.org/10.5281/zenodo.6788303
#'
#' @export
import_Livsey_et_al_2022_data <- function(filepath) {

  data <- readr::read_csv(filepath)
  variable_units <- data[1,]
  data <- data.frame(data)
  data[,3:ncol(data)] <- as.double(unlist(data[,3:50]))
  data$Date_time <- as.POSIXct(data$Date_time,format = "%d/%m/%Y %H:%M:%S")
  data <- data[2:nrow(data),]

  source = c('Particle size data in In_situ_data.csv from Livsey et al 2022. See LinkrealTimeloads/data-raw/import_Livsey_et_al_2022_data() for more details')

  Output <- list(data = data.frame(data),variable_units = data.frame(variable_units),source = source)


  # write data to Training_data_for_particle_size_prediction.rds in package folder

  #outpath <- file <- paste0(system.file("extdata", package = "LinkrealTimeloads"),'/Training_data_for_particle_size_prediction.rds')

  #saveRDS(Output,outpath)


  #outpath <- 'C:/Users/livseyd/OneDrive - Queensland University of Technology/Documents/Github/LinkrealTimeloads/LinkrealTimeloads/inst/extdata/Training_data_for_particle_size_prediction.rds'

  #saveRDS(Output,outpath)

  return(Output)

}




