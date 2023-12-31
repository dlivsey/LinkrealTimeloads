#' Writes rds file with Site_Folder, Site_Number, and Site_Name
#'
#' Site_Folder, Site_Number, and Site_Name must be manually updated as needed. Site_Folder, Site_Number, and Site_Name for 10 WQI ADCP sites are provided as default.
#'
#' @param user_data_folder file path to user data folder (string)
#' @param Site_Folder name of folder under user_data_folder (string)
#' @param Site_Number unique site number (string)
#' @param Site_Name name of site (string)
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
#' Folder structure can be initialized for new site using
#' \code{\link{initialize_new_site_directories}}
#'
#' @section Warning:
#' Site_Folder, Site_Number, and Site_Name must be manually updated when adding new sites
#'
#' Folder structure can be initialized for new site using
#' \code{\link{initialize_new_site_directories}}
#'
#' @return Output written to user_data_folder/Site_List.rds, links site folder to site number and name
#'
#' @seealso
#' \code{\link{Link_to_Real_time_loads}} Process data in specified folder structure using realTimeloads package
#' \code{\link{initialize_new_site_directories}} Initialize folder structure for new site
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
Site_list_update <- function(user_data_folder,Site_Folder = NULL,Site_Number = NULL,Site_Name=NULL) {
# Site list used in processing code
# if user adds new folder one must update list manually

ndir <- length(list.dirs(user_data_folder, recursive = FALSE))

# Current site folders, numbers, and names for ADCP WQI sites
Current_Site_Folders <- c("BCS","DRL","JRI","MRB","MRD","ORC","PCS","PRG","RRE","SCS")
Current_Site_Numbers <- c("1200125","1080025","1120053","1140041","1110056","1240062","1260122","122013A","1111019","1260117")
Current_Site_Names <-c("Bonnie Doon Creek at Strathalbyn","Daintree River at Lower Daintree","Johnstone River at Innisfail","Murray River at Bilyana","Mulgrave River at Deeral","O'Connell River at Caravan Park","Plane Creek at Sucrogen Weir","Proserpine River at Glen Isla","Russell River at East Russell","Sandy Creek South Branch at Downstream Sorbellos Road")

# default site list if no user data provided
if (is.null(Site_Folder)) {
Site_Folder <- Current_Site_Folders
Site_Number <- Current_Site_Numbers
Site_Name <- Current_Site_Names
Site_List = data.frame(Site_Folder,Site_Number,Site_Name)
}

# allow user to add sites if desired
if (!is.null(Site_Folder)) {
  Site_Folder <- c(Current_Site_Folders,Site_Folder)
  Site_Number <- c(Current_Site_Numbers,Site_Number)
  Site_Name <- c(Current_Site_Names,Site_Name)
  Site_List = data.frame(Site_Folder,Site_Number,Site_Name)
  Site_List <- unique(Site_List) # ensure no duplicate rows
}

file <- paste0(user_data_folder,"/Site_List.rds")
if (file.exists(file)) {
Site_List <- rbind(Site_List,readRDS(file))
}
Site_List <- unique(Site_List)

saveRDS(Site_List,file)

if (ndir<=nrow(Site_List)) msg <- 'user_data_folder/Site_List.rds updated'
if (ndir>nrow(Site_List)) stop('ERROR! USER MUST UPDATE Site_list_update.R With new Site numbers')

return(msg)

}
