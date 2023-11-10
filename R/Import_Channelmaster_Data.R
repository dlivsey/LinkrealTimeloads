#' Imports data from binary RDI 000 files using code from Stephen Wallace and moves variables to format expected in the package realTimeloads
#'
#' Imports data from binary RDI 000 files when new files are added OR when 000 file size changes. Stephen Wallace provided all functions that extract data from 000 files. This function we needed to assign variable names expected in realTimeloads.
#'
#' @param user_data_folder file path to user data folder
#' @param site site folder under user_data_folder
#' @param overwrite overwrite all previously imported data
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
#' @return  A rds file for each 000 file and All_ADCP_Programming_Data.rds
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
#' @export
Import_Channelmaster_Data <- function(user_data_folder,site=NULL,overwrite = FALSE) {

# start time of code
st <- Sys.time()

#site <- "MRD"
#user_data_folder <- "N:/Event Monitoring/Data Management/Telemetry Data/ADCP/JRI/newmount"
#user_data_folder <- "C:/Users/livseyd/OneDrive - Queensland University of Technology/Documents/R/LoadDashboard/user_data"
#overwrite <- FALSE

folder <- user_data_folder
filepaths <- list.files(path = folder, pattern = "*\\.000", recursive = TRUE,full.names = TRUE,include.dirs = TRUE)

# used to assign site numbers below
Site <-readRDS(paste0(folder,'/Site_List.rds'))

# only consider one site folder if a specific "site" folder is provided
# all site folders should be placed in one folder, e.g., user_data/JRI/ADCP_data, user_data/MRD/ADCP_data
if (!is.null(site)) {
isite <- grepl(site,filepaths)
filepaths <- filepaths[isite]
#print(filepaths)
}


# Overwrite all previous rds files ?
if (overwrite) inew <- !logical(length(filepaths))

# Check if rds file are present and if any changes in 000 file size has occured
if (!overwrite) {
# check for new 000 files and if 000 file size has changed
inew <- c()
for (i in 1:length(filepaths)) {

  file <- gsub(".000",".rds",filepaths[[i]])
  if (!file.exists(file)) {
  inew[i] <- TRUE # no rds file process as new file
  }
  # if rds is present check file size, if change in 000 file size, process as new file
  if (file.exists(file)) {
    fso <- readRDS(file)$fizesize
    fsn <- file.info(filepaths[[i]])$size
    if (fso==fsn) { inew[i] <- FALSE} # rds file and no change in 000 size
    if (fso!=fsn) {inew[i] <- TRUE } # rds file present and change in 000 size
  }
}
}

Number_of_files_imported = sum(inew)

if (Number_of_files_imported>0) message(paste("Number of files to import = ",Number_of_files_imported))


filepaths <- filepaths[inew] # new files to be imported

### Import 000 files and write to RDS file for each 000 file ----
if (Number_of_files_imported>0) {
s <- strsplit(filepaths,'/')
n <- length(s)
columns = c("sitefolder","data","file","filepath","fizesize")
Output = data.frame(matrix(nrow = n, ncol = length(columns)))
colnames(Output) = columns

# Copied from colnames(realTimeloads::ExampleData$ADCP)
#colnames(realTimeloads::ExampleData$ADCP)
# added salinity, pitch, and roll as well, may be useful for QA/QC later on
ADCPcolumns <-  c("Site_number",	"time",	"Ensemble",	"Accoustic_Frequency_kHz",	"Transducer_radius_m",	"Beam_angle_degrees",	"Beam_aspect_ratio",	"Number_of_Cells",	"Bin_Size_m",	"Blanking_distance_m",	"Instrument_serial_number",	"CPU_serial_number",	"Ambient_Noise_Level_Beam_1_Counts",	"Ambient_Noise_Level_Beam_2_Counts",	"Distance_to_Bin_1_mid_point_m",	"Speed_of_sound_m_per_s",	"Temperature_degC",	"Pressure_dbar","Salinity_PSU","Distance_to_surface_m",	"Power_supply_voltage","Pitch_deg","Roll_deg","Echo_Intensity_Beam_1","Echo_Intensity_Beam_2","Velocity_x_m_per_s_1","Range_to_bed_of_acoustic_beams_m","Range_to_water_surface_of_acoustic_beams_m","Thalweg_relative_to_gauge_datum_m","ADCP_elevation_above_gauge_datum_m","file")

for (i in 1:length(filepaths)) {

  try(out<-import_file(i,filepaths,s,folder,ADCPcolumns,Site,Output),silent = TRUE) # try() on import_file, if import fails code will move to next file. Added to ensures code tries to import all possible files

  # store outputs in structure for debugging, commenting out to save run-time
  #if (exists("out")) {
  #  Output[i,] <- out
  #rm(out)
  #}
}



### Write or Update All_ADCP_Programming_Data.rds for each site in user_data_folder ----
if (exists("Output")) rm(Output)

folder <- user_data_folder
filepaths <- list.files(path = folder, pattern = "*\\.000", recursive = TRUE,full.names = TRUE,include.dirs = TRUE)
sitefolders <- list.dirs(folder,recursive = FALSE)

if (!is.null(site)) {
  isite <- grepl(site,filepaths)
  isitefolder <- grepl(site,sitefolders)
  filepaths <- filepaths[isite]
  sitefolders <- sitefolders[isitefolder]
  #print(filepaths)
}

output_folders <- unique(sitefolders)
# for each 000 file save one rds file
for (i in 1:length(output_folders)) {

  ind <- grepl(output_folders[i],filepaths)
  zfilepaths <- filepaths[ind]
  file <- paste0(output_folders[i],"/ADCP_data/All_ADCP_Programming_Data.rds")

  columns = c("sitefolder","data","file","filepath","fizesize")
  Output = data.frame(matrix(nrow = length(zfilepaths), ncol = length(columns)))
  colnames(Output) = columns
  for (j in 1:length(zfilepaths)) {
    #print(j)
    rdsfile <- gsub('.000','.rds',zfilepaths[j])
    if (file.exists(rdsfile)) {
    z000data <-readRDS(rdsfile)
    Output[j,] <- z000data
    # remove echo intensity to save storage
    if (!is.na(z000data$data)) {
    z000data$data[[1]]<-z000data$data[[1]][!grepl("Echo",names(z000data$data[[1]]))]
    # Ensure that addition of variable names in processing later does not cause rbind to fail below
    z000data$data[[1]] <- z000data$data[[1]][is.element(colnames(z000data$data[[1]]),ADCPcolumns)]
    z000data$data[[1]]$file <- z000data$file # decided to add file name for later loading
    Output[j,] <- z000data[,is.element(colnames(z000data),columns)]
    }
    }
  }
    # for each site append ADCP setting data frame
  # ADCP setting data frame will allow plotting of ADCP settings and Serial Numbers with time to ensure no change in TSS(dB) occurs owing to changing ADCP instrument or settings
  if (sum(!is.na(Output$data))>0) {
  ADCP <- do.call(rbind, Output$data[!is.na(Output$data)])

  # If no ADCP programming file exists write file
  if (!file.exists(file)) {
    saveRDS(ADCP, file)
    Output <- ADCP
  }

  # if file exists load and append new data
  if (file.exists(file)) {
    ADCPo <- readRDS(file)
    inew <- !is.element(ADCP$time,ADCPo$time)
    message(paste('Number of ensembles appended to All_ADCP_Programming_Data.rds',sum(inew)))
    # if new observations are found
    if (sum(inew)>0) {
      #print(sum(inew))
      # combine data
      ADCPn <- rbind(ADCPo,ADCP[inew,])
      # order by time and save
      saveRDS(ADCPn[order(ADCPn$time), ], file)
      Output <- ADCPn[order(ADCPn$time), ]
    }
  }
  }

  return(invisible(NULL))
}




### Print imported files and run time ----
msg <- paste("Number of files imported =",Number_of_files_imported)
message(paste('Total run time in minutes:',as.integer(difftime(Sys.time(),st,units='mins'))))
return(msg)

}



}
### Internal functions ----

import_file <- function(i,filepaths,s,folder,ADCPcolumns,Site,Output) {


  message(paste("Importing file",i))
  message(filepaths[[i]])


  zlist <- s[[i]]
  zlength <- length(zlist)
  Output$file[i] <- zlist[[zlength]]
  Output$sitefolder[i] <- zlist[[zlength-2]]
  Output$filepath[i] <- filepaths[[i]]
  fi<-file.info(filepaths[[i]])
  Output$fizesize[i] <- fi$size
  #print(paste('File size =',fi$size))
  # read in ADCP data from Stephen Wallace's code and move to format needed for realTimeloads package
  data <- decodeChannelmaster(filepaths[[i]], tadj = 0)

  # read in channel geometry file, note add  xs$ADCP_Location in Site__and_Channel_Information_Update.R
  path <- paste0(paste0(paste0(folder,"/"),Output$sitefolder[i]),"/Channel_Geometry")
  xsfile <- list.files(path = path, pattern = "*\\.rds", recursive = FALSE,full.names = TRUE,include.dirs = TRUE)
  if (length(xsfile)==0) {
    xsfile <- list.files(path = path, pattern = "*\\.RDS", recursive = FALSE,full.names = TRUE,include.dirs = TRUE)
  }
  xs <- readRDS(xsfile)
  # get thalweg, define as min of survey points
  for (j in 1:nrow(xs$Periods)) {
    ind <- is.element(xs$XS$n,xs$Periods$TableNum[j])
    #Cxs <- xs$XS$CHAIN[ind]
    zxs <- xs$XS$RL[ind]
    xs$Periods$thalweg[j] <- min(zxs)
  }

  # compute maximum distance to boundaries in "arl"
  nens <-length(data)
  message(paste('Number of ensembles =',nens))
  message(paste('Ratio of File size to Number of ensembles =',fi$size/nens))
  #print("If number of ensembles is low but file size large, indicates checksum failed for each ensemble in file")

  if (nens==0) {
    # Save RDS file even if there is no ensembles to prevent import attempts until change in 000 file size occurs
    Output$data[i] <- NA # no data to store,
    # "Checksum failed, searching for next ensemble" in decodeChannelmaster() resulting in no data even those the file size is > 0
    # Save RDS file for each 000 file
    file <- gsub(".000",".rds",Output$filepath[i])
    saveRDS(Output[i,], file)
    msg <- 'Saved rds file but no data imported'
  }

  if (nens>0) {
    for (j in 1:nens) {
      # print(j)
      AdcpChain <- approx(xs$ADCP_Location$PeriodStart,xs$ADCP_Location$ADCP_distance_from_right_bank_or_chainage_m,data[[j]]$VariableLeader$Timestamp,rule = 2,method = "constant")$y
      AdcpRL <- approx(xs$ADCP_Location$PeriodStart,xs$ADCP_Location$ADCP_elevation_above_gauge_datum_m,data[[j]]$VariableLeader$Timestamp,rule = 2,method = "constant")$y
      ib <- approx(xs$ADCP_Location$PeriodStart,1:nrow(xs$ADCP_Location),data[[j]]$VariableLeader$Timestamp,rule = 2,method = "constant")
      lb <- xs$ADCP_Location$ADCP_bank_location[ib$y] == "left"
      # commented-out thalweg<-optimize(...) below
      thl <- approx(xs$Periods$PeriodStart,xs$Periods$thalweg,data[[j]]$VariableLeader$Timestamp,rule = 2,method = "constant")$y

      arl <- processARL(data[[j]],xs,AdcpChain,AdcpRL,lb)
      # estimate ARL if ensemble time occurs before or after data in xs
      if (is.null(arl)) {
        # estimate ARL if ensemble time occurs before or after data in xs
        arl <- processARL2(xs,data[[j]],AdcpChain,AdcpRL,lb)
      }

      arl$thalweg <- thl
      arl$ADCP_elevation_above_gauge_datum_m <- AdcpRL

      data[[j]]$processARL <- arl
    }

    ADCP = data.frame(matrix(nrow = nens, ncol = length(ADCPcolumns)))
    colnames(ADCP)<-ADCPcolumns
    # assign non-double variable types
    ADCP$Site_number <- as.character(ADCP$Site_number)
    ADCP$Instrument_serial_number <- as.character(ADCP$Site_number)
    ADCP$CPU_serial_number <- as.character(ADCP$Site_number)
    ADCP$file <- as.character(ADCP$Site_number)
    ADCP$time = as.POSIXct("0000-01-01 AEST",tz = "Australia/Brisbane")
    ADCP$file <- gsub(".000","",Output$file[i])
    ADCP$Site_number <- Site$Site_Number[is.element(Site$Site_Folder,Output$sitefolder[i])]

    for (j in 1:nens) {
      # Ensemble number and time
      ADCP$Ensemble[j] <- data[[j]]$VariableLeader$EnsNum
      ADCP$time[j] <- data[[j]]$VariableLeader$Timestamp

      # Temperature and speed of sound
      ADCP$Temperature_degC[j] <- data[[j]]$VariableLeader$Temperature
      ADCP$Pressure_dbar[j] <- data[[j]]$VariableLeader$Pressure/1000 # decaPascal to deciBar
      ADCP$Salinity_PSU[j] <- data[[j]]$VariableLeader$Salinity
      ADCP$Speed_of_sound_m_per_s[j] <- data[[j]]$VariableLeader$SpeedSound

      # Depth, pitch, and roll
      ADCP$Distance_to_surface_m[j] <- data[[j]]$VariableLeader$TransDepth
      ADCP$Pitch_deg[j] <- data[[j]]$VariableLeader$Pitch
      ADCP$Roll_deg[j] <- data[[j]]$VariableLeader$Roll

      # elevation of ADCP above datum
      ADCP$ADCP_elevation_above_gauge_datum_m[j] <- data[[j]]$processARL$ADCP_elevation_above_gauge_datum_m


      # Range to bed and water surface for conical acoustics beams
      ADCP$Range_to_bed_of_acoustic_beams_m[j] <-data[[j]]$processARL$btmrange
      ADCP$Range_to_water_surface_of_acoustic_beams_m[j] <-data[[j]]$processARL$toprange
      ADCP$Thalweg_relative_to_gauge_datum_m[j] <-data[[j]]$processARL$thalweg

      # Mean downstream velocity
      ADCP$Velocity_x_m_per_s_1[j] <-data[[j]]$processARL$arlX

      # Power supply voltage
      ADCP$Power_supply_voltage[j] <- data[[j]]$VariableLeader$BattVolt

      # Cell settings
      ADCP$Number_of_Cells[j] <- data[[j]]$FixedLeader$numCells
      ADCP$Blanking_distance_m[j] <- data[[j]]$FixedLeader$blankingDist/100
      ADCP$Bin_Size_m[j] <- data[[j]]$FixedLeader$rangeCellLength/100
      ADCP$Distance_to_Bin_1_mid_point_m[j] <- data[[j]]$FixedLeader$bin1Dist/100

      # Instrument and CPU information
      ADCP$Instrument_serial_number[j] <- data[[j]]$FixedLeader$chmSN
      ADCP$CPU_serial_number[j] <- paste(as.character(data[[j]][["FixedLeader"]][["CPUBoardSN"]]), collapse = "")

      # Echo_Intensity data, store as list in dataframe and use unlist() when processing
      ADCP$Echo_Intensity_Beam_1[j] <- data[[j]]$Variables$Echo1[1]
      ADCP$Echo_Intensity_Beam_2[j] <- data[[j]]$Variables$Echo1[2]
      # Store ambient noise level if available
      if (!is.null(data[[j]]$Variables$AmbientNoise$RSSI)) {
        # beam 1, beam, 2, and then average
        ADCP$Ambient_Noise_Level_Beam_1_Counts[j] <- data[[j]]$Variables$AmbientNoise$RSSI[1]
        ADCP$Ambient_Noise_Level_Beam_2_Counts[j] <- data[[j]]$Variables$AmbientNoise$RSSI[2]
      }

      # Determine ADCP frequency and beam angle
      config <- paste(as.character(data[[j]]$FixedLeader$sysConfig))
      ifreq <- 14:16
      ibeamtheta <- 7:8

      if (paste(config[ifreq],collapse = '')=='010') {
        ADCP$Accoustic_Frequency_kHz[j] <- 307.2
        ADCP$Transducer_radius_m[j] <- 0.065 # computed from reported Rayleigh distance in FSA-031 Backscatter Estimation Using Broadband Acoustic Doppler Current Profilers - Updated.pdf
        ADCP$Beam_aspect_ratio[j] <- 20
      }

      if (paste(config[ifreq],collapse = '')=='011') {
        ADCP$Accoustic_Frequency_kHz[j] <- 614.4
        ADCP$Transducer_radius_m[j] <- 0.048 # computed from reported Rayleigh distance in FSA-031 Backscatter Estimation Using Broadband Acoustic Doppler Current Profilers - Updated.pdf
        ADCP$Beam_aspect_ratio[j] <- 24
      }

      if (paste(config[ifreq],collapse = '')=='100') {
        ADCP$Accoustic_Frequency_kHz[j] <- '1200, modify code using exact frequency read from WinADCP' #
        ADCP$Beam_aspect_ratio[j] <- 24
      }

      if (paste(config[ifreq],collapse = '')=='101') {
        ADCP$Accoustic_Frequency_kHz[j] <- '2400, modify code using exact frequency read from WinADCP' #
      }

      if (paste(config[ibeamtheta],collapse = '')=='00') {
        ADCP$Beam_angle_degrees <- 15
      }

      if (paste(config[ibeamtheta],collapse = '')=='01') {
        ADCP$Beam_angle_degrees <- 20
      }

      if (paste(config[ibeamtheta],collapse = '')=='10') {
        ADCP$Beam_angle_degrees <- 30
      }

      if (paste(config[ibeamtheta],collapse = '')=='11') {
        ADCP$Beam_angle_degrees <- 25
      }
    }

    # Store dataframe as a list.
    Output$data[i] <- list(ADCP)

    # Save RDS file for each 000 file
    file <- gsub(".000",".rds",Output$filepath[i])
    ADCPout <- Output[i,]
    ADCPout$data[[1]] <- subset(ADCPout$data[[1]],select = -c(file))
    saveRDS(ADCPout, file)
    msg <- 'Saved rds file'
  }
  out <- Output[i,]
  #out <- msg
  return(out)
}

### Functions from Stephen Wallace's channelmaster.R ----
qualCode <- function(good, total)
{
  if(good / total > 0.9) return (10)
  if(good / total > 0.8) return (20)
  if(good / total > 0.5) return (30)
  return (60)
}

# decode
decodeChannelmaster <- function(fname, tadj = 0)
{

  sizeOfFile <- file.info(fname)
  buffer <- readBin(fname, "int", size = 1, n = sizeOfFile$size, signed = FALSE)

  processHeader <- function(buffer, hdrOffset)
  {

    #processHeader(buffer, hdrOffset) # process header

    ensHeader <- list()
    # number of bytes in ensemble
    ensHeader[["numBytes"]] <- buffer[hdrOffset+3]+buffer[hdrOffset+4] * 16 ^ 2

    # number of data types
    ensHeader[["numDataTypes"]] <- buffer[hdrOffset+6]

    # Offest for data type "dataType"
    typeOffsets <- list()

    if (ensHeader[["numDataTypes"]] > 0)
    {
      typeSeq <- seq(1, buffer[hdrOffset+6]*2, by = 2)
      for(dataType in seq_along(typeSeq))
      {
        # sequence continues for n data types
        typeOffset <- hdrOffset + 6 + typeSeq[dataType]
        typeOffsets[[dataType]] <- buffer [ typeOffset ] + buffer [ typeOffset + 1 ] * 16 ^ 2
      }
    }
    ensHeader[["typeOffsets"]] <- typeOffsets

    class(ensHeader) <- "EnsembleHeader"
    return(ensHeader)

  }

  calcChecksum <- function(buffer, offset, size)
  {
    if(offset+size > length(buffer)) return(FALSE)

    # calcChecksum(buffer, hdrOffset, header$numBytes)
    #calcChecksum(buffer, hdrOffset, header$numBytes)
    offset <- hdrOffset
    size <- header$numBytes

    checksumLoc <- offset + size

    LSB <- checksumLoc + 1
    MSB <- checksumLoc + 2
    checksumCheck <- buffer[LSB] + buffer[MSB] * 16 ^ 2

    CheckSumTotal <- 0
    for( byte in buffer[(offset+1):(checksumLoc)] )
    {
      CheckSumTotal <- CheckSumTotal + byte
      if (CheckSumTotal > 65535) CheckSumTotal <- CheckSumTotal - 65536
    }

    #CheckSumTotal
    #checksumCheck

    if(CheckSumTotal == checksumCheck){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }

  processFixedLdr <- function(buffer, fixedLdrOffset)
  {
    LSB <- buffer[fixedLdrOffset + 1]
    MSB <- buffer[fixedLdrOffset + 2]
    word <- LSB+MSB * 16 ^ 2

    FIXEDLEADERID <- 0
    if (word == FIXEDLEADERID)
    {
      fixedLdr <- list()
      fixedLdr[["cpuFwVer"]] <- buffer[fixedLdrOffset+3]
      fixedLdr[["cpuFwRev"]] <- buffer[fixedLdrOffset+4]

      LSB <- buffer[fixedLdrOffset + 5]
      MSB <- buffer[fixedLdrOffset + 6]

      binaryBase <- decimal2Base(
        LSB+MSB * 16 ^ 2, 2
      )
      binaryBase <- c(  rep(0, 16 - length(binaryBase) ),
                        binaryBase   )
      fixedLdr[["sysConfig"]] <- binaryBase


      fixedLdr[["numBeams"]] <- buffer[fixedLdrOffset + 9]
      fixedLdr[["numCells"]] <- buffer[fixedLdrOffset + 10] # WN

      LSB <- buffer[fixedLdrOffset + 11]
      MSB <- buffer[fixedLdrOffset + 12]
      fixedLdr[["numPings"]] <- LSB+MSB * 16 ^ 2 # WP

      LSB <- buffer[fixedLdrOffset + 13]
      MSB <- buffer[fixedLdrOffset + 14]
      fixedLdr[["rangeCellLength"]] <- LSB+MSB * 16 ^ 2 # WS

      LSB <- buffer[fixedLdrOffset + 15]
      MSB <- buffer[fixedLdrOffset + 16]
      fixedLdr[["blankingDist"]] <- LSB+MSB * 16 ^ 2 # WF

      fixedLdr[["lowCorThresh"]] <- buffer[fixedLdrOffset + 18] # WC

      fixedLdr[["tppMin"]] <- buffer[fixedLdrOffset + 23] # WC
      fixedLdr[["tppSec"]] <- buffer[fixedLdrOffset + 24] # WC
      fixedLdr[["tppHund"]] <- buffer[fixedLdrOffset + 25] # WC

      binaryBase <- decimal2Base(
        buffer[fixedLdrOffset + 26], 2
      )
      binaryBase <- c(  rep(0, 8 - length(binaryBase) ),
                        binaryBase   )
      fixedLdr[["coordTransfm"]] <- binaryBase


      binaryBase <- decimal2Base(
        buffer[fixedLdrOffset + 31], 2
      )
      # pad with zeroes at beginning
      binaryBase <- c(  rep(0, 7 - length(binaryBase) ),
                        binaryBase   )

      fixedLdr[["sensorSrc"]] <- binaryBase

      fixedLdr[["sensorAvail"]] <- buffer[fixedLdrOffset + 32]

      fixedLdr[["bin1Dist"]] <- buffer[fixedLdrOffset + 33] +
        buffer[fixedLdrOffset + 34] * 16 ^ 2 -
        ( 0.5 * fixedLdr[["rangeCellLength"]] )

      # for some reason, the serial is stored as hex, which when converted to decimal, needs to be converted back to hex
      fixedLdr[["CPUBoardSN"]] <- c(
        as.integer ( paste ( as.hexmode ( buffer[fixedLdrOffset + 43] ))),# * 16 ^ 0, # WB
        as.integer ( paste ( as.hexmode ( buffer[fixedLdrOffset + 44] ))),# * 16 ^ 2,
        as.integer ( paste ( as.hexmode ( buffer[fixedLdrOffset + 45] ))),# * 16 ^ 4,
        as.integer ( paste ( as.hexmode ( buffer[fixedLdrOffset + 46] ))),# * 16 ^ 6,
        as.integer ( paste ( as.hexmode ( buffer[fixedLdrOffset + 47] ))),# * 16 ^ 8,
        as.integer ( paste ( as.hexmode ( buffer[fixedLdrOffset + 48] ))),# * 16 ^ 10,
        as.integer ( paste ( as.hexmode ( buffer[fixedLdrOffset + 49] ))),# * 16 ^ 12,
        as.integer ( paste ( as.hexmode ( buffer[fixedLdrOffset + 50] )))# * 16 ^ 14
      )

      fixedLdr[["transmitPulse"]] <- buffer[fixedLdrOffset + 35]
      fixedLdr[["transmitPulse"]] <- fixedLdr[["transmitPulse"]] + buffer[fixedLdrOffset + 36] * 16 ^ 2

      fixedLdr[["falseTarget"]] <- buffer[fixedLdrOffset + 39]

      fixedLdr[["sysBW"]] <- buffer[fixedLdrOffset + 51] # WB
      fixedLdr[["chmSN"]] <- buffer[fixedLdrOffset + 55] * 16 ^ 0 + # WB
        buffer[fixedLdrOffset + 56] * 16 ^ 2 +
        buffer[fixedLdrOffset + 57] * 16 ^ 4 +
        buffer[fixedLdrOffset + 58] * 16 ^ 6

      class(fixedLdr) <- "FixedLeader"
      return(fixedLdr)

    }else{
      message("issue with fixed leader")
      return (0)
    }

  }

  processVarLdr <- function(buffer, offset, tadj = 0)
  {

    # CHANGE EVERY ENSEMBLE
    LSB <- buffer[offset + 1]
    MSB <- buffer[offset + 2]
    word <- LSB+MSB*2^16

    VARLEADERID <- 128
    if (word == VARLEADERID)
    {
      variableLdr <- list()
      variableLdr[["EnsNum"]] <- buffer[offset+3] +
        buffer[offset+4] * 16 ^ 2


      yr <- sprintf("%02d", buffer[offset+5])
      mn <- sprintf("%02d", buffer[offset+6])
      dy <- sprintf("%02d", buffer[offset+7])
      hr <- sprintf("%02d", buffer[offset+8])
      mi <- sprintf("%02d", buffer[offset+9])
      se <- sprintf("%02d", buffer[offset+10])
      hu <- sprintf("%02d", buffer[offset+11])


      variableLdr[["Timestamp"]] <-    as.POSIXct(paste(yr,mn,dy,hr,mi,se,".",hu,sep=""),format = "%y%m%d%H%M%S")


      variableLdr[["Timestamp"]] <- variableLdr[["Timestamp"]] - tadj * 60

      variableLdr[["EnsNum"]] <- variableLdr[["EnsNum"]] + buffer[offset+12] * 16 ^ 4

      variableLdr[["SpeedSound"]] <- ( buffer[offset+15] +
                                         buffer[offset+16] * 16 ^ 2 )

      variableLdr[["TransDepth"]] <- ( buffer[offset+17] +
                                         buffer[offset+18] * 16 ^ 2 ) * 0.1

      temp <- buffer[offset+21] +
        buffer[offset+22] * 16 ^ 2
      if ( temp > 32767 ) temp <- temp - 65536
      variableLdr[["Pitch"]] <- temp

      temp <- buffer[offset+23] +
        buffer[offset+24] * 16 ^ 2
      if ( temp > 32767 ) temp <- temp - 65536
      variableLdr[["Roll"]] <- temp

      temp <- buffer[offset+25] +
        buffer[offset+26] * 16 ^ 2
      variableLdr[["Salinity"]] <- temp # ppt

      temp <- buffer[offset+27] +
        buffer[offset+28] * 16 ^ 2
      variableLdr[["Temperature"]] <- temp * 0.01

      variableLdr[["MPTSecs"]]  <- ( buffer[offset+29] * 60 +
                                       buffer[offset+30] +
                                       buffer[offset+31] ) / 100

      variableLdr[["BattVolt"]]  <- buffer[offset+35] / 100 +  buffer[offset+36] + 0.6

      temp <- buffer[offset+49] * 16 ^ 0  +
        buffer[offset+50] * 16 ^ 2 +
        buffer[offset+51] * 16 ^ 4 +
        buffer[offset+52] * 16 ^ 6
      if ( temp > 8 * 16 ^ 7 ) temp <- 0 - (16 ^ 8 - 1 - temp) ## skw
      variableLdr[["Pressure"]] <- temp

      class(variableLdr) <- "VariableLeader"
      return(variableLdr)

    }else{
      message("issue with variable leader")
      return (0)
    }

  }

  processTypes <- function(buffer, offset, header, fixedleader)
  {

    processVelocity <- function(buffer, offset, cells, beams)
    {
      beamList <- list()
      for (beam in 1:beams)
      {
        cellList <- list()
        for (cell in 1:cells)
        {
          LSB <- buffer[offset + 3 + ( 8 * cell - ( 10 - 2 * beam )  ) ]
          MSB <- buffer[offset + 4 + ( 8 * cell - ( 10 - 2 * beam )  ) ]

          temp <- LSB + MSB * 16 ^ 2
          if ( temp > 32767) temp <- temp - 65536
          if ( temp != -32768 ) temp <- temp * 0.001

          cellList[[cell]] <- temp
        }
        beamList[[beam]] <- do.call(rbind,cellList)
      }
      class(beamList) <- "Velocity"
      return(beamList)
    }

    processCorrelation <- function(buffer, offset, cells, beams)
    {
      beamList <- list()
      for (beam in 1:beams)
      {
        cellList <- list()
        for (cell in 1:cells)
        {
          correl <- buffer[offset + 3 + ( 4 * cell + beam ) - 5  ]
          cellList[[cell]] <- correl
        }
        beamList[[beam]] <- as.vector( do.call(rbind,cellList) )
      }
      class(beamList) <- "Correlation"
      return(beamList)
    }

    processSurfTrackStatus <- function(buffer, offset)
    {
      LSB <- buffer[offset + 1]
      MSB <- buffer[offset + 2]
      word <- LSB+MSB * 16 ^ 2

      surfTrackStatus <- list()
      temp <- buffer[offset+3] * 16 ^ 0 +
        buffer[offset+4] * 16 ^ 2 +
        buffer[offset+5] * 16 ^ 4 +
        buffer[offset+6] * 16 ^ 6
      surfTrackStatus[["DepthCorr"]] <- temp * 0.0001

      temp <- buffer[offset+7] * 16 ^ 0 +
        buffer[offset+8] * 16 ^ 2 +
        buffer[offset+9] * 16 ^ 4 +
        buffer[offset+10] * 16 ^ 6
      surfTrackStatus[["DepthUncorr"]] <- temp * 0.0001

      surfTrackStatus[["EvalAmp"]] <- buffer[offset+11]
      surfTrackStatus[["SurfAmp"]] <- buffer[offset+12]
      surfTrackStatus[["surfPerGood"]] <- buffer[offset+13]

      class(surfTrackStatus) <- "SurfTrackStatus"
      return(surfTrackStatus)

    }

    processHighResRSSI <- function(buffer, offset)
    {
      # not enabled
      LSB <- buffer[offset + 1]
      MSB <- buffer[offset + 2]
      word <- LSB+MSB * 16 ^ 2
    }

    processAmbient <- function(buffer, offset)
    {
      # not enabled
      LSB <- buffer[offset + 1]
      MSB <- buffer[offset + 2]
      word <- LSB+MSB * 16 ^ 2

      beams <- buffer[offset + 3]

      noiseRSSI <- list()
      for (beam in 1:beams)
      {
        #print (beam)
        # Ambient Noise
        LSB <- buffer[offset + 2 + beam * 2]
        MSB <- buffer[offset + 3 + beam * 2]
        noiseRSSI[[beam]] <- (LSB + MSB * 16 ^ 2) * 0.1
      }
      # Ambient Noise
      LSB <- buffer[offset + 2 * beams + 4 ]
      MSB <- buffer[offset + 2 * beams + 5 ]
      noiseRSSI[[beams+1]] <- (LSB + MSB * 16 ^ 2) * 0.1

      noiseCorrel <- list()
      for (beam in 1:beams)
      {
        #print (beam)
        # Ambient Noise Correlation
        noiseCorrel[[beam]] <-  buffer[offset + 2 * beams + 5 + beam]
      }
      # Ambient Noise
      noiseCorrel[[beams + 1]] <-  buffer[offset + 3 * beams + 6 ]

      noiseSNR <- list()
      for (beam in 1:beams)
      {
        #print (beam)
        # Ambient Noise Correlation
        noiseSNR[[beam]] <-  buffer[offset + 3 * beams + 6 + beam]
      }
      # Ambient Noise
      noiseSNR[[beams + 1]] <-  buffer[offset + 4 * beams + 7 ]

      ambientNoise <- list()
      ambientNoise[["RSSI"]] <- do.call(rbind,noiseRSSI)
      ambientNoise[["Correl"]] <- do.call(rbind,noiseCorrel)
      ambientNoise[["SNR"]] <- do.call(rbind,noiseSNR)

      class(ambientNoise) <- "AmbientNoise"

      return(ambientNoise)
    }

    # fixedleader <- fixedLdr
    offsets <- header$typeOffsets
    #offsets <- header$typeOffsets
    WORDS <- data.frame(VELOCITY = 256,
                        CORREL = 512,
                        ECHO1 = 768,
                        ECHO2 = 1024,
                        STATUS = 1280,
                        SURFTRACK = 16384,
                        SURFAMP = 16386,
                        HRDRSSI = 32769, # not enabled in HADCP
                        AMBIENT = 32772
    )

    dataTypes <- list()
    for (type in seq(3,length(offsets)))
    {
      varoffset <- offsets[[type]]+offset
      #offset <- offsets[[3]]
      LSB <- buffer[varoffset+1]
      MSB <- buffer[varoffset+2]
      word <- LSB + MSB * 16 ^ 2
      word

      if(word == WORDS$VELOCITY){
        dataTypes[["Velocity"]] <- processVelocity(buffer,
                                                   varoffset,
                                                   fixedleader$numCells,
                                                   fixedleader$numBeams)
      }
      if(word == WORDS$CORREL){
        dataTypes[["Correlation"]] <- processCorrelation(buffer,
                                                         varoffset,
                                                         fixedleader$numCells,
                                                         fixedleader$numBeams)
      }
      if(word == WORDS$ECHO1){
        dataTypes[["Echo1"]] <- processCorrelation(buffer,
                                                   varoffset,
                                                   fixedleader$numCells,
                                                   fixedleader$numBeams)
      }
      if(word == WORDS$ECHO2){
        dataTypes[["Echo2"]] <- processCorrelation(buffer,
                                                   varoffset,
                                                   fixedleader$numCells,
                                                   fixedleader$numBeams)
      }
      if(word == WORDS$STATUS){
        #dataTypes[["Status"]] <-
      }
      if(word == WORDS$SURFTRACK){
        dataTypes[["SurfStatus"]] <- processSurfTrackStatus(buffer,
                                                            varoffset)


      }
      if(word == WORDS$SURFAMP){
        #dataTypes[["SurfAmp"]] <- processSurfAmp(buffer, varoffset)
      }

      if(word == WORDS$HRDRSSI){
        #dataTypes[["HRDRssi"]] <- processS(buffer, varoffset)
      }

      if(word == WORDS$AMBIENT){
        dataTypes[["AmbientNoise"]] <- processAmbient(buffer, varoffset)
      }



    }

    class(dataTypes) <- "EnsembleVariables"
    return(dataTypes)

  }

  ensembleList <- list()
  ensCount <- 0
  i <- 1
  # loop through entire file
  while(i < length(buffer))
  {

    # When the buffer contains bytes 127 and 127
    if(buffer[i] == 127 && buffer[i+1] == 127)
    {

      # header offset.  Where i is 1,
      # make it zero so position 1 is hdrOffset+1.
      hdrOffset <- i-1
      # header is information about the ensemble,
      # including size, data types and their offsets

      header <- processHeader(buffer, hdrOffset) # process header
      if (length(header$typeOffsets) == 0)
      {
        i <- i + 1
      }else{

        if(calcChecksum(buffer, hdrOffset, header$numBytes)) # check checksum, then proceed
        {
          # increment ensembles
          ensCount <- ensCount+1
          # FIXED LEADER DATA
          fixedLdrOffset <- hdrOffset + header$typeOffsets[[1]]
          # Fixed leader contains fixed info about adcp
          # Namely the settings at the time, bin numbers, pings, cellsize etc.
          fixedLdr <- processFixedLdr(buffer, fixedLdrOffset)

          # VARIABLE LEADER DATA
          varLdrOffset <- hdrOffset + header$typeOffsets[[2]]
          # Variable leader changes with each ensemble,
          # i.e. ensemble count/pitch/roll/temp/time/battery
          varLdr <- processVarLdr(buffer, varLdrOffset, tadj = tadj)

          # PROCESS THE REST OF THE VARIABLES
          # variable types are everything actually measured in the water
          # by the HADCP
          variableTypes <- processTypes(buffer, hdrOffset, header, fixedLdr)

          # build output
          ensemble <- list()
          ensemble[["Header"]] <- header
          ensemble[["FixedLeader"]] <- fixedLdr
          ensemble[["VariableLeader"]] <- varLdr
          ensemble[["Variables"]] <- variableTypes

          ensembleList[[ensCount]] <- ensemble

          # ensemble processed
          # skip to the next ensemble
          i <- i + header$numBytes

        }else{
          message("Checksum failed, searching for next ensemble")
          i <- i + 1
        }
      }
    }else{
      # ensemble not detected
      # searching for next ensemble start
      # can be eroneous data in binary file, i.e. "CS", ">".
      i <- i + 1
    }
  }

  return(ensembleList)

}

# decode a folder
decodeADCPFolder <- function(folder, tadj = 0)
{
  files <- list.files(path = folder, pattern = "*\\.000", recursive = TRUE)
  files <- files[!grepl(".PD0", files)]

  chmList <- list()
  count <- 0
  for(file in files)
  {
    file <- paste(folder, file, sep = "" )
    chmList[[paste(file)]] <- decodeChannelmaster(file, tadj = tadj)
  }
  return(do.call(c, chmList))
  #print(length(chmList))
  #return(chmList)
}

# adcp info
ensembleStatus <- function(ensemble){
  ifreq <- 14:16
  ibeamtheta <- 7:8

  c(
    switch(bin2dec ( ensemble$FixedLeader$sysConfig[ifreq] ) - 1, # switch (value minus 1) as there is no result for "1"
           data.frame(Accoustic_Frequency_kHz = 307.2, # 0 1 0, 300khz
                      Transducer_radius_m = 0.065,
                      Beam_aspect_ratio = 20),
           data.frame(Accoustic_Frequency_kHz = 614.4, # 0 1 1, 600khz
                      Transducer_radius_m = 0.048,
                      Beam_aspect_ratio = 24),
           data.frame(Accoustic_Frequency_kHz = 1200, # 1 0 0, 1200khz
                      Transducer_radius_m = NULL,
                      Beam_aspect_ratio = NULL),
           data.frame(Accoustic_Frequency_kHz = 2400, # 1 0 1, 2400khz
                      Transducer_radius_m = NULL,
                      Beam_aspect_ratio = NULL),
    ),
    Beam_angle_degrees = switch(bin2dec ( ensemble$FixedLeader$sysConfig[ibeamtheta] ) + 1, #when answer is "0", index is 1
                                15,20,30,25)
  )
}

processARL <- function( ensemble, xs, AdcpChain, AdcpRL, lb, doPlot = FALSE, goodbins = NULL, echo = NULL)
{
  if (is.null(xs))    return(NULL)
  timestamp <- ensemble$VariableLeader$Timestamp

  xs$Periods$PeriodEnd <- c(xs$Periods$PeriodStart[-1], Sys.time())

  if ( timestamp > xs$Periods$PeriodEnd[nrow(xs$Periods)] |
       timestamp < xs$Periods$PeriodStart[1])
  {
    #message("bad dates, skipping ARL...")
    #print(timestamp)
    #print(xs$Periods)
    return(NULL)
  }

  # filter to dates between
  thisxs <- xs$Periods[timestamp > xs$Periods$PeriodStart & timestamp < xs$Periods$PeriodEnd, ]

  # which array position is thisxs in
  #which(xs$Periods$PeriodStart == thisxs$PeriodStart)
  #nrow(xs$Periods)

  nextxs <- xs$Periods[
    min( which(xs$Periods$PeriodStart == thisxs$PeriodStart) + 1,  nrow(xs$Periods)),]

  # weighting to apply this and next cross section
  tratio <- approx(x = c(thisxs$PeriodStart, thisxs$PeriodEnd),
                   # if it's not phased, only "thisxs" is applied
                   y = if(thisxs$Phased == 1){c(0,1)}else{c(0,0)},
                   timestamp)

  # select this table number
  XS <- split(xs$XS, xs$XS$n)[[ thisxs$TableNum ]]
  XS2 <- split(xs$XS, xs$XS$n)[[ nextxs$TableNum ]]

  # for aspect ratio
  ensStatus <- ensembleStatus(ensemble)

  arl <- ARLRange(xs = data.frame(chain = XS$CHAIN, rl = XS$RL ),
                  adcp.chain = AdcpChain,
                  adcp.rl = AdcpRL,
                  wl = ensemble$Variables$SurfStatus$DepthCorr + AdcpRL,
                  ARL = ensStatus$Beam_aspect_ratio,
                  #doPlot = TRUE, #  ensemble$Variables$SurfStatus$DepthCorr > 11
                  lb = lb,
                  #### allow importing validated height data instead
                  xs2 = data.frame(chain = XS2$CHAIN, rl = XS2$RL ), # optional
                  tratio = tratio$y, # without a ratio, it will default to 0
                  echo = echo,
                  doPlot = doPlot
  )


  # ARL is effectively the furthermost possible distance from the adcp
  arlBins <- ( arl$range - ensemble$FixedLeader$bin1Dist / 100 ) /
    ( ensemble$FixedLeader$rangeCellLength / 100 )

  #if (is.null(arlBins)) arlBins >- 0

  # so arlbins doesn't exceed the number of good bins
  if(!is.null(goodbins))  arlBins[arlBins > goodbins] <- goodbins

  zbins <- 1:max(1: as.integer(arlBins))
  if (length(ensemble$Variables$Velocity[[1]])<length(zbins)){
    zbins <- zbins[1:length(ensemble$Variables$Velocity[[1]])]
  }

  arlX <- mean( ensemble$Variables$Velocity[[1]][zbins,] )
  arlY <- mean( ensemble$Variables$Velocity[[2]][zbins,] )

  if(lb == TRUE) arlX <- arlX * -1

  output <- data.frame( timestamp, arlBins, arlX, arlY, #Xvel.mean,
                        arlQC = qualCode ( arlBins, ensemble$FixedLeader$numCells ) )

  if(!is.null(goodbins))   output <- c(output, goodbins)
  output <- c(output, arl)
  return(output)
}

binVelocity <- function(ensemble, lb, distance = 0)
{
  BADRESULT <- -32768.000

  Xvel <- ensemble$Variables$Velocity[[1]]
  Yvel <- ensemble$Variables$Velocity[[2]]

  meanCorrel <- (ensemble$Variables$Correlation[[1]] +
                   ensemble$Variables$Correlation[[2]] ) / 2

  Xvel <- Xvel[ !meanCorrel < 72]
  Yvel <- Xvel[ !meanCorrel < 72]

  Xvel <- Xvel[Xvel != BADRESULT]
  Yvel <- Yvel[Yvel != BADRESULT]

  if (lb == TRUE) Xvel <- -1*Xvel

  if(length(Xvel) == 0) return(NULL)
  Distance = seq(ensemble$FixedLeader$bin1Dist / 100,
                 by = ensemble$FixedLeader$rangeCellLength / 100,
                 length.out = length(Xvel))

  df <- data.frame(Distance,
    Xvel, Yvel, meanCorrel = meanCorrel[1:length(Xvel)])
  if ( distance > ensemble$FixedLeader$bin1Dist / 100 )


    df <- df %>% dplyr::filter( Distance <= distance )

  return(df)

}

# process a single decoded ensemble
ensembleProcessing <- function(ensemble, xs = NULL, AdcpChain = NULL, AdcpRL = NULL, lb = NULL, distance = -1, doPlot = FALSE)
{
  # distance can be either -2, -1, 0 or a number greater than zero.
  # -2 = subset velocity output to only the good range of ARL ( not recommended )
  # -1 = subset velocity output to only within the bottom range ( recommended )
  # 0 = no limit, use all good bins
  # >0 = limit to this many metres of profiling always

  output <- list()

  Chainage <- seq(ensemble$FixedLeader$bin1Dist / 100,  by = ensemble$FixedLeader$rangeCellLength / 100, length.out = length(ensemble$Variables$Echo1[[1]]))
  ensEcho <- data.frame(Chainage,
                        cbind(ensemble$Variables$Echo1[[1]],
                              ensemble$Variables$Echo1[[2]])
  )

  arl <- processARL( ensemble, xs, AdcpChain, AdcpRL, lb, goodbins = goodbins, echo = ensEcho, doPlot = doPlot)

  output[["Depth"]]  <- ensemble$Variables$SurfStatus$DepthCorr

  range <- function(distance, arl){
    if(is.null(arl)) return(0)
    if(distance == 0) return(0)
    if(distance == -1) return(arl$btmrange)
    if(distance == -2) return(arl$range)
    return(distance)
  }
  vel <- binVelocity(ensemble, lb, range(distance, arl))
  if (is.null(vel) ){
    output[["GoodBins"]] <- 0
    return(output)
  }

  ensVelocity <- data.frame(
    Timestamp = ensemble$VariableLeader$Timestamp,
    vel,
    Echo1 = ensemble$Variables$Echo1[[1]][ 1:length(vel$Xvel) ],
    Echo2 = ensemble$Variables$Echo1[[2]][ 1:length(vel$Xvel) ]
  )

  output[["Xvel.mean"]] <- mean(vel$Xvel)
  output[["Yvel.mean"]] <- mean(vel$Yvel)

  # length of original data
  originalBins <- length(ensemble$Variables$Velocity[[1]])
  # length of end data
  goodbins <- length(vel$Xvel)

  output[["GoodBins"]] <- goodbins

  QC <- qualCode(goodbins, originalBins)
  # Quality code
  output[["QC"]] <- QC

  if(!is.null(ensemble$Variables$AmbientNoise))
    output[["AmbientNoise"]] <- data.frame(Timestamp = ensemble$VariableLeader$Timestamp, Ambient_Noise = ensemble$Variables$AmbientNoise$RSSI[[3]])

  #ensemble$Header

  # ensemble data
  # don't include this, it's in the ensembles list already
  #output[["VariableLeader"]] <- ensemble$VariableLeader
  #output[["FixedLeader"]] <- ensemble$FixedLeader
  #output[["SurfaceTrackStatus"]] <- ensemble$Variables$SurfStatus

  output[["EchoIntensity"]] <- ensemble$Variables$Echo1
  output[["Xvel"]] <- vel$Xvel
  output[["Yvel"]] <- vel$Yvel
  output[["BinData"]] <- ensVelocity # output all per bin data, including cell chainage
  output[["MeanCorrel"]] <- vel$meanCorrel
  output[["Status"]] <- ensembleStatus(ensemble)
  if (!is.null(arl))  output[["ARL"]] <- arl

  return(output)

}

bulkEnsProcess <- function(ensembles, xs = NULL, AdcpChain = NULL, AdcpRL = NULL, lb = NULL, distance = -1, doPlot = FALSE)
{
  start <- Sys.time()
  count <- 0
  Processed <- list()
  for(ensemble in ensembles)
  {
    #print(count)
    processed <- ensembleProcessing(ensemble, xs, AdcpChain = AdcpChain, AdcpRL = AdcpRL, lb = lb, distance = distance, doPlot = doPlot)
    #if(processed$GoodBins > 0)    Processed[[count <- count + 1]] <- processed
    if(processed$GoodBins > 0)    Processed[[names(ensembles)[count <- count + 1]]] <- processed
  }
  Sys.time() - start
  return(Processed)
}

### Functions from Stephen Wallace's ARLRange.R -----------------------------------------------
ARLRange <- function(xs, adcp.chain, adcp.rl, wl, ARL = 24, lb = TRUE, doPlot = FALSE, xs2 = NULL, tratio = 0, echo = NULL)
{

  # ARL = 1/12 for CM600
  # ARL = 1/10 for CM300

  # cm 600 = tan(1/12) * 180 / pi / 2
  # cm 300 = tan(1/10) * 180 / pi / 2

  surface <- function(wl, x) 0*x+wl
  beamline <- function(m,C,x) m*x+C

  slope = 1 / ARL

  if(is.null(xs2))   {
    bedline <- approxfun(xs)
  }else{
    bedline <- function(x) phasedXS(x, # bedline is an optional parameter, it is RL as a function of Chainage (x)
                                    xs, # cross section 1
                                    xs2, # cross section 2
                                    tratio = tratio # this is the ratio of how much to weight each xs
                                    # 0 = xs, 1 = xs2, 0.5 = half etc
    )
  }

  if ( adcp.rl <= bedline(adcp.chain) | adcp.rl > wl ) {
    message("ADCP lies outside wetted area...")
    return (0)
  }

  # plot beam lines
  # Calculate height of

  # calculate the y intercept
  #plot(adcp.chain, adcp.rl)

  # for a slope that inreases at the rate of "slope",
  # what would it be at the point of adcp.chain

  # rearrange mx+c to get chainage of top line intersect
  if( lb == TRUE)
  {
    cOffset <- adcp.chain * slope
    X <- (wl - ( adcp.rl - cOffset)) / slope #  y = mx + c;  x = ( y - c ) / m
    Y <- beamline( slope, adcp.rl - cOffset, X)
  }else{
    cOffset <- adcp.chain * slope
    X <- adcp.chain + ( adcp.chain - (wl - ( adcp.rl - cOffset)) / slope )
    Y <- beamline( -slope, adcp.rl + cOffset, X)
  }

  top <- data.frame(X, Y)


  # bottom line intersect
  if (lb == TRUE)
  {
    X <- uniroot(function(x)  beamline( m = -slope, C = adcp.rl + cOffset, x) - bedline(x),
                 c(adcp.chain, max(xs$chain))  )$root
    Y <- beamline( -slope, adcp.rl + cOffset, X)
    #thalweg <- optimize ( bedline, interval = c(adcp.chain, max(xs$chain)) )
  }else{
    X <- uniroot(function(x)  beamline( m = slope, C = adcp.rl - cOffset, x) - bedline(x),
                 c(min(xs$chain), adcp.chain)  )$root
    Y <- beamline( slope, adcp.rl - cOffset, X)
    #thalweg <- optimize ( bedline, interval = c(min(xs$chain), adcp.chain) )
  }

  bottom <- data.frame(X, Y)

  arl <- c(top = top, bottom = bottom)
  if (lb == TRUE)
  {
    top$AR <- ( top$X - adcp.chain ) / (top$Y - adcp.rl)
    bottom$AR <- ( bottom$X - adcp.chain ) / ( adcp.rl - bottom$Y )
    arl$toprange <- top$X - adcp.chain
    arl$btmrange <- bottom$X - adcp.chain
  }else{
    top$AR <- ( adcp.chain - top$X  ) / (top$Y - adcp.rl)
    bottom$AR <- ( adcp.chain - bottom$X ) / ( adcp.rl - bottom$Y )
    arl$toprange <- adcp.chain - top$X
    arl$btmrange <- adcp.chain - bottom$X
  }
  arl$range <- min(( arl$toprange ), ( arl$btmrange  ))

  #arl <- c(arl, thalweg)

  #print(arl)

  if( doPlot == TRUE)
  {
    par(mfrow = c(2,1))
    par(mar = c(2, 2, 2, 2))
    #plot(xs, xlim = c(adcp.chain - 40, (max(top$X, bottom$X) + 40) ))
    #lines(xs)

    if(lb == TRUE){
      plot(function(x) bedline(x), xlim = c(adcp.chain - 40, (max(top$X, bottom$X) + 40) ))
    }else{
      plot(function(x) bedline(x), xlim = c((min(top$X, bottom$X) - 40), adcp.chain + 40 ))
    }

    points(adcp.chain, adcp.rl, col="red", pch = 17)
    abline(wl, 0, col="blue")

    abline(a = adcp.rl - cOffset,
           b = slope)

    abline(a = adcp.rl + cOffset,
           b = -slope)

    points(top, col="red", pch = 4)
    points(bottom, col="red", pch = 4)

    if(lb == TRUE)
    {
      segments(x0 = arl$range+adcp.chain ,
               y0 = max( beamline( -slope, adcp.rl + cOffset, arl$range+adcp.chain),
                         bottom$Y),
               x1 = arl$range+adcp.chain ,
               y1= min(beamline( slope, adcp.rl - cOffset, arl$range+adcp.chain),
                       top$Y),
               col="orange" )
      segments(x0 = adcp.chain , y0 = adcp.rl,  x1 = adcp.chain + arl$range ,
               y1= adcp.rl, col="orange" )
    }else{
      segments(x0 = adcp.chain - arl$range ,
               y0 = max( beamline( slope, adcp.rl - cOffset, adcp.chain - arl$range),
                         bottom$Y),
               x1 = adcp.chain - arl$range ,
               y1= min(beamline( -slope, adcp.rl + cOffset, adcp.chain - arl$range),
                       top$Y),
               col="orange" )
      segments(x0 = adcp.chain , y0 = adcp.rl,  x1 = adcp.chain - arl$range ,
               y1= adcp.rl, col="orange" )
    }


    if(!is.null(echo))
    {
      if(lb == TRUE){
        # plot(function(x) bedline(x), xlim = c(adcp.chain - 40, (max(top$X, bottom$X) + 40) ))
        plot(echo$Chainage + adcp.chain, echo$X1, xlim = c(adcp.chain - 40, (max(top$X, bottom$X) + 40)),
             ylim = c(70,250))
        points(echo$Chainage + adcp.chain, echo$X2, col="red")

      }else{
        # plot(function(x) bedline(x), xlim = c((min(top$X, bottom$X) - 40), adcp.chain + 40 ))
        plot(adcp.chain - echo$Chainage, echo$X1, xlim = c((min(top$X, bottom$X) - 40), adcp.chain + 40 ),
             ylim = c(70,250))
        points(adcp.chain - echo$Chainage, echo$X2, col="red")
      }
    }


    #invisible(readline(prompt="Press [enter] to continue"))
    Sys.sleep(0.01)

  }

  # max range for ARL
  return( arl )


}

phasedXS <- function(x, xs, xs2 = NULL, tratio = 0) {
  f.xs <- approxfun(xs)
  if(is.null(xs2)) xs2 <- xs
  f.xs2 <- approxfun(xs2)

  tratio[tratio > 1] <- 1
  tratio[tratio < 0] <- 0

  tratio * (f.xs2(x) - f.xs(x)) + f.xs(x)
}

### Functions from Stephen Wallace's decimal2Base.R -----------------------------------------------

# A function to convert a decimal number to a new base
# Stephen Wallace

decimal2Base <- function(decimal, base)
{
  baseDigits <- function(decimal, base = 10, count = 0){
    value <- decimal / base ^ count
    if (value < 1) return(count)
    baseDigits(decimal, base, count + 1)
  }

  digits <- baseDigits(decimal, base = base)
  newBase <- rep(0, digits)
  for(digit in seq(digits,1) ){
    result <- floor(decimal / base ^ (digit-1))
    newBase[digit] <- result
    decimal <- decimal - result * base ^ (digit-1)
  }
  return( rev(newBase) )
}

# converts binary to decimal
bin2dec <- function(vec)
{
  vec <- rev(vec)
  decvalue <- 0
  for(bindigit in 1:(length(vec)))
  {
    decvalue <- decvalue + vec[bindigit]*2^(bindigit-1)
  }
  return(decvalue)
}






### Process ARL modified for one cross-section when 000 file occurs before or after xs data ----
processARL2 <- function(xs,ensemble,ADCPChain,ADCPrl,lb,goodbins = NULL, echo = NULL) {

  timestamp <- ensemble$VariableLeader$Timestamp
  if (timestamp<=min(xs$Periods$PeriodStart)) {
    zXS <- is.element(xs$XS$n,xs$Periods$TableNum[min(xs$Periods$PeriodStart)==xs$Periods$PeriodStart])
    zXSdf <- data.frame(chain = xs$XS$CHAIN[zXS], rl = xs$XS$RL[zXS] )

  }
  if (timestamp>=max(xs$Periods$PeriodStart)) {
    zXS <- is.element(xs$XS$n,xs$Periods$TableNum[max(xs$Periods$PeriodStart)==xs$Periods$PeriodStart])
    zXSdf <- data.frame(chain = xs$XS$CHAIN[zXS], rl = xs$XS$RL[zXS] )
  }

  ensStatus <- ensembleStatus(ensemble)
  wl = ensemble$Variables$SurfStatus$DepthCorr + ADCPrl

  arl <- ARLRange(zXSdf,ADCPChain, ADCPrl, wl, ARL = ensStatus$Beam_aspect_ratio,lb)

  arlBins <- (arl$range - ensemble$FixedLeader$bin1Dist/100)/(ensemble$FixedLeader$rangeCellLength/100)
  if (!is.null(goodbins))
    arlBins[arlBins > goodbins] <- goodbins
  zbins <- 1:max(1:as.integer(arlBins))
  if (length(ensemble$Variables$Velocity[[1]]) < length(zbins)) {
    zbins <- zbins[1:length(ensemble$Variables$Velocity[[1]])]
  }
  arlX <- mean(ensemble$Variables$Velocity[[1]][zbins, ])
  arlY <- mean(ensemble$Variables$Velocity[[2]][zbins, ])
  if (lb == TRUE)
    arlX <- arlX * -1
  output <- data.frame(timestamp, arlBins, arlX, arlY, arlQC = qualCode(arlBins,
                                                                        ensemble$FixedLeader$numCells))
  if (!is.null(goodbins))
    output <- c(output, goodbins)
  output <- c(output, arl)
  return(output)
}

