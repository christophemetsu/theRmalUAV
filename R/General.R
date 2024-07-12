#' Gets all the necessary info
#'
#' @description
#' `T_uav_info()` gets all the information needed for further analysis
#'
#' @param path The path to the folder containing the tiff files, or path to 1 tiff file
#' @param sensor (character) Indicates the sensor alias name as given in the sensor.csv file
#' @param meta_csv The Patch to an ddditional meta data can be provided in the form of a csv, if not provided all info will be derived from exif data (which may limit some function options)
#' @param flight_height (numerical) the height between the SENSOR and the GROUND. can be a single value or a vector of the same length as the number of images. If not specified it will search for GPS altitude in the exif/meta data
#' @return A Thermal.UAV object with all the necessary information, serves as input in the following functions
#' @export
T_uav_info <- function(path, sensor = "ThermalCapture", meta_csv = NA, flight_height = NA){

  # Make first general class Thermal.Info
  thermal_uav <- invisible(methods::new("Thermal.UAV"))
  # Read Exif Data
  message("Reading Exif data ")
  if (grepl(".tif", path)){ # In case of path equals path to one image
    split_path <- base::strsplit(path, "/")[[1]]
    thermal_uav@Info@path <- paste0("/", paste(split_path[-length(split_path)], collapse = "/"), "/")
    thermal_uav@Info@images <- split_path[length(split_path)]
    thermal_uav@Info@exif <- exiftoolr::exif_read(path) # Needed to check the sensor
  } else { # In case of map
    if (!substr(path, nchar(path), nchar(path)) == "/") {
      path <- paste0(path, '/')
    }
    thermal_uav@Info@path <- path
    images <- list.files(path = path, pattern = '*.tif', full.names = FALSE)
    if (length(unique(nchar(gsub(".*_(.+).tif.*", "\\1", images)))) > 1){    # if file names do not have the same label length, img_1000.tif and img_10000.tif are placed right after each other, not desirable
      warning("Large dataset; image labels do not have the same length \n")
      message("Adding leading zero's to filename to avoid mismatches \n")
      id_length_max <- max(unique(nchar(gsub(".*_(.+).tif.*", "\\1", images))))
      images <- list.files(path = path, pattern = '*.tif', full.names = TRUE)
      images2 <- stringr::str_replace(images, "\\d+[.tif]", function(m) stringr::str_pad(m, (id_length_max+1), pad = '0'))
      file.rename(images, images2)
      images <- list.files(path = path, pattern = '*.tif', full.names = FALSE)
    }
    thermal_uav@Info@images <- images
    # In case ".tif.aux.xml" are present in de map
    if (length(which(grepl(".tif.aux.xml", thermal_uav@Info@images) == TRUE)) != 0){
      thermal_uav@Info@images <- thermal_uav@Info@images[-which(grepl(".tif.aux.xml", thermal_uav@Info@images) == TRUE)]
    }
    thermal_uav@Info@exif <- exiftoolr::exif_read(list.files(path = path, pattern = '*.tif', full.names = TRUE)) # Needed to check the sensor + later on
  }
  # Get sensor information
  thermal_uav@Info@sensor <- base::unique(thermal_uav@Info@exif$Model)
  if (length(thermal_uav@Info@sensor) > 1){
    stop(paste("TIFs of multiple sensors detected \nPlease seperate TIFs in different folders per sensor \n"))
  }
  check_sensor <- sensor %in% sensor_info$sensor_name
  if (!check_sensor){
    stop(paste("Sensor not available in sensor list \nPlease add your sensor with the characteristics in the sensor.csv file"))
  }
  thermal_uav@Info@sensor_info <- sensor_info[sensor_info$sensor_name == sensor,]
  thermal_uav@Info@dataset_length <- length(thermal_uav@Info@images)
  # if additional meta data is provided
  if (!is.na(meta_csv)){
    # metadata is given as csv file, example in case after ThermoViewer
    message("Reading additional metadata")
    thermal_uav@Info@meta_df <- utils::read.csv(meta_csv)
    # Failsafe for ThermalCapture, the csv thermoViewer exports is not conform with the general format.
    if (sensor == "ThermalCapture" & ncol(thermal_uav@Info@meta_df) == 1){
      thermal_uav@Info@meta_df <- utils::read.csv2(meta_csv, dec = ".")
    }
    # Check if number of images is equal to the number of rows in the meta.csv
    if (thermal_uav@Info@dataset_length != nrow(thermal_uav@Info@meta_df)) {
      stop(paste("Nr of images is not equal to rows in Meta-table\n"))
    }
    # Get thermal time
    thermal_uav@Info@TTime <- as.POSIXct(paste(thermal_uav@Info@meta_df$Date,thermal_uav@Info@meta_df$Time),
                                         tryFormats = c("%d.%m.%Y %H:%M:%OS", "%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%Y:%m:%d %H:%M:%OS"), tz = "UTC")
    # Check if GPS is recorded
    message("Checking for GPS data")
    if (any(is.numeric(thermal_uav@Info@meta_df$Latitude))) {
      thermal_uav@Position@GPS <- 1
      thermal_uav@Position@Lat <- thermal_uav@Info@meta_df$Latitude
      thermal_uav@Position@Lon <- thermal_uav@Info@meta_df$Longitude
    } else {
      thermal_uav@Position@GPS  <- 0
    }
  } else {
    # Get thermal time
    if (thermal_uav@Info@sensor_info$date_time %in% names(thermal_uav@Info@exif)){
      thermal_uav@Info@TTime <- as.POSIXct(dplyr::select(thermal_uav@Info@exif, thermal_uav@Info@sensor_info$date_time)[[1]],
                                           tryFormats = c("%d.%m.%Y %H:%M:%OS", "%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%Y:%m:%d %H:%M:%OS"), tz = "UTC")
    } else {
      warning("date_time tag in sensor.csv not not found in exif data \n")
      thermal_uav@Info@TTime <- 0
    }
    # Check if GPS is recorded
    # Latitude
    if (thermal_uav@Info@sensor_info$tag_lat %in% names(thermal_uav@Info@exif)){
      if (any(is.numeric(dplyr::select(thermal_uav@Info@exif, thermal_uav@Info@sensor_info$tag_lat)[[1]]))){
        thermal_uav@Position@Lat <- dplyr::select(thermal_uav@Info@exif, thermal_uav@Info@sensor_info$tag_lat)[[1]]
      } else {
        warning("No latitudinal GPS data recorded, GPS info is set to 0 \n")
        thermal_uav@Position@Lat <- 0
        thermal_uav@Position@GPS <- 0
      }
    } else {
      warning("tag_lat in sensor.csv not found in exif data, GPS info is set to 0 \n")
      thermal_uav@Position@Lat <- 0
      thermal_uav@Position@GPS <- 0
    }
    # Longitude
    if (thermal_uav@Info@sensor_info$tag_lon %in% names(thermal_uav@Info@exif)){
      if (any(is.numeric(dplyr::select(thermal_uav@Info@exif, thermal_uav@Info@sensor_info$tag_lon)[[1]]))){
        thermal_uav@Position@Lon <- dplyr::select(thermal_uav@Info@exif, thermal_uav@Info@sensor_info$tag_lon)[[1]]
      } else {
        warning("No longitudinal GPS data recorded, GPS info is set to 0 \n")
        thermal_uav@Position@Lon <- 0
        thermal_uav@Position@GPS <- 0
      }
    } else {
      warning("tag_lon in sensor.csv not found in exif data, GPS info is set to 0 \n")
      thermal_uav@Position@Lon <- 0
      thermal_uav@Position@GPS <- 0
    }
  }
  # Get SubSecTime if possible
  if (nchar(thermal_uav@Info@sensor_info$SubSecTime) != 0 & !(is.na(thermal_uav@Info@sensor_info$SubSecTime))){
    if (!(thermal_uav@Info@sensor_info$SubSecTime %in% names(thermal_uav@Info@exif))){
      stop(paste0("SubSecTime is provided in the sensor.csv, but not found in the exifdata. Please provide the correct label or leave blank \n"))
    } else {
      thermal_uav@Info@SubSecTime <- thermal_uav@Info@exif[, thermal_uav@Info@sensor_info$SubSecTime]
    }
  } else if (length(unique(substr(format(thermal_uav@Info@TTime, "%OS3"),4,6))) > 1){ # If SubSecTime is stored in datetime, pull them apart, needed for further processing
    thermal_uav@Info@SubSecTime <- substr(format(thermal_uav@Info@TTime, "%OS3"),4,6)
    thermal_uav@Info@TTime <- as.POSIXct(as.character(strftime(thermal_uav@Info@TTime, tz = "UTC")), tz = "UTC")
  } else { # If multiple images per second, try estimate SubSecTime based on frequency
    UnTT <- base::unique(thermal_uav@Info@TTime)
    SubSecTime <- rep(0, length(thermal_uav@Info@TTime))
    for (i1 in seq_along(UnTT)) {
      Id <- which(UnTT[i1] == thermal_uav@Info@TTime)
      for (i2 in 1:length(Id)) {
        SubSecTime[Id[i2]] <- (i2-1)/length(Id) # Sub second time stored as integer, possibly needed for further analysis
      }
    }
    thermal_uav@Info@SubSecTime <- as.integer(1000*SubSecTime)
  }
  # Get the right flight height
  message("Checking for flight height")
  if (is.na(flight_height)){
    if (length(thermal_uav@Info@meta_df) != 0){ # Try to search for flight height (FH) in the provided meta data
      if (!anyNA(thermal_uav@Info@meta_df$Altitude)){ # If the data is complete use this as FH
        thermal_uav@Position@FH <- thermal_uav@Info@meta_df$Altitude
        message("Flight height (FH) taken from provided metadata csv file \n")
      } else { # Need to take it from exif data
        if (thermal_uav@Info@sensor_info$tag_elev_agl %in% names(thermal_uav@Info@exif)){
          if (any(is.numeric(dplyr::select(thermal_uav@Info@exif, thermal_uav@Info@sensor_info$tag_elev_agl)[[1]]))){
            thermal_uav@Position@FH <- dplyr::select(thermal_uav@Info@exif, thermal_uav@Info@sensor_info$tag_elev_agl)[[1]]
            message("One or more values were missing in provided meta data -> flight height (FH) from exif data \n")
          } else {
            stop(paste0("No altitude data recorded, please provide flight height either through the meta data csv or the flight_height variabel in the function \n"))
          }
        } else {
          stop(paste0("'tag_elev_agl' in thermal_sensor.csv not found in exif data, please provide the correct tag \n"))
        }
      }
    } else {
      if (thermal_uav@Info@sensor_info$tag_elev_agl %in% names(thermal_uav@Info@exif)){
        if (any(is.numeric(dplyr::select(thermal_uav@Info@exif, thermal_uav@Info@sensor_info$tag_elev_agl)[[1]]))){
          thermal_uav@Position@FH <- dplyr::select(thermal_uav@Info@exif, thermal_uav@Info@sensor_info$tag_elev_agl)[[1]]
          message("Flight height (FH) taken from exif data \n")
        } else {
          stop(paste0("No altitude data recorded, please provide flight height either through the meta data csv or the flight_height variabel in the function \n"))
        }
      } else {
        stop(paste0("'tag_elev_agl' in thermal_sensor.csv not found in exif data, please provide the correct tag \n"))
      }
    }
  } else {
    if (length(flight_height) == 1){
      thermal_uav@Position@FH <- rep(flight_height, thermal_uav@Info@dataset_length)
    } else if (length(flight_height) == thermal_uav@Info@dataset_length){
      thermal_uav@Position@FH <- flight_height
    } else {
      stop(paste0("The length of provided flight_height vector does not match the number of images \n"))
    }
  }
  return(thermal_uav)
}

#' Provides a list of supported sensors
#'
#' @description
#' `T_uav_sensors()` provides an overview of all the sensors which can be used for the processing
#'
#' @return prints an overview of the available sensors
#' @export
T_uav_sensors <- function(){
  # Read sensor info
  # tryCatch({
  #   sensor_info <- utils::read.csv("sensors/sensors.csv")
  # }, error=function(e){cat("ERROR : sensor.csv could not be read \noriginal error:",conditionMessage(e), "\n")})
  # List all sensors
  df <- sensor_info[,c("sensor_name", "description")]
  names(df) <- c("Sensor Name", "Description")
  knitr::kable(df, caption = "Available sensors")
}

#' Makes a TheRmalUAV report
#'
#' @description
#' `T_uav_report()` writes a report with information about the processing done with this package
#'
#' @param thermal_uav Thermal.UAV object retrieved by previous functions (e.g.: `T_uav_info()`).
#' @param path the path to the folder were you want to save the report. If NA the report will be saved in the path of the original tifs
#' @param project_name (character) the name of the project to put in the report
#' @param flight_name (character) the name of the flight to put in the report
#' @param pilot_name (character) the name of the pilot to put in the report
#' @param location (character) the location of where the flight was executed
#' @return exports the Thermal.Data as tif files
#' @export
T_uav_report <- function(thermal_uav, path = NA, project_name = "Thermal_uav", flight_name = "", pilot_name = "pilot_name", location = "location"){

  # Check class structure of thermal_uav
  if (!isa(thermal_uav, "Thermal.UAV")){
    stop(paste0("The provided parameter thermal_uav is not of class Thermal.UAV \n"))
  }
  # The path where the report should be saved
  if (!is.na(path)){
    if (!dir.exists(path)){
      stop(paste0("The provided path does not exist \n"))
    } else if (!substr(path, nchar(path), nchar(path)) == "/") {
      path <- paste0(path, '/')
    }
  } else {
    path <- thermal_uav@Info@path
  }
  # R markdown
  package_path <- find.package("theRmalUAV")
  rmarkdown::render(paste0(package_path,'/rmd/Report.Rmd'), output_dir = path, params = list(output_file = paste0(path, "Thermal_uav_report.html"), output_format = "html_document", output_options = list(widescreen = T)))
}

#' Exports thermal data as tiff-files
#'
#' @description
#' `T_uav_export()` exports the Thermal.Data. Note in order to export the data, at least T_uav_corr_atm should have been ran first
#'
#' @param thermal_uav Thermal.UAV object retrieved by previous functions (e.g.: `T_uav_info()`).
#' @param export_path path to the folder where the images should be stored. If NA, images are stored in a new folder in the original path called 'Ts'
#' @return exports the Thermal.Data as tiff files. Note data is stored as centikelvin.
#' @export
T_uav_export <- function(thermal_uav, export_path = NA){

  suppressWarnings({
    # Check class structure of thermal_uav
    if (!isa(thermal_uav, "Thermal.UAV")){
      stop(paste0("The provided parameter thermal_uav is not of class Thermal.UAV \n"))
    }
    if (length(thermal_uav@Thermal.Data) == 0){
      stop(paste0("Thermal.Data is empty \n"))
    }
    # The path where all the tifs should be exported
    if (!is.na(export_path)){
      if (!dir.exists(export_path)){
        stop(paste0("The provided export_path does not exist \n"))
      } else if (!substr(export_path, nchar(export_path), nchar(export_path)) == "/") {
        export_path <- paste0(export_path, '/')
      }
    } else {
      if (!("Ts" %in% thermal_uav@Info@path)) {
        # Create a new map
        dir.create(paste0(thermal_uav@Info@path, "Ts"))
        export_path <- paste0(thermal_uav@Info@path, "Ts/")
      }
    }
    # Create a data.frame with all the info that should be exported
    exif_export <- data.frame(matrix(NA, nrow = thermal_uav@Info@dataset_length, ncol = 0))
    # Change the pathnames accordingly
    exif_export$SourceFile <- paste0(export_path, thermal_uav@Info@images)
    exif_export$SourceFile <- gsub(".tif", "_Ts.tif", exif_export$SourceFile)
    exif_export$FileName <- gsub(".tif", "_Ts.tif", thermal_uav@Info@images)
    exif_export$Directory <- rep(substr(export_path, 1, nchar(export_path)-1), thermal_uav@Info@dataset_length)
    # Time data
    exif_export$DateTimeOriginal <- gsub("-", ":", as.character(thermal_uav@Info@TTime)) # converted to :
    if (length(thermal_uav@Info@SubSecTime) != 0){
      exif_export$SubSecTime <- thermal_uav@Info@SubSecTime
    }
    # sensor data
    exif_export$Make <- thermal_uav@Info@exif$Make
    exif_export$Model<- thermal_uav@Info@exif$Model
    if ("FocalLength" %in% names(thermal_uav@Info@exif)){
      exif_export$FocalLength <- thermal_uav@Info@exif$FocalLength
    }
    if ("FocalPlaneResolutionUnit" %in% names(thermal_uav@Info@exif)){
      if (thermal_uav@Info@exif$FocalPlaneResolutionUnit[1] == 2){
        if ("FocalPlaneXResolution" %in% names(thermal_uav@Info@exif)){
          exif_export$FocalPlaneXResolution <- thermal_uav@Info@exif$FocalPlaneXResolution
        }
        if ("FocalPlaneYResolution" %in% names(thermal_uav@Info@exif)){
          exif_export$FocalPlaneYResolution <- thermal_uav@Info@exif$FocalPlaneYResolution
        }
      } else if (thermal_uav@Info@exif$FocalPlaneResolutionUnit[1] == 3){
        if ("FocalPlaneXResolution" %in% names(thermal_uav@Info@exif)){
          exif_export$FocalPlaneXResolution <- 2.54*thermal_uav@Info@exif$FocalPlaneXResolution
        }
        if ("FocalPlaneYResolution" %in% names(thermal_uav@Info@exif)){
          exif_export$FocalPlaneYResolution <- 2.54*thermal_uav@Info@exif$FocalPlaneYResolution
        }
      } else if (thermal_uav@Info@exif$FocalPlaneResolutionUnit[1] == 4){
        if ("FocalPlaneXResolution" %in% names(thermal_uav@Info@exif)){
          exif_export$FocalPlaneXResolution <- 10*2.54*thermal_uav@Info@exif$FocalPlaneXResolution
        }
        if ("FocalPlaneYResolution" %in% names(thermal_uav@Info@exif)){
          exif_export$FocalPlaneYResolution <- 10*2.54*thermal_uav@Info@exif$FocalPlaneYResolution
        }
      }
      exif_export$FocalPlaneResolutionUnit <- rep(2, thermal_uav@Info@dataset_length)
    }
    if ("CircleOfConfusion" %in% names(thermal_uav@Info@exif)){
      exif_export$CircleOfConfusion <- thermal_uav@Info@exif$CircleOfConfusion
    }
    if ("FOV" %in% names(thermal_uav@Info@exif)){
      exif_export$FOV <- thermal_uav@Info@exif$FOV
    }
    if ("GPSLatitudeRef" %in% names(thermal_uav@Info@exif)){
      exif_export$GPSLatitudeRef <- thermal_uav@Info@exif$GPSLatitudeRef
    }
    if ("GPSLongitudeRef" %in% names(thermal_uav@Info@exif)){
      exif_export$GPSLongitudeRef <- thermal_uav@Info@exif$GPSLongitudeRef
    }
    # GPS data
    if (length(thermal_uav@Position@opt_positions) != 0){
      exif_export$GPSLatitude <- thermal_uav@Position@opt_positions$Lat
      exif_export$GPSLongitude <- thermal_uav@Position@opt_positions$Lon
      exif_export$GPSPosition <- paste(thermal_uav@Position@opt_positions$Lat, thermal_uav@Position@opt_positions$Lon)
      exif_export$GPSAltitude <- thermal_uav@Position@opt_positions$Z
      exif_export$GimbalYawDegree <- thermal_uav@Position@opt_positions$Yaw
      exif_export$GimbalRollDegree <- thermal_uav@Position@opt_positions$Roll
      exif_export$GimbalPitchDegree <- -90 + thermal_uav@Position@opt_positions$Pitch # GimbalPitchDgeree and Pitch is read differently in Photogrammetry
    } else {
      exif_export$GPSLatitude <- thermal_uav@Position@Lat
      exif_export$GPSLongitude <- thermal_uav@Position@Lon
      exif_export$GPSPosition <- paste(thermal_uav@Position@Lat, thermal_uav@Position@Lon)
      exif_export$GimbalYawDegree <- thermal_uav@Position@Yaw
      if (nchar(thermal_uav@Info@sensor_info$tag_pitch) != 0 & thermal_uav@Info@sensor_info$tag_pitch %in% names(thermal_uav@Info@exif)){
        exif_export$GimbalPitchDegree <- thermal_uav@Info@exif[thermal_uav@Info@sensor_info$tag_pitch][[1]]
      } else {
        exif_export$GimbalPitchDegree <- rep(0, thermal_uav@Info@dataset_length)
      }
      if (nchar(thermal_uav@Info@sensor_info$tag_roll) != 0& thermal_uav@Info@sensor_info$tag_roll %in% names(thermal_uav@Info@exif)){
        exif_export$GimbalRollDegree <- thermal_uav@Info@exif[thermal_uav@Info@sensor_info$tag_roll][[1]]
      }
      if (nchar(thermal_uav@Info@sensor_info$tag_elev) != 0 & thermal_uav@Info@sensor_info$tag_elev %in% names(thermal_uav@Info@exif)){
        exif_export$GPSAltitude <- thermal_uav@Info@exif[thermal_uav@Info@sensor_info$tag_elev][[1]]
      } else if (length(thermal_uav@Info@meta_df) != 0 & "Altitude" %in% names(thermal_uav@Info@meta_df) & length(thermal_uav@Info@meta_df$Altitude) != 0) {
        exif_export$GPSAltitude <- thermal_uav@Info@meta_df$Altitude
      } else {
        exif_export$GPSAltitude <- thermal_uav@Position@FH
      }
    }
    # export exif csv
    utils::write.csv(exif_export, file = paste0(export_path, "exif.csv"), row.names = FALSE, sep = ",")
    # for loop to export path
    len <- thermal_uav@Info@dataset_length
    pb <- progress::progress_bar$new(
      format = "Exporting thermal images: [:bar] :percent ETA: :eta",
      total = len
    )
    for (i in 1:len){
      my_matrix <- ceiling(100*thermal_uav@Thermal.Data[[i]])
      my_matrix_int <- matrix(as.integer(my_matrix), nrow = dim(my_matrix)[1], ncol = dim(my_matrix)[2])/65535
      tiff::writeTIFF(my_matrix_int, exif_export$SourceFile[i], bits.per.sample = 16, compression = "none")
      pb$tick()
    }
    # Alter exif data
    message("Writing exif data")
    invisible(exiftoolr::exif_call(path = substr(export_path, 1, nchar(export_path)-1),
                                   args = paste0('-csv = ',export_path,'exif.csv')))
    # exif_call(path = substr(export_path, 1, nchar(export_path)-1),
    #           args = paste0('-json = ',export_path,'exif_json.json'))
    # Remove the original tiff files
    remove_orig <- list.files(path = export_path, pattern = '*.tif_original', full.names = TRUE)
    invisible(file.remove(remove_orig))
  })
}

methods::setClass("Thermal.Info", slots = c(path = "character", images = "character", exif = "ANY", sensor = "character", sensor_info = "data.frame",
                                            meta_df = "data.frame", TTime = c("POSIXct"), SubSecTime = "ANY", dataset_length = "numeric"))

methods::setClass("Thermal.Position", slots = c(GPS = "numeric", Lat = "numeric", Lon = "numeric", FH = "numeric", Yaw = "numeric", Zone = "numeric", hemisphere = "character", units = "character", crs = "ANY",
                                                locations_vector = "ANY", extents_vector = "ANY", overlap = "numeric", opt_positions = "data.frame", min_overlap = "numeric",
                                                rig_offset = "numeric", timediff = "numeric", T_uav_pos_coreg = "character"))

methods::setClass("Thermal.Sharpness", slots = c(Tsharp = "numeric", Thresh = "numeric", number_keep = "numeric", unsharp_kept_ID = "ANY", method = "character",
                                                 T_uav_clean_persec = "character", img_persec_remove = "numeric", T_uav_clean_reduc = "character", img_reduc_remove = "numeric"))

methods::setClass("Thermal.Atmosphere", slots = c(T_air = "numeric", T_air_mode = "numeric", rel_hum = "numeric", flight_height = "numeric", omega = "numeric",
                                                  Tau_atm = "numeric", emiss = "numeric", T_bg = "numeric", T_uav_corr_atm = "character"))

methods::setClass("Thermal.Smooth", slots = c(smooth_length = "numeric", T_smooth = "numeric", method = "character", T_uav_corr_smooth = "character"))

#' Class "Thermal.UAV"
#'
#' @description
#' `Thermal.UAV-class` groups all the necessary information together in a logical order.
#' The object of Class Thermal.UAV serves in most functions as input as it provides the data upon which most functions are build.
#' It consists of other thermal classes, each representing different kinds of information.
#'
#' @slot Info (S4 Thermal.Info) Holds general information regaring the information of the image files and project such as path, filenames, sensor info, meta data, time etc.
#' @slot Position (S4 Thermal.Position) Holds information about the position of the tifs including gps location, flightheight, overlap as well as shapefiles (when `T_uav_pos_sensor()` is run)
#' @slot Sharpness (S4 Thermal.Sharpness) Holds information about sharpness which is used for cleaning (e.g.: Sharpness threshold, cleaning methods, number of images removed. etc.)
#' @slot Atmosphere (S4 Thermal.Atmosphere) Holds information about the atmospheric conditions during the flight including air temperature, relative humidity, atm. transmission, emissivity
#' @slot Smooth (S4 Thermal.Smooth) Holds information about the thermal smoothing correction such as: smooth length, method etc.
#' @slot Thermal.Data (list) a list of matrices holding the thermal tiff files. Depending on which functions are already run, it represents raw or corrected data.
#' @name Thermal.UAV-Class
#' @examples
#' # Creates an empty Thermal.UAV class
#' thermal_class <- methods::new("Thermal.UAV")
methods::setClass("Thermal.UAV", slots = c(Info = "Thermal.Info", Position = "Thermal.Position",
                                           Sharpness = "Thermal.Sharpness", Atmosphere = "Thermal.Atmosphere", Smooth = "Thermal.Smooth", Thermal.Data = "list"))

