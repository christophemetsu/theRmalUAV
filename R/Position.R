#' Gets the information about the locations of the cameras
#'
#' @description
#' `T_uav_pos_sensor()` gives the location of all images with the option to retrieve the extents overlap. Furthermore, it gives the option to export the GPS/extent information
#'
#' @param thermal_uav Thermal.UAV object retrieved by previous functions (e.g.: `T_uav_info()`)
#' @param extent (logical) use TRUE to get the extents instead of the GPS locations, default is FALSE
#' @param overlap (logical) use TRUE if you want to calculate the average overlap between the subsequent images (only if `extent` = TRUE), default is FALSE
#' @param export (logical) use TRUE if you want to export the GPS/extents as a shapefile with the image names as attributes into a new folder "Shapefiles" within the path folder, default is FALSE
#' @return an updated Thermal.UAV object containing additional positional information
#' @export
T_uav_pos_sensor <- function(thermal_uav, extent = FALSE, overlap = FALSE, export = FALSE){

  # Check class structure of thermal_uav
  if (!isa(thermal_uav, "Thermal.UAV")){
    stop(paste0("The provided parameter thermal_uav is not of class Thermal.UAV \n"))
  }
  # Yaw for ThermalCapture
  if ((thermal_uav@Info@sensor == "ThermalCapture") & (length(thermal_uav@Info@meta_df) != 0) &
      (all(thermal_uav@Info@meta_df$Yaw == rep(0, length(thermal_uav@Info@meta_df$Yaw))) == TRUE) &
      ("TrackHeading" %in% names(thermal_uav@Info@meta_df))){
    thermal_uav@Info@meta_df$Yaw <- thermal_uav@Info@meta_df$TrackHeading
  }
  # If option extent is chosen or not
  if (extent == FALSE){
    df <- data.frame(
      Images = thermal_uav@Info@images,
      lat = thermal_uav@Position@Lat,
      lon = thermal_uav@Position@Lon
    )
    thermal_uav@Position@locations_vector <- terra::wrap(terra::vect(df, crs = "epsg:4326"))
  } else { # if option extent is chosen:
    # Look for YAW data
    if (length(thermal_uav@Info@meta_df) != 0){ # Try to search for yaw in the provided meta data
      if (!anyNA(thermal_uav@Info@meta_df$Yaw)){ # If the data is complete use this as Yaw
        thermal_uav@Position@Yaw <- thermal_uav@Info@meta_df$Yaw
        message("Yaw data taken from provided metadata csv file")
      } else { # Need to take it from exif data
        if (thermal_uav@Info@sensor_info$tag_yaw %in% names(thermal_uav@Info@exif)){
          if (any(as.numeric(dplyr::select(thermal_uav@Info@exif, thermal_uav@Info@sensor_info$tag_yaw)[[1]]))){
            thermal_uav@Position@Yaw <- as.numeric(dplyr::select(thermal_uav@Info@exif, thermal_uav@Info@sensor_info$tag_yaw)[[1]])
            message("One or more values were missing in provided meta data -> yaw from exif data")
          } else {
            stop(paste0("No yaw data recorded, please provide yaw through the meta data csv \n"))
          }
        } else {
          stop(paste0("'tag_yaw' in thermal_sensor.csv not found in exif data, please provide the correct tag \n"))
        }
      }
    } else {
      if (thermal_uav@Info@sensor_info$tag_yaw %in% names(thermal_uav@Info@exif)){
        if (any(as.numeric(dplyr::select(thermal_uav@Info@exif, thermal_uav@Info@sensor_info$tag_yaw)[[1]]))){
          thermal_uav@Position@Yaw <- as.numeric(dplyr::select(thermal_uav@Info@exif, thermal_uav@Info@sensor_info$tag_yaw)[[1]])
          message("Yaw data taken from exif data")
        } else {
          stop(paste0("No yaw data recorded, please provide yaw through the meta data csv \n"))
        }
      } else {
        stop(paste0("'tag_yaw' in thermal_sensor.csv not found in exif data, please provide the correct tag \n"))
      }
    }
    # Calculate extents
    Extents <- list()
    pb <- progress::progress_bar$new(
      format = "Extracting extents: [:bar] :percent ETA: :eta",
      total = thermal_uav@Info@dataset_length
    )
    for (i in 1:thermal_uav@Info@dataset_length){ # Loop over all the images
      # Get info about the zone & hemisphere
      thermal_uav@Position@Zone <-  (floor((thermal_uav@Position@Lon[i] + 180) / 6) %% 60) + 1
      if (thermal_uav@Position@Lat[i] > 0){
        thermal_uav@Position@hemisphere <- "north"
      } else {
        thermal_uav@Position@hemisphere <- "south"
      }
      thermal_uav@Position@units <- "m"
      # Get centroids
      xy <- data.frame(thermal_uav@Position@Lon[i], thermal_uav@Position@Lat[i])
      names(xy) <- c("long", "lat")
      vect <- terra::vect(xy, geom=c("long", "lat"))
      terra::crs(vect) <- terra::crs("epsg:4326")
      CRSstring <- paste0(
        "+proj=utm +zone=", thermal_uav@Position@Zone,
        " +ellps=WGS84",
        " +", thermal_uav@Position@hemisphere,
        " +units=", thermal_uav@Position@units)
      CRSstring_as_crs <- terra::crs(CRSstring)
      thermal_uav@Position@crs <- CRSstring_as_crs
      vect_proj <- terra::project(vect, CRSstring_as_crs)
      ext_o <- terra::ext(vect_proj)
      imageH_m <- (as.numeric(thermal_uav@Position@FH[i])*thermal_uav@Info@sensor_info$sensor_height)/thermal_uav@Info@sensor_info$focal_length_def
      imageW_m <- (as.numeric(thermal_uav@Position@FH[i])*thermal_uav@Info@sensor_info$sensor_width)/thermal_uav@Info@sensor_info$focal_length_def
      image_ext <- terra::ext((ext_o[1] - (imageW_m/2)),
                              (ext_o[2] + (imageW_m/2)),
                              (ext_o[3] - (imageH_m/2)),
                              (ext_o[4] + (imageH_m/2)))
      ext_poly <- terra::as.polygons(image_ext, crs = CRSstring_as_crs)
      ext_poly_spin <- terra::spin(ext_poly, as.numeric(thermal_uav@Position@Yaw[i]))
      Extents[i] <- list(ext_poly_spin)
      pb$tick()
    }
    # As one big vector file
    s <- lapply(Extents, terra::as.polygons) |> terra::svc()
    names(s) <- thermal_uav@Info@images
    thermal_uav@Position@extents_vector <- terra::wrap(terra::vect(s))
  }
  # calculate overlap if asked
  if (overlap == TRUE & extent == FALSE){
    warning("Overlap is not calculated as extent was not set to TRUE")
  }
  if (overlap == TRUE & extent == TRUE){
    len <- thermal_uav@Info@dataset_length-1
    overlap <- rep(NaN, len)
    pb <- progress::progress_bar$new(
      format = "Calculating overlap: [:bar] :percent ETA: :eta",
      total = len
    )
    for (i in 1:len){
      intersect <-  terra::intersect(s[i], s[(i+1)])
      area_ext <- terra::expanse(s[i], unit="m")
      area_int <- terra::expanse(intersect)
      overlap[i] <- area_int/area_ext
      pb$tick()
    }
    thermal_uav@Position@overlap <- mean(overlap)
  }
  # Export the vectors if asked
  if (export == TRUE){
    if (!("Shapefiles" %in% thermal_uav@Info@path)) {
      # Create a new map
      dir.create(paste0(thermal_uav@Info@path, "Shapefiles"))
      export_path <- paste0(thermal_uav@Info@path, "Shapefiles", "/")
    } else {
      export_path <- paste0(thermal_uav@Info@path, "Shapefiles", "/")
    }
    if (extent == FALSE){ # write locations
      terra::writeVector(terra::unwrap(thermal_uav@Position@locations_vector), paste0(export_path, "thermal_image_locations.shp"), overwrite = TRUE)
    } else {
      terra::writeVector(terra::unwrap(thermal_uav@Position@extents_vector), paste0(export_path, "thermal_image_extents.shp"), overwrite = TRUE)
    }
  }
  # end of function
  return(thermal_uav)
}

#' Shows the cameras/image extents on an interactive map
#'
#' @description
#' `T_uav_pos_view()` shows the extents or sensor positions in the viewer as interactive map. NOTE: the function `T_uav_pos_sensor()` should have been run previously
#'
#' @param thermal_uav Thermal.UAV object retrieved by previous functions (e.g.: `T_uav_info()`)
#' @param extent (logical) use TRUE if you want to plot the image extents. NOTE: only if extents are previously calculated in the `T_uav_pos_sensor()` function
#' @return shows the GPS locations or the extents in an interactive way
#' @export
T_uav_pos_view <- function(thermal_uav, extent){

  suppressWarnings({
    # Recreate the function crs from terra, as this is called in Leaflet,
    # without loading terra and without this function leaflet errors as function crs() is not found,
    # probably a bad call in the leaflet package
    crs <- function(x, proj = FALSE, describe = FALSE, parse = FALSE){
      suppressWarnings({
        y <- terra::crs(x, proj = FALSE, describe = FALSE, parse = FALSE)
      })
      return(y)
    }
    # check class structure of thermal_uav
    if (!isa(thermal_uav, "Thermal.UAV")){
      stop(paste0("The provided parameter thermal_uav is not of class Thermal.UAV \n"))
    }
    # Plot the positional information
    if (extent == TRUE){
      if (length(terra::unwrap(thermal_uav@Position@extents_vector)) != 0){
        plet <- terra::plet(terra::unwrap(thermal_uav@Position@extents_vector), label = TRUE)
      } else if (length(terra::unwrap(thermal_uav@Position@locations_vector)) != 0){
        plet <- terra::plet(terra::unwrap(thermal_uav@Position@locations_vector), label = TRUE)
        warning("No extents found, locations are plotted instead \n")
      } else {
        stop(paste0("No position shapefile found, please run the function T_uav_pos_sensor \n"))
      }
    } else {
      if (length(terra::unwrap(thermal_uav@Position@locations_vector)) != 0){
        plet <- terra::plet(terra::unwrap(thermal_uav@Position@locations_vector), label = TRUE)
      } else if ((length(terra::unwrap(thermal_uav@Position@extents_vector)) != 0)){
        plet <- terra::plet(terra::unwrap(thermal_uav@Position@extents_vector), label = TRUE)
        warning("No point locations found, extents are plotted instead \n")
      }
      else {
        stop(paste0("No position shapefile found, please run the function T_uav_pos_sensor \n"))
      }
    }
    return(plet)
  })
}

#' Prepares data for coregistration
#'
#' @description
#' `T_uav_prep_coreg()` prepares the data from a different sensor (with higher GPS accuracy) to use in the `T_uav_pos_coreg()` function.
#'
#' @param img_path the folder containing the high resolution images
#' @param SfM_option if the cameras were first aligned in a SfM software options are: "Agisoft Metashape", "Pix4DMapper". Use NA if this was not the case and you want to run it without previous alignement. Default is NA
#' @param opt_camera_path path to the .txt or .csv file where the optimized camera positions is stored (only if SfM_option is not NA), defualt is NA
#' @param sensor_name The name of the high resolution sensor of which the optimized GPS data will be used
#' @param epsg the epsg in which the coordinates are exported/stored, by default "4326" (WGS 84, latitude/longitude coordinate system based on the Earth's center of mass)
#' @param label needed for multispectral data, it is the subscript/sequence describing the band/sensor on which everything is calibrated (in case of the Micasence Altum-PT it is the panchromatic band: "_6"). If no label enter ''
#' @param timezone the timezone in which the datetime data is stored. Default is NA and this preset will use the timezone of your computer system
#' @return This function returns a data.frame containing positional information about optimized cameras which can be used as input in the `T_uav_pos_coreg()` function
#' @export
T_uav_prep_coreg <- function(img_path, SfM_option = NA, opt_camera_path = NA, epsg = "4326", sensor_name = "Altum-PT_MSP", label = NA, timezone = NA){

  # Check if path to image folder is provided
  if (anyNA(img_path)){
    stop(paste0("No image path found, please provide a valid image path. \n"))
  }
  # Check which sensor is used and extract necessary info
  # sensor_info <- utils::read.csv("sensors/sensors.csv")
  check_sensor <- sensor_name %in% sensor_info$sensor_name
  if (!check_sensor){
    stop(paste("Sensor not available in sensor list \nPlease add your sensor with the characteristics in the sensor.csv file"))
  }
  sensor_info <- sensor_info[sensor_info$sensor_name == sensor_name,]
  # Extract exif data
  if (!substr(img_path, nchar(img_path), nchar(img_path)) == "/") {
    img_path <- paste0(img_path, '/')
  }
  message("Reading Exif data of optimized cameras ")
  opt_cameras_exif <- exiftoolr::exif_read(list.files(path = img_path, pattern = paste0('*.',sensor_info$filetype), full.names = TRUE))
  # Account for possible .tif.aux.xml files
  if (length(which(grepl(".tif.aux.xml", opt_cameras_exif$FileName) == 1)) != 0){
    opt_cameras_exif <- opt_cameras_exif[-which(grepl(".tif.aux.xml", opt_cameras_exif$FileName) == 1),]
  }
  # check which option is provided and extract the good position info
  if (SfM_option == "Agisoft Metashape"){
    if (anyNA(opt_camera_path)){
      stop(paste0("csv/txt file not found, please provide the correct path to the csv file. \n"))
    }
    if (is.na(label)){
      stop(paste0("label was not provided correctly, please provide label, in case of no label enter '' "))
    }
    # Extract the needed information from the csv or txt file
    if (substr(opt_camera_path, nchar(opt_camera_path)-3, nchar(opt_camera_path)) == ".csv" | substr(opt_camera_path, nchar(opt_camera_path)-3, nchar(opt_camera_path)) == ".txt"){
      opt_cameras_df <- utils::read.csv2(opt_camera_path, header=FALSE, comment.char="#", sep = ",")
      names(opt_cameras_df) <- c("Label", "x", "y", "z", "yaw", "pitch", "roll", "x_est", "y_est", "z_est", "yaw_est", "pitch_est", "roll_est")
      opt_cameras_filtered <- opt_cameras_df[grepl(label, opt_cameras_df$Label), c("Label", "x_est", "y_est", "z_est", "yaw_est", "pitch_est", "roll_est")]
      opt_cameras_filtered$x_est <- as.numeric(opt_cameras_filtered$x_est); opt_cameras_filtered$y_est <- as.numeric(opt_cameras_filtered$y_est)
      opt_cameras_filtered$z_est <- as.numeric(opt_cameras_filtered$z_est); opt_cameras_filtered$yaw_est <- as.numeric(opt_cameras_filtered$yaw_est)
      opt_cameras_filtered$pitch_est <- as.numeric(opt_cameras_filtered$pitch_est); opt_cameras_filtered$roll_est <- as.numeric(opt_cameras_filtered$roll_est)
      opt_cameras_filtered <- stats::na.omit(opt_cameras_filtered)
    } else {
      stop(paste0("opt_camera_path is not provided as .csv or .txt file, please provide the right extention"))
    }
  } else if (SfM_option == "Pix4DMapper") {

    ############################################################################

    ########## INSERT

    ############################################################################

  } else if (is.na(SfM_option)){
    opt_cameras_filtered <- opt_cameras_exif[grepl(label, opt_cameras_exif$FileName),
                                             c("FileName", sensor_info$tag_lon, sensor_info$tag_lat, sensor_info$tag_elev,
                                               sensor_info$tag_yaw, sensor_info$tag_pitch, sensor_info$tag_roll)]
    names(opt_cameras_filtered) <- c("Label", "x_est", "y_est", "z_est", "yaw_est", "pitch_est", "roll_est")
  } else {
    stop(paste0("No valid option for SfM_option was provided, please enter one of the options mentioned in the description. \n"))
  }
  # If cameras where used for calibration purposes during the flight, they occured twice in the export camera dataset
  opt_cameras_filtered <- unique(opt_cameras_filtered)
  # Now we have all the necessary positional information. However we still need the exact time when these reference images are taken
  if (is.na(timezone)){
    timezone <- Sys.timezone()
  }
  if (nchar(sensor_info$date_time) == 0 | is.na(sensor_info$date_time)){
    stop(paste0("date_time is not provided in the sensor.csv file. Please provide the right date_time label \n"))
  }
  if (!(sensor_info$date_time %in% names(opt_cameras_exif))){
    stop(paste0("date_time label was not found in the exifdata of the sensor. Please provide the right date_time label \n"))
  }
  datetime <- as.POSIXct(opt_cameras_exif[opt_cameras_exif$FileName %in% opt_cameras_filtered$Label, sensor_info$date_time],
                         tz = timezone, tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%Y:%m:%d %H:%M:%OS"))
  # In case SubSecTime is provided
  if (nchar(sensor_info$SubSecTime) != 0 & !(is.na(sensor_info$SubSecTime))){
    if (!(sensor_info$SubSecTime %in% names(opt_cameras_exif))){
      stop(paste0("SubSecTime is provided in the sensor.csv, but not found in the exifdata. Please provide the correct label or leave blank"))
    } else {
      SubSecTime <- opt_cameras_exif[opt_cameras_exif$FileName %in% opt_cameras_filtered$Label, sensor_info$SubSecTime]
    }
  } else {
    SubSecTime <- rep(0, length(datetime))
    warning("SubSecTime is not provided/found, can have negative implications in T_uav_pos_coreg function. \nIf flown at relative high speed, consider carefully if T_uav_pos_coreg improves the data...")
  }
  # check if length of datetime and SubSecTime, matches with positional data
  if (length(opt_cameras_filtered$Label) == length(datetime) & length(opt_cameras_filtered$Label) == length(SubSecTime)){
    opt_cameras_filtered$datetime <- datetime
    opt_cameras_filtered$SubSecTime <- as.integer(SubSecTime)
  } else {
    stop(paste0("The length of date_time and SubSecTime info is not equal to the positional data \nPlease check if the right map is provided and the same images are used to extract the optimized camerapositions \n"))
  }
  # Now we have alle the necessary info about the optimized camera dataset as dataframe. export to provide in T_uav_pos_coreg function
  return(opt_cameras_filtered)
}

#' Coregister locations with another camera
#'
#' @description
#' `T_uav_pos_coreg()` corrects the GPS-info of the thermal data with more accurate GPS information of a coregistered (high resolution) camera. Could also be that the cameras of this coregistered sensor are already aligned in a structure form motion software.
#'
#' @param thermal_uav Thermal.UAV object retrieved by previous functions (e.g.: `T_uav_info()`).
#' @param opt_cameras (data.frame) containing the optimized camera positions. In the right format, can be prepared with the `T_uav_prep_coreg()` function
#' @param rig_offset (numerical) vector containing the different offsets of the thermal camera  compared to the reference optimized camera with following offsets in mm or ° (x, y, z, yaw, pitch, roll). Default is a vector of zeros
#' @param timediff (numerical) if there is a time difference (in seconds) between the optimized cameraset and the thermal data, it can be specified here, default is 0
#' @return This function returns an updated Thermal.UAV object containing optimized thermal camera positions
#' @export
T_uav_pos_coreg <- function(thermal_uav, opt_cameras = NA, rig_offset = c(0,0,0,0,0,0), timediff = 0){

  # Check class structure of thermal_uav
  if (!isa(thermal_uav, "Thermal.UAV")){
    stop(paste0("The provided parameter thermal_uav is not of class Thermal.UAV \n"))
  }
  if (anyNA(opt_cameras)){
    stop(paste0("opt_cameras not found, please provide opt_cameras, which can be obtained by running the 'T_uav_prep_coreg' function \n"))
  }
  # Check if structure of the opt_camera data.frame is correct for further processing
  names <- c("Label", "x_est", "y_est", "z_est", "yaw_est","pitch_est","roll_est","datetime","SubSecTime")
  if (!all(names(opt_cameras) == names)){
    stop(paste0("The following variables are not found in opt_cameras data.frame: ", names[which((names(opt_cameras) == names) == FALSE)]))
  }
  rownames(opt_cameras) <- 1:nrow(opt_cameras)
  opt_cameras$Label <- as.character(opt_cameras$Label); opt_cameras$x_est <- as.numeric(opt_cameras$x_est); opt_cameras$y_est <- as.numeric(opt_cameras$y_est)
  opt_cameras$z_est <- as.numeric(opt_cameras$z_est); opt_cameras$yaw_est <- as.numeric(opt_cameras$yaw_est); opt_cameras$pitch_est <- as.numeric(opt_cameras$pitch_est)
  opt_cameras$roll_est <- as.numeric(opt_cameras$roll_est); opt_cameras$datetime <- as.POSIXct(opt_cameras$datetime); opt_cameras$SubSecTime <- as.integer(opt_cameras$SubSecTime)
  # Get the numerical datetime with subsecond info
  numtime_opt <- as.numeric(paste(as.numeric(opt_cameras$datetime), opt_cameras$SubSecTime, sep = "."))
  numtime_thermal <- as.numeric(paste(as.numeric(thermal_uav@Info@TTime), thermal_uav@Info@SubSecTime, sep = "."))
  # make dataframe to store the positions of the optimized thermal data
  opt_thermal <- data.frame(matrix(nrow = thermal_uav@Info@dataset_length, ncol = 7))
  names(opt_thermal) <- c("FileName","X", "Y", "Z", "Yaw", "Pitch", "Roll")
  opt_thermal$FileName <- thermal_uav@Info@images
  # get the x and y in meters instead of degrees
  # Get info about the zone & hemisphere
  zone <-  (floor((opt_cameras$x_est[1] + 180) / 6) %% 60) + 1
  if (opt_cameras$y_est[1] > 0){
    hemisphere <- "north"
  } else {
    hemisphere <- "south"
  }
  units <- "m"
  # Get centroids
  xy <- data.frame(opt_cameras$x_est, opt_cameras$y_est)
  names(xy) <- c("X", "Y")
  vect <- terra::vect(xy, geom=c("X", "Y"))
  terra::crs(vect) <- terra::crs("epsg:4326")
  CRSstring <- paste0(
    "+proj=utm +zone=", zone,
    " +ellps=WGS84",
    " +", hemisphere,
    " +units=", units)
  crs <- terra::crs(CRSstring)
  vect_proj <- terra::project(vect, crs)
  opt_cameras$x <- rep(NA, length(opt_cameras$Label))
  opt_cameras$y <- rep(NA, length(opt_cameras$Label))
  for (i in 1:length(opt_cameras$Label)){
    opt_cameras$x[i] <- terra::ext(vect_proj[i])[2]
    opt_cameras$y[i] <- terra::ext(vect_proj[i])[4]
  }
  # estimate positions
  for (i in 1:length(opt_thermal$FileName)) {
    tryCatch({
      opt_thermal$X[i] <- stats::approx(x = numtime_opt, y =  opt_cameras$x,
                                        xout = (numtime_thermal[i] - timediff))$y + rig_offset[1]/1000 # rig offset is in mm, utm in m
      opt_thermal$Y[i] <- stats::approx(x = numtime_opt, y = opt_cameras$y,
                                        xout = (numtime_thermal[i] - timediff))$y + rig_offset[2]/1000
      opt_thermal$Z[i] <- stats::approx(x = numtime_opt, y = opt_cameras$z_est,
                                        xout = (numtime_thermal[i] - timediff))$y + rig_offset[3]/1000
      opt_thermal$Yaw[i] <- stats::approx(x = numtime_opt, y = opt_cameras$yaw_est,
                                          xout = (numtime_thermal[i] - timediff))$y + rig_offset[4]
      opt_thermal$Pitch[i] <- stats::approx(x = numtime_opt, y = opt_cameras$pitch_est,
                                            xout = (numtime_thermal[i] - timediff))$y + rig_offset[5]
      opt_thermal$Roll[i] <- stats::approx(x = numtime_opt, y = opt_cameras$roll_est,
                                           xout = (numtime_thermal[i] - timediff))$y + rig_offset[6]
    }, error = function(e) {})
  }
  # check for NA's -> means
  for (i in which(is.na(opt_thermal))) {
    tryCatch({
      opt_thermal$X[i] <- mean(opt_thermal$X[max(1, i - 5):min(i + 5, length(opt_thermal$X))], na.rm = TRUE)
      opt_thermal$Y[i] <- mean(opt_thermal$Y[max(1, i - 5):min(i + 5, length(opt_thermal$X))], na.rm = TRUE)
      opt_thermal$Z[i] <- mean(opt_thermal$Z[max(1, i - 5):min(i + 5, length(opt_thermal$X))], na.rm = TRUE)
      opt_thermal$Yaw[i] <- mean(opt_thermal$Yaw[max(1, i - 5):min(i + 5, length(opt_thermal$X))], na.rm = TRUE)
      opt_thermal$Pitch[i] <- mean(opt_thermal$Pitch[max(1, i - 5):min(i + 5, length(opt_thermal$X))], na.rm = TRUE)
      opt_thermal$Roll[i] <- mean(opt_thermal$Roll[max(1, i - 5):min(i + 5, length(opt_thermal$X))], na.rm = TRUE)
    }, error = function(e) {})
  }
  # convert back to epsg 4326
  xy_T <- data.frame(opt_thermal$X, opt_thermal$Y)
  names(xy_T) <- c("X", "Y")
  vect_T <- terra::vect(xy_T, geom=c("X", "Y"))
  terra::crs(vect_T) <- crs
  vect_T_proj <- terra::project(vect_T, terra::crs("epsg:4326"))
  opt_thermal$Lon <- rep(NA, length(opt_thermal$X))
  opt_thermal$Lat <- rep(NA, length(opt_thermal$X))
  for (i in 1:length(opt_thermal$X)){
    opt_thermal$Lon[i] <- terra::ext(vect_T_proj[i])[2]
    opt_thermal$Lat[i] <- terra::ext(vect_T_proj[i])[4]
  }
  # ask whether these positions are correct?
  # Plot interactive camera location plot
  terra::plet(vect_T_proj, label = TRUE)
  # Prompt whether these locations are correct
  # res <- svDialogs::dlg_message("Are these cameralocations correct?", "yesno")$res
  #
  # if (res == "no") {
  #   stop(paste0("Camerapositions not correct, issue with matching thermal and multispectral image position"))
  # }
  message("Please check map to confirm if corrected locations are correct. \n")
  # Store in thermal_uav object
  thermal_uav@Position@opt_positions <- opt_thermal
  thermal_uav@Position@rig_offset <- rig_offset
  thermal_uav@Position@timediff <- timediff
  thermal_uav@Position@T_uav_pos_coreg <- "Yes"
  return(thermal_uav)
}
