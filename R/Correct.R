#' Computes atmospheric corrections
#'
#' @description
#' `T_uav_corr_atm()` executes atmospheric corrections. Based on the amount of input different corrections will be done. The equations to estimate the transmission and Ts using background temperature are based on Heinemann et al. 2020 RS https://doi.org/10.3390/rs12071075
#'
#' @param thermal_uav Thermal.UAV object retrieved by previous functions (e.g.: `TheRmalUAV::T_uav_info()`)
#' @param flight_height (numerical) the height between the SENSOR and the OBJECT OF INTEREST. Can be a single value or a vector of the same length as the number of images. If not specified it will search for GPS altitude in the exif/meta data, or use the flight_height calculated in the `T_uav_sensor_pos` function
#' @param T_air (numerical) the air temperature in °C during the flight. Can be a single value, a vector of same length as the number of images or a data.frame containing temperature data ("T_air") and datetime ("datetime") in UTC ("\%d.\%m.\%Y \%H:\%M:\%OS" format). If not provided the mean temperature of the images will be taken
#' @param rel_hum (numerical) the relative humidity in \% during the flight. Can be a single value, a vector of same length as the number of images or a data.frame containing relative humidity data ("rel_hum") and datetime ("datetime") in UTC ("\%d.\%m.\%Y \%H:\%M:\%OS" format). If not provided a value of 50\% is assumed across the entire flight.
#' @param T_bg (numerical) the background temperature in Kelvin, temperature measured from the aluminium panel. If not recorded set to NA and the parameter will not be accounted for.
#' @param emiss (numerical) the emissivity, here by default an average for plants is given (0.985)
#' @seealso [`TheRmalUAV::T_uav_info()`] to get all the necessary information in the right format.
#' @return This function returns an updated Thermal.UAV object containing Thermal.Data which is atmospherically corrected
#' @export
T_uav_corr_atm <- function(thermal_uav, flight_height = NA, T_air = NA, rel_hum = NA, T_bg = 263.55, emiss = 0.985){

  # Check class structure of thermal_uav
  if (!isa(thermal_uav, "Thermal.UAV")){
    stop(paste0("The provided parameter thermal_uav is not of class Thermal.UAV \n"))
  }
  # Reading tiff's ----
  suppressWarnings({
    len <- thermal_uav@Info@dataset_length
    pb <- progress::progress_bar$new(
      format = " Reading tiff's as matrix: [:bar] :percent ETA: :eta",
      total = len
    )
    raw_thermal_list <- list()
    for (i in 1:len) {
      raw_thermal_list[[i]] <- tiff::readTIFF(paste0(thermal_uav@Info@path, thermal_uav@Info@images[i]), as.is = TRUE)
      pb$tick()
    }
    names(raw_thermal_list) <- thermal_uav@Info@images
    # convert raw data to kelvin brightness temperatures ----
    if (thermal_uav@Info@sensor_info$raw_data %in% c("DN", "centikelvin", "kelvin", "celcius")){
      # for images provided in DN
      if (thermal_uav@Info@sensor_info$raw_data == "DN"){
        if (!anyNA(thermal_uav@Info@sensor_info$lin_const) & is.numeric(thermal_uav@Info@sensor_info$lin_const)){
          T_sens_list <- list()
          for (i in 1:len){
            T_sens_list[[i]] <- raw_thermal_list[[i]]*thermal_uav@Info@sensor_info$lin_const
          }
        } else {
          stop(paste0("The linear constant is not provided (correctly), please adjust in the thermal_sensor.csv \n"))
        }
      }
      # For images provided in centikelvin
      else if (thermal_uav@Info@sensor_info$raw_data == "centikelvin"){
        T_sens_list <- list()
        for (i in 1:len){
          T_sens_list[[i]] <- raw_thermal_list[[i]]/100
        }
      }
      # For images provided in kelvin
      else if (thermal_uav@Info@sensor_info$raw_data == "kelvin"){
        T_sens_list <- raw_thermal_list
      }
      # For images provided in celcius
      else if (thermal_uav@Info@sensor_info$raw_data == "celcius"){
        T_sens_list <- list()
        for (i in 1:len){
          T_sens_list[[i]] <- raw_thermal_list[[i]]+273.15
        }
      }
    } else {
      stop(paste0("The 'raw_data' variable does not contain a valid value, please adjust in the thermal_sensor.csv \n"))
    }
  })
  rm(raw_thermal_list)
  gc()
  # Now all the tiffs represent the brightness temperature (@sensor T) in Kelvin
  # Now get all the necessary atm info:
  # Air temperature in °C ----
  if (anyNA(T_air)){
    T_air <- rep(NA, len)
    for (i in 1:len){
      T_air[i] <- mean(T_sens_list[[i]], 0.2)-273.15 # trimmed mean to exclude extreme values
    }
    thermal_uav@Atmosphere@T_air_mode <- 1
  } else if (length(T_air) == 1){
    if (!is.numeric(T_air)){
      stop(paste0("T_air is not numeric, please provide the air temperature a °C \n"))
    }
    T_air <- rep(T_air, len)
    thermal_uav@Atmosphere@T_air_mode <- 2
  } else if (length(T_air) == len){
    if (!is.numeric(T_air)){
      stop(paste0("T_air is not numeric, please provide the air temperature a °C \n"))
    }
    if (anyNA(T_air)){
      stop(paste0("T_air is missing some data points, please update the air temperature dataset \n"))
    }
    T_air <- T_air
    thermal_uav@Atmosphere@T_air_mode <- 3
  } else if (is.data.frame(T_air)){ # In case a separate data frame if given WITH datetime
    # Check if variables are in dataframe
    if (!"T_air" %in% names(T_air)){
      stop(paste0("Variable 'T_air' not found in dataframe \n"))
    }
    if (!"datetime" %in% names(T_air)){
      stop(paste0("Variable 'datetime' not found in dataframe \n"))
    }
    # check if datetime matches our thermal time
    if (!class(T_air$datetime)[1] == "POSIXct"){
      T_air$datetime <- as.POSIXct(T_air$datetime, tryFormats = c("%d.%m.%Y %H:%M:%OS", "%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS",
                                                                  "%Y:%m:%d %H:%M:%OS"), tz = "UTC")
    }
    TTime_start <- thermal_uav@Info@TTime[1]
    TTime_stop <- thermal_uav@Info@TTime[len]
    T_air_start <- T_air$datetime[1]
    T_air_stop <- T_air$datetime[length(T_air$datetime)]
    if (TTime_start < T_air_start | TTime_stop > T_air_stop){
      stop(paste0("T_air data.frame does not cover the whole flight. Reminder: datetime should be in UTC \n"))
    }
    # Get data from T_air corresponding with
    start_id <- max(min(which(T_air$datetime >= (TTime_start)))-1, 1)
    stop_id <- min(max(which(T_air$datetime <= (TTime_stop)))+1, length(T_air$datetime))
    T_air_TT <- T_air[start_id:stop_id, ]
    # Check if the time interval is at 1 sec, if not interpolate for each second
    time_int <- diff(T_air_TT$datetime)
    if (any(time_int >= 300)){
      warning("Please note there is a gap of 5 minutes or more in your weather dataset. \n")
    }
    if (!all(round(as.numeric(time_int), digits = 0) == 1)){
      time_seconds <- as.numeric(T_air_TT$datetime)
      interpol_data <- stats::approx(time_seconds, T_air_TT$T_air, method = "linear", xout = seq(min(time_seconds), max(time_seconds), by = 1))
      T_air_sec <- data.frame(
        datetime = as.POSIXct(interpol_data$x, origin = "1970-01-01", tz = "UTC"),
        T_air = interpol_data$y
      )
    } else {
      T_air_sec <- T_air
    }
    T_air_vector <- rep(NA, len)
    for (i in 1:len){
      T_air_vector[i] <- T_air_sec$T_air[T_air_sec$datetime == thermal_uav@Info@TTime[i]]
    }
    T_air <- T_air_vector
    thermal_uav@Atmosphere@T_air_mode <- 4
  }
  if (min(T_air) > 200){
    warning("Extremely high minimum temperature, please note that the air temperature should be given in °C")
  }
  thermal_uav@Atmosphere@T_air <- T_air
  # Relative Humidity in % ----
  if (anyNA(rel_hum)){
    rel_hum <- rep(50, len)
  } else if (length(rel_hum) == 1){
    if (!is.numeric(rel_hum)){
      stop(paste0("rel_hum is not numeric, please provide the relative humidity in % \n"))
    }
    rel_hum <- rep(rel_hum, len)
  } else if (length(rel_hum) == len){
    if (!is.numeric(rel_hum)){
      stop(paste0("rel_hum is not numeric, please provide the relative humidity in % \n"))
    }
    if (anyNA(rel_hum)){
      stop(paste0("rel_hum is missing some data points, please update the relative humidity dataset \n"))
    }
    rel_hum <- rel_hum
  } else if (is.data.frame(rel_hum)){ # In case a separate data frame if given WITH datetime
    # Check if variables are in dataframe
    if (!"rel_hum" %in% names(rel_hum)){
      stop(paste0("Variable 'rel_hum' no found in dataframe \n"))
    }
    if (!"datetime" %in% names(rel_hum)){
      stop(paste0("Variable 'datetime' no found in dataframe \n"))
    }
    # check if datetime matches our thermal time
    if (!class(rel_hum$datetime)[1] == "POSIXct"){
      rel_hum$datetime <- as.POSIXct(rel_hum$datetime, tryFormats = c("%d.%m.%Y %H:%M:%OS", "%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS",
                                                                      "%Y:%m:%d %H:%M:%OS"), tz = "UTC")
    }
    TTime_start <- thermal_uav@Info@TTime[1]
    TTime_stop <- thermal_uav@Info@TTime[len]
    rel_hum_start <- rel_hum$datetime[1]
    rel_hum_stop <- rel_hum$datetime[length(rel_hum$datetime)]
    if (TTime_start < rel_hum_start | TTime_stop > rel_hum_stop){
      stop(paste0("rel_hum data.frame does not cover the whole flight. Reminder: datetime should be in UTC \n"))
    }
    # Get data from rel_hum corresponding with
    start_id <- max(min(which(rel_hum$datetime >= (TTime_start)))-1, 1)
    stop_id <- min(max(which(rel_hum$datetime <= (TTime_stop)))+1, length(rel_hum$datetime))
    rel_hum_TT <- rel_hum[start_id:stop_id, ]
    # Check if the time interval is at 1 sec, if not interpolate for each second
    time_int <- diff(rel_hum$datetime)
    if (!all(round(as.numeric(time_int), digits = 0) == 1)){
      time_seconds <- as.numeric(rel_hum_TT$datetime)
      interpol_data <- stats::approx(time_seconds, rel_hum_TT$rel_hum, method = "linear", xout = seq(min(time_seconds), max(time_seconds), by = 1))
      rel_hum_sec <- data.frame(
        datetime = as.POSIXct(interpol_data$x, origin = "1970-01-01", tz = "UTC"),
        rel_hum = interpol_data$y
      )
    } else {
      rel_hum_sec <- rel_hum
    }
    rel_hum_vector <- rep(NA, len)
    for (i in 1:len){
      rel_hum_vector[i] <- rel_hum_sec$rel_hum[rel_hum_sec$datetime == thermal_uav@Info@TTime[i]]
    }
    rel_hum <- rel_hum_vector
  }
  if (min(rel_hum) < 0 | max(rel_hum > 100)){
    warning("Unusual relative humidities, please note that the relative humidity should be given in %")
  }
  thermal_uav@Atmosphere@rel_hum <- rel_hum
  # flight_height ----
  if (anyNA(flight_height)){
    flight_height <- thermal_uav@Position@FH
  } else if (length(flight_height) == 1){
    if (!is.numeric(flight_height)){
      stop(paste0("flight_height is not numeric, please provide the flight_height in meter \n"))
    }
    flight_height <- rep(flight_height, len)
  } else if (length(flight_height) == len){
    if (!is.numeric(flight_height)){
      stop(paste0("flight_height is not numeric, please provide the flight_height in meter \n"))
    }
    if (anyNA(flight_height)){
      stop(paste0("flight_height is missing some data points, please update the flight_height vector \n"))
    }
  }
  thermal_uav@Atmosphere@flight_height <- flight_height
  # omega and Tau ----
  omega <- rep(NA, len)
  Tau_atm <- rep(NA, len)
  for (i in 1:len){
    omega[i] <- rel_hum[i] / 100 * exp(6.8455 * 10^-7 * (T_air[i])^3 - 2.7816 * 10^-4 * (T_air[i])^2 + 6.939 * 10^-2 * (T_air[i]) + 1.5587)
    Tau_atm[i] <- 1.9 * exp(-sqrt(flight_height[i]) * (0.0066 - 0.0023 * sqrt(omega[i]))) + (1 - 1.9) * exp(-sqrt(flight_height[i]) * (0.0126 - 0.0067 * sqrt(omega[i])))
  }
  thermal_uav@Atmosphere@omega <- omega
  thermal_uav@Atmosphere@Tau_atm <- Tau_atm
  # emissivity ----
  if (!anyNA(emiss)){
    if (!is.numeric(emiss)){
      stop(paste0("Emissivity is not numeric, please provide emissivity as numerical \n"))
    }
    if (emiss < 0 | emiss > 1){
      stop(paste0("Emissivity needs to be between 0 and 1 \n"))
    }
    emiss <- emiss
  } else {
    emiss <- 0.985
  }
  thermal_uav@Atmosphere@emiss <- emiss
  # Background temperature ----
  if (!anyNA(T_bg)){
    if (!is.numeric(T_bg)){
      stop(paste0("T_bg is not numeric, please provide background temperature as numerical in Kelvin \n"))
    }
    if (T_bg < 200){
      warning(paste0("Background temperature is exceptionally low, keep in mind that background temperature should be provided in Kelvin \n"))
    }
  } else {
    message(paste0("T_bg is not provided, this parameter will not be inclueded in the processing \n"))
    T_bg <- 0
  }
  thermal_uav@Atmosphere@T_bg <- T_bg
  # loop through image list and convert brightness temperature to Ts ----
  pb <- progress::progress_bar$new(
    format = " Converting brightness temperature to surface temperature (Ts): [:bar] :percent ETA: :eta",
    total = len
  )
  Ts_list <- list()
  for (i in 1:len) {
    Ts_list[[i]] <- ((T_sens_list[[i]]^4 - (1 - emiss) * T_bg^4 - (1 - Tau_atm[i]) * (T_air[i] + 273.15)^4) / (emiss * Tau_atm[i]))^(0.25)
    pb$tick()
  }
  names(Ts_list) <- thermal_uav@Info@images
  rm(T_sens_list)
  gc()
  # Return the list with matrices with the thermal data
  thermal_uav@Thermal.Data <- Ts_list
  thermal_uav@Atmosphere@T_uav_corr_atm <- "Yes"
  return(thermal_uav)
}

#' Corrects for emissivity
#'
#' @description
#' `T_uav_corr_emis()` corrects a stitched thermal orthomosaic for emissivity, based on a high resolution landcover or emissivity map.
#'
#' @param thermal_path the path to the thermal map
#' @param thermal_uav the Thermal.UAV object corresponding to this thermal map. Is needed to extract parameters used in `T_uav_corr_atm()`
#' @param temp indicate in which scale the thermal map represents the temperatures. Use "K" for Kelvin and "C" for degree Celcius.
#' @param corrmap_path path to the correction map. This can either be a landcover map with the right labels, an NDVI map or a emissivity map
#' @param method Indicate which variable the correction map provides. Use "LC" when landcover is provided, "NDVI" if you want to use the NDVI method or "EM" when emissivity is provided.
#' @param write_Ts (logical) use TRUE if the corrected thermal map must be written. Default = FALSE
#' @param filename_Ts the path and filename where the corrected thermal map should be stored. If NA, it is written in the thermal path with label "_emis_corr". This variable is only used if `write_Ts` = TRUE
#' @param write_emiss (logical) use TRUE if the emissivity map must be written. Default = FALSE
#' @param filename_emiss the path and filename where the emissivity map should be stored. If NA, it is written in the thermal path with label "_emis". This variable is only used if `write_emiss` = TRUE
#' @param NDVI_veg the NDVI value of vegetation. NDVI values above this threshold will receive an emissivity as defined in `emiss_veg`
#' @param NDVI_soil the NDVI value of bare soil. NDVI values below this threshold will receive an emissivity as defined in `emiss_soil`
#' @param emiss_veg the emissivity for vegetation, will be taken for an NDVI higher than `NDVI_veg`
#' @param emiss_soil the emissivity for bare soil, will be taken for an NDVI lower than `NDVI_soil`
#' @return returns a recalculated thermal map corrected for spatially varying different emissivities.
#' @export
T_uav_corr_emis <- function(thermal_path, thermal_uav, temp = "K", corrmap_path, method = "LC", write_Ts = FALSE, filename_Ts = NA,
                            write_emiss = FALSE, filename_emiss = NA, NDVI_veg = 0.8, NDVI_soil = 0.2, emiss_veg = 0.988, emiss_soil = 0.935){

  # Check if thermal_path is correct
  tryCatch({
    thermal_orig <- terra::rast(thermal_path)
  }, error=function(e){paste0("ERROR : could not read 'thermal_path' \noriginal error: ",conditionMessage(e), " \n")})
  # Check if corrmap_path is correct
  tryCatch({
    corrmap <- terra::rast(corrmap_path)
  }, error=function(e){paste0("ERROR : could not read 'thermal_path' \noriginal error: ",conditionMessage(e), " \n")})
  # Check if method is provided correctly
  if (!(method %in% c("LC", "EM", "NDVI"))){
    stop(paste0("The variabel method is not provided correctly, please use 'LC' for Landcover, 'NDVI' for the NDVI method or 'EM' for emissivity \n"))
  }
  # Check class structure of thermal_uav
  if (!isa(thermal_uav, "Thermal.UAV")){
    stop(paste0("The provided parameter thermal_uav is not of class Thermal.UAV \n"))
  }
  if (length(thermal_uav@Atmosphere@Tau_atm) == 0){
    stop(paste0("Atmosphric information not found in thermal_uav \n"))
  }
  # If Celcius covert to Kelvin
  if (temp == "C"){
    thermal_T <- thermal_orig+273.15
  } else if (temp == "K"){
    thermal_T <- thermal_orig
  } else {
    stop(paste0("The variabel temp is not provided correctly, please use 'K' for Kelvin and 'C' for degrees Celcius \n"))
  }
  rm(thermal_orig)
  # Get variables from thermal_uav
  Tau_atm <- mean(thermal_uav@Atmosphere@Tau_atm)
  T_air <- mean(thermal_uav@Atmosphere@T_air)+273.15
  emiss_orig <- thermal_uav@Atmosphere@emiss
  T_bg <- thermal_uav@Atmosphere@T_bg
  # Calculate back to brightness temperature at sensor
  T_sens <- ((emiss_orig*Tau_atm)*thermal_T^4 + (1-emiss_orig)*Tau_atm*T_bg^4 + (1-Tau_atm)*T_air^4)^(0.25)
  # check if thermal_T and corrmap have the same geometrics
  if (terra::crs(thermal_T) != terra::crs(corrmap)){
    message("Reprojecting corrmap to crs of thermal map")
    corrmap <- terra::project(corrmap, terra::crs(thermal_T))
  }
  # Check for extents:
  message("Checking if extents match")
  corrmap_check_crs <- terra::project(corrmap, "epsg:3857")
  thermal_T_check_crs <- terra::project(thermal_T, "epsg:3857")
  ext_diff <- c(terra::ext(thermal_T_check_crs)[1] - terra::ext(corrmap_check_crs)[1],
                terra::ext(thermal_T_check_crs)[2] - terra::ext(corrmap_check_crs)[2],
                terra::ext(thermal_T_check_crs)[3] - terra::ext(corrmap_check_crs)[3],
                terra::ext(thermal_T_check_crs)[4] - terra::ext(corrmap_check_crs)[4])
  if ((ext_diff[1] < -1) | (ext_diff[2] > 1) | (ext_diff[3] < -1) | (ext_diff[4] > 1)){
    stop(paste0("Extent of corrmap does not match with the extent of thermal map \n"))
  }
  if (terra::res(thermal_T)[1] != terra::res(corrmap)[1] | terra::res(thermal_T)[2] != terra::res(corrmap)[2]){
    message("Resampling corrmap to match thermal map")
    if (method == "NDVI" | method == "EM"){
      corrmap <- terra::resample(corrmap, thermal_T, method = "bilinear")
    } else {
      corrmap <- terra::resample(corrmap, thermal_T, method = "near")
    }
  }
  # Now everything should be ready to convert the thermal data
  # METHOD 1: NDVI
  if (method == "NDVI"){
    # check if NDVI values and emissivity is between 0 and 1.
    if (NDVI_veg > 1 | NDVI_veg < 0){
      stop(paste0("NDVI_veg should be between 0 and 1 \n"))
    }
    if (NDVI_soil > 1 | NDVI_soil < 0){
      stop(paste0("NDVI_soil should be between 0 and 1 \n"))
    }
    if (emiss_veg > 1 | emiss_veg < 0){
      stop(paste0("emiss_veg should be between 0 and 1 \n"))
    }
    if (emiss_soil > 1 | emiss_soil < 0){
      stop(paste0("emiss_soil should be between 0 and 1 \n"))
    }
    # Get emissivity map based on NDVI method
    rcl <- matrix(c(-1, NDVI_veg, NDVI_soil, NDVI_soil, 1, NDVI_veg, emiss_soil, emiss_veg, 0), nrow = 3)
    emiss_veg_soil <- terra::classify(corrmap, rcl, include.lowest = TRUE) # emiss for veg and soil
    Pv <- ((corrmap-NDVI_soil)/(NDVI_veg-NDVI_soil))^2
    C <- 4*0.01*Pv*(1-Pv)
    Pv_emiss <- emiss_veg*Pv + emiss_soil*(1-Pv) + C
    mixed_emiss <- terra::mask(Pv_emiss, emiss_veg_soil, inverse = TRUE, maskvalues = 0, updatevalue = 0) # emiss for mixed
    emiss <- emiss_veg_soil + mixed_emiss # total emissivity map
    # Now go back from brightness tempertaure at sensor to LST
    Ts <- ((T_sens^4 - (1 - emiss) * T_bg^4 - (1 - Tau_atm) * (T_air)^4) / (emiss * Tau_atm))^(0.25)
  }
  # Method 2: Emissivity map
  if (method == "EM"){
    # Check if emissivity map is between 0 and 1
    if (terra::minmax(corrmap)[1] < 0 | terra::minmax(corrmap)[2] > 1){
      stop(paste0("The emissivity map should contain values between 0 and 1 \n"))
    }
    Ts <- ((T_sens^4 - (1 - corrmap) * T_bg^4 - (1 - Tau_atm) * (T_air)^4) / (corrmap * Tau_atm))^(0.25)
  }
  # Method 3: Landcover

  ##############################################################################

  # INSERT part over the landcover

  ##############################################################################

  # If temperature was given in degree celcius, convert back.
  if (temp == "C"){
    Ts <- Ts - 273.15
  }
  # Now we have the corrected Ts => write_Ts it
  if (write_Ts == TRUE){
    if (is.na(filename_Ts)){
      export_path <- paste0(substr(thermal_path, 1, nchar(thermal_path)-4), "_emis_corr.tif")
    } else {
      export_path <- filename_Ts
    }
    tryCatch({
      terra::writeRaster(Ts, export_path, overwrite = TRUE)
    }, error=function(e){cat("ERROR : corrected thermal map not saved \noriginal error:",conditionMessage(e), "\n")})
  }
  if (write_emiss == TRUE){
    if (is.na(filename_emiss)){
      export_path <- paste0(substr(thermal_path, 1, nchar(thermal_path)-4), "_emiss.tif")
    } else {
      export_path <- filename_emiss
    }
    tryCatch({
      terra::writeRaster(emiss, export_path, overwrite = TRUE)
    }, error=function(e){cat("ERROR : emissivity map not saved \noriginal error:",conditionMessage(e), "\n")})
  }
  return(Ts)
}

#' Smooths thermal data
#'
#' @description
#' `T_uav_corr_smooth()` smooths the thermal dataset to avoid large fluctuations. Two methods are possible: (i) based on a provided high temporal resolution air temperature dataset or (ii) empirical smoothing using the temperatures of the images.
#'
#' @param thermal_uav Thermal.UAV object retrieved by previous functions (e.g.: `T_uav_info()`). Note in order to smooth the thermal data, T_uav_corr_atm should be ran at first
#' @param method One of the two methods: (i) "T_air" the dataset is smoothed based on a high resolution air temperature dataset or (ii) "image" smoothing is done based on the average image temperature, default is "image"
#' @param T_air For method "T_air": the air temperature in °C during the flight. Can be a vector of same length as the number of images or a data.frame containing temperature data ("T_air") and datetime ("datetime") in UTC ("\%d.\%m.\%Y \%H:\%M:\%OS" format). If already provided in `T_uav_corr_atm`, you can leave it black here. Default is NA
#' @param smooth_length (numerical) is the amount of images that will be taken into account to smooth the temperature. Default is NA where the smooth length is the number of overlapping images.
#' @return This function returns an updated Thermal.UAV object with smoothed Thermal.Data
#' @export
T_uav_corr_smooth <- function(thermal_uav, method = "image", T_air = NA, smooth_length = NA){

  # Check class structure of thermal_uav
  if (!isa(thermal_uav, "Thermal.UAV")){
    stop(paste0("The provided parameter thermal_uav is not of class Thermal.UAV \n"))
  }
  # check if Thermal.Data is available
  if (length(thermal_uav@Thermal.Data) == 0){
    stop(paste0("Thermal.Data not found in thermal_uav, please first run the T_uav_corr_atm function \n"))
  }
  # Smooth: choose method
  if (method == "image"){
    # check if the mean temperatures per tiff are already calculated:
    if (thermal_uav@Atmosphere@T_air_mode != 1){
      T_air <- rep(NA, thermal_uav@Info@dataset_length)
      for (i in 1:thermal_uav@Info@dataset_length){
        T_air[i] <- mean(thermal_uav@Thermal.Data[[i]], 0.2)-273.15 # trimmed mean to exclude extreme values
      }
    } else {
      T_air <- thermal_uav@Atmosphere@T_air
    }
  } else if (method == "T_air"){
    if (anyNA(T_air)){
      if (thermal_uav@Atmosphere@T_air_mode == 3 | thermal_uav@Atmosphere@T_air_mode == 4){
        T_air <- thermal_uav@Atmosphere@T_air
      } else {
        stop(paste0("No air temperature dataset provided, please provide a correct T_air dataset \n"))
      }
    } else if (length(T_air) == thermal_uav@Info@dataset_length){
      if (!is.numeric(T_air)){
        stop(paste0("T_air is not numeric, please provide the air temperature a °C \n"))
      }
      if (anyNA(T_air)){
        stop(paste0("T_air is missing some data points, please update the air temperature dataset \n"))
      }
      T_air <- T_air
    } else if (is.data.frame(T_air)){ # In case a separate data frame if given WITH datetime
      # Check if variables are in dataframe
      if (!"T_air" %in% names(T_air)){
        stop(paste0("Variable 'T_air' no found in dataframe \n"))
      }
      if (!"datetime" %in% names(T_air)){
        stop(paste0("Variable 'datetime' no found in dataframe \n"))
      }
      # check if datetime matches our thermal time
      if (!class(T_air$datetime)[1] == "POSIXct"){
        T_air$datetime <- as.POSIXct(T_air$datetime, tryFormats = c("%d.%m.%Y %H:%M:%OS", "%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS",
                                                                    "%Y:%m:%d %H:%M:%OS"), tz = "UTC")
      }
      TTime_start <- thermal_uav@Info@TTime[1]
      TTime_stop <- thermal_uav@Info@TTime[thermal_uav@Info@dataset_length]
      T_air_start <- T_air$datetime[1]
      T_air_stop <- T_air$datetime[length(T_air$datetime)]
      if (TTime_start < T_air_start | TTime_stop > T_air_stop){
        stop(paste0("T_air data.frame does not cover the whole flight. Reminder: datetime should be in UTC \n"))
      }
      # Get data from T_air corresponding with images
      start_id <- max(min(which(T_air$datetime >= (TTime_start)))-1, 1)
      stop_id <- min(max(which(T_air$datetime <= (TTime_stop)))+1, length(T_air$datetime))
      T_air_TT <- T_air[start_id:stop_id, ]
      # Check if the time interval is at 1 sec, if not interpolate for each second
      time_int <- diff(T_air$datetime)
      if (!all(round(as.numeric(time_int), digits = 0) == 1)){
        time_seconds <- as.numeric(T_air_TT$datetime)
        interpol_data <- stats::approx(time_seconds, T_air_TT$T_air, method = "linear", xout = seq(min(time_seconds), max(time_seconds), by = 1))
        T_air_sec <- data.frame(
          datetime = as.POSIXct(interpol_data$x, origin = "1970-01-01", tz = "UTC"),
          T_air = interpol_data$y
        )
      } else {
        T_air_sec <- T_air
      }
      T_air_vector <- rep(NA, thermal_uav@Info@dataset_length)
      for (i in 1:thermal_uav@Info@dataset_length){
        T_air_vector[i] <- T_air_sec$T_air[T_air_sec$datetime == thermal_uav@Info@TTime[i]]
      }
      T_air <- T_air_vector
    } else {
      stop(paste0("Unvalid T_air format. Please provide T_air as described \n"))
    }
  } else {
    stop(paste0("No valid method was entered \n"))
  }
  # Now perform smoothing
  if (!anyNA(smooth_length)){
    if (!is.numeric(smooth_length)){
      stop(paste0("smooth_length is not numeric \n"))
    }
    smooth_length <-  2*ceiling(smooth_length/2)
  } else {
    # Calculate what the smoothing length must be, need overlap
    if (length(thermal_uav@Position@extents_vector) == 0){
      thermal_uav <- T_uav_sensor_pos(thermal_uav, extent = TRUE, overlap = TRUE, export = FALSE)
    } else if (length(thermal_uav@Position@overlap) == 0 | thermal_uav@Position@overlap == 0){
      len <- thermal_uav@Info@dataset_length-1
      overlap <- rep(NaN, len)
      pb <- progress::progress_bar$new(
        format = "Calculating overlap (needed to calculate smooth_length): [:bar] :percent ETA: :eta",
        total = len
      )
      for (i in 1:len){
        intersect <-  terra::intersect(thermal_uav@Position@extents_vector[i], thermal_uav@Position@extents_vector[(i+1)])
        area_ext <- terra::expanse(thermal_uav@Position@extents_vector[i], unit="m")
        area_int <- terra::expanse(intersect)
        overlap[i] <- area_int/area_ext
        pb$tick()
      }
      thermal_uav@Position@overlap <- mean(overlap)
    }
    # Calculate how many overlapping images there are
    smooth_length <- 2*ceiling(1/(1-thermal_uav@Position@overlap))
  }
  T_Smooth <- rep(NaN, thermal_uav@Info@dataset_length)
  for (i in 1:length(T_Smooth)){
    start_index <- max(1, i - smooth_length * 0.5)
    end_index <- min(length(T_Smooth), i + smooth_length * 0.5)

    T_Smooth[i] <- mean(T_air[start_index:end_index], na.rm = TRUE)
  }
  # Correction for each image
  T_Corr = mean(T_Smooth, na.rm = TRUE) - T_Smooth
  pb <- progress::progress_bar$new(
    format = " Performing smoothing correction: [:bar] :percent ETA: :eta",
    total = length(T_Corr)
  )
  # Correct all the images
  for (i in length(T_Corr)){
    thermal_uav@Thermal.Data[[i]] <- thermal_uav@Thermal.Data[[i]] + T_Corr[i]
    pb$tick()
  }
  # end of funtion
  thermal_uav@Smooth@T_smooth <- T_Corr
  thermal_uav@Smooth@smooth_length <- smooth_length
  thermal_uav@Smooth@method <- method
  thermal_uav@Smooth@T_uav_corr_smooth <- "Yes"
  return(thermal_uav)
}

#' Atmospheric and emissivity correction for othomosaics
#'
#' @description
#' `T_uav_corr_ortho()` executes atmospheric and emissivity corrections on already stitched maps.
#' This function can be used if only single values for the atmospheric parameters collected or when data from integrated sensors have difficulties with stitching thermal data separately.
#' Within flight variations are not captured in this function.
#'
#' @param thermal_ortho Map (SpatRaster) or path to geotiff file (chr) containing brightness temperatures
#' @param temp Indicate in which scale the `thermal_ortho` map represents the temperatures. Use "K" for Kelvin and "C" for degree Celcius.
#' @param flight_height (numerical) the height between the SENSOR and the OBJECT OF INTEREST expressed in meters.
#' @param T_air (numerical) the air temperature in °C during the flight. If NA it will use a trimmed mean of the temperatures in the `thermal_ortho` map.
#' @param rel_hum (numerical) the relative humidity in \% during the flight. If not provided a value of 50\% is assumed.
#' @param T_bg (numerical) the background temperature in Kelvin, temperature measured from the aluminium panel. If not recorded set to NA and the parameter will not be accounted for.
#' @param emiss (numerical) the emissivity, here by default an average for plants is given (0.985)
#' @param method Indicate which variable the correction map provides. Use "LC" when landcover is provided, "NDVI" if you want to use the NDVI method or "EM" when emissivity is provided. Set to NA if no spatial information on the emissivity can be calculated.
#' @param corrmap Map (SpatRaster) or path to geotiff file (chr) for emissivity correction. This can either be a landcover map with the right labels, an NDVI map or a emissivity map.
#' @param return_emis (logical) when set to TRUE, the calcualted emissivty map is returned together with the surface temperature as raster stack
#' @param NDVI_veg the NDVI value of vegetation. NDVI values above this threshold will receive an emissivity as defined in `emiss_veg`
#' @param NDVI_soil the NDVI value of bare soil. NDVI values below this threshold will receive an emissivity as defined in `emiss_soil`
#' @param emiss_veg the emissivity for vegetation, will be taken for an NDVI higher than `NDVI_veg`
#' @param emiss_soil the emissivity for bare soil, will be taken for an NDVI lower than `NDVI_soil`
#' @return returns a thermal map with atmospherically corrected temperatures, and accounted for (spatially varying) emissivity.
#' @export
T_uav_corr_ortho <- function(thermal_ortho = thermal_ortho,
                             temp = "C",
                             flight_height = NA, # In meters
                             T_air = NA, # In °C
                             rel_hum = NA, # In %
                             T_bg = 263.55, # In Kelvin
                             emiss = 0.985, # 0-1
                             method = NA, # NDVI?
                             corrmap = NA,
                             return_emis = NA,
                             NDVI_veg = 0.8,
                             NDVI_soil = 0.2,
                             emiss_veg = 0.988,
                             emiss_soil = 0.935){

  # Check if thermal_ortho is correct
  if (is.character(thermal_ortho)){
    tryCatch({
      thermal_orig <- terra::rast(thermal_path)
    }, error=function(e){paste0("ERROR : could not read 'thermal_path' \noriginal error: ",conditionMessage(e), " \n")})
  } else if (class(thermal_ortho)[1] == "SpatRaster"){
    thermal_orig <- thermal_ortho
  } else {
    stop(paste0("thermal_ortho should either be a path to a tif or a SpatRaster class from the terra package \n"))
  }
  # Check if tempertaure is provided in Kelvin or degrees Celcius
  if (temp == "C"){
    thermal_K <- thermal_orig + 273.15
  } else if (temp == "K"){
    thermal_K <-  thermal_orig
  } else {
    stop(paste0("Invalid value for temp, should be either 'C' for degrees Celcius or 'K' for Kelvin \n"))
  }
  # Check if flightheight is numeric
  if (is.na(flight_height)){
    stop(paste0("flight_heighth not found, please provide flight_height in meters \n"))
  } else {
    tryCatch({
      FH <- as.numeric(flight_height)
    }, error=function(e){paste0("ERROR : could not convert flight_height to numeric \noriginal error: ",conditionMessage(e), " \n")})
  }
  # Check T_air
  if (is.numeric(T_air) | is.integer(T_air)){
    T_air <- as.numeric(T_air)
    if (T_air > 100){
      warning("T_air extremely high, please note T_air should be provided in degrees Celcius \n")
    }
  } else if (is.na(T_air)){
    mean(thermal_K, 0.3)-273.15
  } else {
    stop(paste0("Invalid value for T_air, please provide air temperature in degrees Celcius or set as NA \n"))
  }
  T_air_C <- T_air
  T_air_K <- T_air + 273.15
  # Check rel_hum
  if (is.na(rel_hum)){
    rel_hum <- 50
  } else if (is.numeric(rel_hum) | is.integer(rel_hum)){
    rel_hum <- as.numeric(rel_hum)
  } else {
    stop(paste0("Invalid value for rel_hum, please provide relative humidity in % or set as NA \n"))
  }
  if (rel_hum < 0 | rel_hum > 100){
    stop(paste0("Invalid value for rel_hum, please provide relative humidity in % or set as NA \n"))
  }
  # Check for Background temperatures
  if (!anyNA(T_bg)){
    if (!is.numeric(T_bg)){
      stop(paste0("T_bg is not numeric, please provide background temperature as numerical in Kelvin \n"))
    }
    if (T_bg < 200){
      warning(paste0("Background temperature is exceptionally low, keep in mind that background temperature should be provided in Kelvin \n"))
    }
  } else {
    message(paste0("T_bg is not provided, this parameter will not be inclueded in the processing \n"))
    T_bg <- 0
  }
  # Check for emissivity
  if (is.na(emiss)){
    emiss <- 0.985
  } else if (is.numeric(emiss)){
    emiss <- emiss
    if (emiss < 0 | emiss > 1){
      stop(paste0("Emissivity needs to be between 0 and 1 \n"))
    }
  } else {
    stop(paste0("Invalid value for emissivity, provide numeric values betwen 0 and 1 or set to NA \n"))
  }
  # Check method
  if (!(method %in% c("NDVI", "EM", "LC", NA))){
    stop("Invalid value for method, choose either 'NDVI', 'EM', 'LC', or NA if emissivity is not available as map \n")
  }
  if (is.na(method)){
    emiss_map_method <- FALSE
  }
  if (!is.na(method)){
    emiss_map_method <- TRUE
    # Check if corrmap can be loaded
    if (class(corrmap)[1] == "SpatRaster"){
      corrmap <- corrmap
    } else if (is.character(corrmap)){
      tryCatch({
        corrmap <- terra::rast(corrmap_path)
      }, error=function(e){paste0("ERROR : could not read 'corrmap' as path, if you want to provide corrmap as a path, please check your file path again  \noriginal error: ",conditionMessage(e), " \n")})
    }
  }

  # Atmospheric Parameters

  omega <- rel_hum / 100 * exp(6.8455 * 10^-7 * (T_air_C)^3 - 2.7816 * 10^-4 * (T_air_C)^2 + 6.939 * 10^-2 * (T_air_C) + 1.5587)
  Tau_atm <- 1.9 * exp(-sqrt(FH) * (0.0066 - 0.0023 * sqrt(omega))) + (1 - 1.9) * exp(-sqrt(FH) * (0.0126 - 0.0067 * sqrt(omega)))

  # Emissivity

  if (emiss_map_method == FALSE){
    Ts <- ((thermal_K^4 - (1 - emiss) * T_bg^4 - (1 - Tau_atm) * (T_air_K)^4) / (emiss * Tau_atm))^(0.25)
    if (temp == "C"){
      Ts <- Ts - 273.15
    }
  } else if (emiss_map_method <- TRUE){
    # check if thermal_K and corrmap have the same geometrics
    if (terra::crs(thermal_K) != terra::crs(corrmap)){
      message("Reprojecting corrmap to crs of thermal map")
      corrmap <- terra::project(corrmap, terra::crs(thermal_K))
    }
    # Check for extents:
    message("Checking if extents match")
    corrmap_check_crs <- terra::project(corrmap, "epsg:3857")
    thermal_K_check_crs <- terra::project(thermal_K, "epsg:3857")
    ext_diff <- c(terra::ext(thermal_K_check_crs)[1] - terra::ext(corrmap_check_crs)[1],
                  terra::ext(thermal_K_check_crs)[2] - terra::ext(corrmap_check_crs)[2],
                  terra::ext(thermal_K_check_crs)[3] - terra::ext(corrmap_check_crs)[3],
                  terra::ext(thermal_K_check_crs)[4] - terra::ext(corrmap_check_crs)[4])
    if ((ext_diff[1] < -1) | (ext_diff[2] > 1) | (ext_diff[3] < -1) | (ext_diff[4] > 1)){
      stop(paste0("Extent of corrmap does not match with the extent of thermal map \n"))
    }
    if (terra::res(thermal_K)[1] != terra::res(corrmap)[1] | terra::res(thermal_K)[2] != terra::res(corrmap)[2]){
      message("Resampling corrmap to match thermal map")
      if (method == "NDVI" | method == "EM"){
        corrmap <- terra::resample(corrmap, thermal_K, method = "bilinear")
      } else {
        corrmap <- terra::resample(corrmap, thermal_K, method = "near")
      }
    }

    # Now everything should be ready to convert the thermal data
    # METHOD 1: NDVI
    if (method == "NDVI"){
      # check if NDVI values and emissivity is between 0 and 1.
      if (NDVI_veg > 1 | NDVI_veg < 0){
        stop(paste0("NDVI_veg should be between 0 and 1 \n"))
      }
      if (NDVI_soil > 1 | NDVI_soil < 0){
        stop(paste0("NDVI_soil should be between 0 and 1 \n"))
      }
      if (emiss_veg > 1 | emiss_veg < 0){
        stop(paste0("emiss_veg should be between 0 and 1 \n"))
      }
      if (emiss_soil > 1 | emiss_soil < 0){
        stop(paste0("emiss_soil should be between 0 and 1 \n"))
      }
      # Get emissivity map based on NDVI method
      rcl <- matrix(c(-1, NDVI_veg, NDVI_soil, NDVI_soil, 1, NDVI_veg, emiss_soil, emiss_veg, 0), nrow = 3)
      emiss_veg_soil <- terra::classify(corrmap, rcl, include.lowest = TRUE) # emiss for veg and soil
      Pv <- ((corrmap-NDVI_soil)/(NDVI_veg-NDVI_soil))^2
      C <- 4*0.01*Pv*(1-Pv)
      Pv_emiss <- emiss_veg*Pv + emiss_soil*(1-Pv) + C
      mixed_emiss <- terra::mask(Pv_emiss, emiss_veg_soil, inverse = TRUE, maskvalues = 0, updatevalue = 0) # emiss for mixed
      emiss <- emiss_veg_soil + mixed_emiss # total emissivity map
      # Now go back from brightness tempertaure at sensor to LST
      Ts <- ((thermal_K^4 - (1 - emiss) * T_bg^4 - (1 - Tau_atm) * (T_air_K)^4) / (emiss * Tau_atm))^(0.25)
    }
    # Method 2: Emissivity map
    if (method == "EM"){
      # Check if emissivity map is between 0 and 1
      if (terra::minmax(corrmap)[1] < 0 | terra::minmax(corrmap)[2] > 1){
        stop(paste0("The emissivity map should contain values between 0 and 1 \n"))
      }
      Ts <- ((thermal_K^4 - (1 - corrmap) * T_bg^4 - (1 - Tau_atm) * (T_air_K)^4) / (corrmap * Tau_atm))^(0.25)
    }
    # Method 3: Landcover

    ##############################################################################

    # INSERT part over the landcover

    ##############################################################################

    # If temperature was given in degree celcius, convert back.
    if (temp == "C"){
      Ts <- Ts - 273.15
    }
  }
  names(Ts) <- "Ts"
  if (emiss_map_method == TRUE & return_emis == TRUE){
    Ts <-  terra::rast(list(Ts, emiss))
    names(Ts) <- c("Ts", "Emiss")
  }
  return(Ts)
}
