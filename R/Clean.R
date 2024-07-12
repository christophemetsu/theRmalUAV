# #' Estimates image sharpness
# #'
# #' @description
# #' `T_uav_est_sharp()` estimates sharpness of the thermal images.
# #'
# #' @param thermal_uav Thermal.UAV object retrieved by previous functions (e.g.: `T_uav_info()`). If Thermal.Data is empty, the raw TIFFs are loaded directly from the folder containing the tiff files.
# #' @return This function returns an updated Thermal.UAV object containing a vector with the sharpness for each image.
T_uav_est_sharp <- function(thermal_uav){

  suppressWarnings({
    # Check class structure of thermal_uav
    if (!isa(thermal_uav, "Thermal.UAV")){
      stop(paste0("The provided parameter thermal_uav is not of class Thermal.UAV \n"))
    }
    # Thermal TSharp: compile sharpness info about all the tiffs
    len <- thermal_uav@Info@dataset_length
    TSharp <- rep(NA, len)
    if (length(thermal_uav@Thermal.Data) != 0){
      if (length(thermal_uav@Thermal.Data) != len){
        stop(paste0("Length of Thermal.Data is not equal to the length of images \n"))
      }
    }
    pb <- progress::progress_bar$new(
      format = " Estimating image sharpness: [:bar] :percent ETA: :eta",
      total = len
    )
    for (i in 1:len) {
      if (length(thermal_uav@Thermal.Data) == 0){
        matrix <- tiff::readTIFF(paste0(thermal_uav@Info@path, thermal_uav@Info@images[i]), as.is = TRUE)
      } else {
        matrix <- thermal_uav@Thermal.Data[[i]]
      }
      # Calculate gradient
      siz <- dim(matrix)
      # Gradient over first dimension
      Gy <- matrix(0, nrow = siz[1], ncol = siz[2], dimnames = dimnames(matrix))
      n <- siz[1]
      # Take forward differences on left and right edges
      if (n > 1) {
        Gy[1,] <- matrix[2,] - matrix[1,]
        Gy[n,] <- matrix[n,] - matrix[n-1,]
      }
      # Take centered difference on interior points
      if (n > 2) {
        Gy[2:(n-1),] <- matrix[3:n,] - matrix[1:(n-2),]
      }
      # Gradient over second dimension
      Gx <- matrix(0, nrow = siz[1], ncol = siz[2], dimnames = dimnames(matrix))
      n <- siz[2]
      # Take forward differences on left and right edges
      if (n > 1) {
        Gx[, 1] <- matrix[, 2] - matrix[, 1]
        Gx[, n] <- matrix[, n] - matrix[, n-1]
      }
      # Take centered differences on interior points
      if (n > 2) {
        Gx[, 2:(n-1)] <- matrix[, 3:n] - matrix[, 1:(n-2)]
      }
      # Calculate normalized sharpness
      Gx_abs <- abs(Gx)
      Gy_abs <- abs(Gy)
      min <- min(Gx_abs, Gy_abs)
      max <- max(Gx_abs, Gy_abs)
      Gx_norm <- (Gx_abs - min)/(max - min)
      Gy_norm <- (Gy_abs - min)/(max - min)
      S <- sqrt(Gx_norm^2 + Gy_norm^2)
      TSharp[i] <- 5* sum(S) / length(Gx_norm)
      pb$tick()
    }
  })
  thermal_uav@Sharpness@Tsharp <- TSharp
  return(thermal_uav)
}

#' Get sharpness threshold
#'
#' @description
#' `T_uav_clean_thresh()` estimates sharpness by iteratively asking whether an image is sharp enough, until it is sharp enough. The goal is to set your own sharpness threshold which is used in the image reduction function.
#'
#' @param thermal_uav Thermal.UAV object retrieved by previous functions (e.g.: `T_uav_info()`). If Thermal.Data is empty, the raw TIFFs are loaded directly from the folder containing the tiff files.
#' @return This function returns a sharpness value which can be set as threshold in the `T_uav_clean_reduc()` function
#' @export
T_uav_clean_thresh <- function(thermal_uav){

  suppressWarnings({
    # Check class structure of thermal_uav
    if (!isa(thermal_uav, "Thermal.UAV")){
      stop(paste0("The provided parameter thermal_uav is not of class Thermal.UAV \n"))
    }
    # Check if sharpness is already calculated
    if (length(thermal_uav@Sharpness@Tsharp) == 0){
      thermal_uav <- T_uav_est_sharp(thermal_uav)
      TSharp <- thermal_uav@Sharpness@Tsharp
    } else {
      TSharp <- thermal_uav@Sharpness@Tsharp
    }
    # show images and prompt whether it is sharp enough
    if (length(thermal_uav@Thermal.Data) == 0){
      answ <- "no"
      Q <- 3
      while (answ == "no") {
        warning("off")
        Thresh <- stats::quantile(TSharp, probs = Q/100, na.rm = TRUE)
        IDEdge <- which.min(abs(TSharp - Thresh))
        rast <- terra::rast(paste0(thermal_uav@Info@path, thermal_uav@Info@images[IDEdge]))
        grDevices::dev.new()
        terra::plot(rast)
        answ <- svDialogs::dlg_message("Is this image sharp enough?", "yesno")$res
        grDevices::dev.off()
        if (answ == "no") {
          Q <- Q + 1
        }
      }
    } else {
      answ <- "no"
      Q <- 3
      while (answ == "no") {
        warning("off")
        Thresh <- stats::quantile(TSharp, probs = Q/100, na.rm = TRUE)
        IDEdge <- which.min(abs(TSharp - Thresh))
        rast <- terra::rast(thermal_uav@Thermal.Data[[IDEdge]])
        grDevices::dev.new()
        terra::plot(rast)
        answ <- svDialogs::dlg_message("Is this image sharp enough?", "yesno")$res
        grDevices::dev.off()
        if (answ == "no") {
          Q <- Q + 1
        }
      }
    }
  })
  return(Thresh)
}

#' Keeps the best image per second
#'
#' @description
#' `T_uav_clean_persec()` ranks all images within 1 second based on sharpness and keeps x sharpest images. This function is primarily for streaming/filming thermal cameras like the ThermalCapture. The goal is to reduce the number of images before further processing.
#'
#' @param thermal_uav Thermal.UAV object retrieved by previous functions (e.g.: `T_uav_info()`)
#' @param number_keep (numerical) the number of images you want to keep per second (default = 1)
#' @param remove (logical) use TRUE to delete the images for your local disk space, default is FALSE where images are simple replaced in a subfolder called "Removed by img_persec"
#' @return an updated Thermal.UAV object and (re)moves the least sharp images
#' @export
T_uav_clean_persec <- function(thermal_uav, number_keep = 1, remove = FALSE){

  suppressWarnings({
    # Check class structure of thermal_uav
    if (!isa(thermal_uav, "Thermal.UAV")){
      stop(paste0("The provided parameter thermal_uav is not of class Thermal.UAV \n"))
    }
    thermal_uav@Sharpness@number_keep <- number_keep
    # Check if sharpness is already calculated
    if (length(thermal_uav@Sharpness@Tsharp) == 0){
      thermal_uav <- T_uav_est_sharp(thermal_uav)
      TSharp <- thermal_uav@Sharpness@Tsharp
    } else {
      TSharp <- thermal_uav@Sharpness@Tsharp
    }
    # Scan the number of images taken per sec and order images per second according to sharpness
    start_no_images <- thermal_uav@Info@dataset_length
    len <- thermal_uav@Info@dataset_length
    UnTT <- base::unique(thermal_uav@Info@TTime)
    KeepT <- integer(len)
    # sharpness$TTime_corr <- rep(NA, len)
    for (i in seq_along(UnTT)) {
      Id <- which(UnTT[i] == thermal_uav@Info@TTime)
      KeepId <- base::order(TSharp[Id], decreasing = TRUE)[1:min(number_keep,length(Id))] # Cannot select more images than there are available
      KeepT[Id[KeepId]] <- 1
    }
    # Remove the unsharp images and continue working with the sharp images only.
    # Alter the lengths of all variables
    Rem <- thermal_uav@Info@images[KeepT == 0]
    # Info
    thermal_uav@Info@images <- thermal_uav@Info@images[KeepT == 1]
    thermal_uav@Info@TTime <- thermal_uav@Info@TTime[KeepT == 1]
    if (length(thermal_uav@Info@SubSecTime) != 0){
      thermal_uav@Info@SubSecTime <- thermal_uav@Info@SubSecTime[KeepT == 1]
    }
    thermal_uav@Info@dataset_length <- length(which(KeepT == 1))
    thermal_uav@Info@exif <- thermal_uav@Info@exif[KeepT == 1, ]
    if (length(thermal_uav@Info@meta_df) != 0){
      thermal_uav@Info@meta_df <- thermal_uav@Info@meta_df[KeepT == 1, ]
      df <- thermal_uav@Info@meta_df
      df$FileName <- thermal_uav@Info@images
      utils::write.csv(df, paste0(thermal_uav@Info@path, "metadata_update.csv"), sep = ",", row.names = FALSE)
    }
    # Sharpness
    thermal_uav@Sharpness@Tsharp <- thermal_uav@Sharpness@Tsharp[KeepT == 1]
    # Position
    if (length(thermal_uav@Position@Lat) != 0){
      thermal_uav@Position@Lat <- thermal_uav@Position@Lat[KeepT == 1]
    }
    if (length(thermal_uav@Position@Lon) != 0){
      thermal_uav@Position@Lon <- thermal_uav@Position@Lon[KeepT == 1]
    }
    thermal_uav@Position@FH <- thermal_uav@Position@FH[KeepT == 1]
    if (length(thermal_uav@Position@Yaw) != 0){
      thermal_uav@Position@Yaw <- thermal_uav@Position@Yaw[KeepT == 1]
    }
    if (length(terra::unwrap(thermal_uav@Position@locations_vector)) != 0){
      thermal_uav@Position@locations_vector <- terra::wrap(terra::unwrap(thermal_uav@Position@locations_vector)[KeepT == 1])
    }
    if (length(terra::unwrap(thermal_uav@Position@extents_vector)) != 0){
      thermal_uav@Position@extents_vector <- terra::wrap(terra::unwrap(thermal_uav@Position@extents_vector)[KeepT == 1])
    }
    thermal_uav@Position@overlap <- 0
    # Atmosphere
    if (length(thermal_uav@Atmosphere@T_air) != 0){
      thermal_uav@Atmosphere@T_air <- thermal_uav@Atmosphere@T_air[KeepT == 1]
      thermal_uav@Atmosphere@rel_hum <- thermal_uav@Atmosphere@rel_hum[KeepT == 1]
      thermal_uav@Atmosphere@flight_height <- thermal_uav@Atmosphere@flight_height[KeepT == 1]
      thermal_uav@Atmosphere@omega <- thermal_uav@Atmosphere@omega[KeepT == 1]
      thermal_uav@Atmosphere@Tau_atm <- thermal_uav@Atmosphere@Tau_atm[KeepT == 1]
    }
    # Thermal.Data
    if (length(thermal_uav@Thermal.Data) != 0){
      thermal_uav@Thermal.Data <- thermal_uav@Thermal.Data[[KeepT == 1]]
    }
    # (Re)move files
    if (remove == TRUE){
      for (i in seq_along(Rem)) {
        file.remove(paste0(thermal_uav@Info@path, Rem[i]))
      }
    } else {
      if (!("Removed by clean_persec" %in% thermal_uav@Info@path)) {
        # Create a new map
        dir.create(paste0(thermal_uav@Info@path, "Removed by clean_persec"))
      }
      for (i in seq_along(Rem)){
        fs::file_move(path = paste0(thermal_uav@Info@path, Rem[i]), new_path = paste0(thermal_uav@Info@path, "Removed by clean_persec", "/", Rem[i]))
      }
    }
    stop_no_images <- thermal_uav@Info@dataset_length
    no_images_removed <- start_no_images - stop_no_images
    # end of function
    thermal_uav@Sharpness@T_uav_clean_persec <- "Yes"
    thermal_uav@Sharpness@img_persec_remove <- no_images_removed
    message("Number of images (re)moved: ", no_images_removed)
    return(thermal_uav)
  })
}

#' Clean the dataset based on overlap or sharpness
#'
#' @description
#' `T_uav_clean_reduc()` reduces the thermal dataset, either based on sharpness or minimal desired overlap. NOTE: if extents are not yet calculated, this function will automatically calculate it.
#'
#' @param thermal_uav Thermal.UAV object retrieved by previous functions (e.g.: `T_uav_info()`). If Thermal.Data is empty, the raw TIFFs are loaded directly from the folder containing the tiff files.
#' @param method the method you want to choose for image reduction. This can be "Overlap" to reduce the dataset based on a minimal overlap (the final targetted overlap will be around `min_overlap`) or "Sharpness" to remove unsharp images (based on `sharpness_threshold`).
#' @param min_overlap (numerical) the targetted overlap (0-1) for the "Overlap" method or the criteria to acertain a minimal overlap in case of a large part of consecutive unsharp images. Default is 0.80
#' @param sharpness_threshold (numerical) the threshold to decide if an image is sharp or unsharp. Default is 0.25
#' @param remove (logical) use TRUE to delete the images for your local disk space. Default is FALSE where images are simple replaced in a subfolder called "Removed by img_reduc"
#' @return This function returns an updated (reduced) Thermal.UAV object and (re)moves the least sharp images
#' @export
T_uav_clean_reduc <- function(thermal_uav, method = "Overlap", min_overlap = 0.80, sharpness_threshold = 0.25, remove = FALSE){

  suppressWarnings({
    # Check class structure of thermal_uav
    if (!isa(thermal_uav, "Thermal.UAV")){
      stop(paste0("The provided parameter thermal_uav is not of class Thermal.UAV \n"))
    }
    # Check if sharpness is already calculated
    if (length(thermal_uav@Sharpness@Tsharp) == 0){
      thermal_uav <- T_uav_est_sharp(thermal_uav)
      TSharp <- thermal_uav@Sharpness@Tsharp
    } else {
      TSharp <- thermal_uav@Sharpness@Tsharp
    }
    # Check if extents are already calculated
    if (length(terra::unwrap(thermal_uav@Position@extents_vector)) == 0){
      thermal_uav <- T_uav_pos_sensor(thermal_uav, extent = TRUE, overlap = FALSE, export = FALSE)
    }
    # Number of images at start of function
    start_no_images <- thermal_uav@Info@dataset_length
    # Unwrap the extents to easily work with the shapefiles
    thermal_uav@Position@extents_vector <- terra::unwrap(thermal_uav@Position@extents_vector)
    # In case you want to reduce the images based on a certain minimal overlap
    if (method == "Overlap"){
      message("Image reduction is being executed using method 'Overlap'")
      i <- 1
      while (TSharp[i] < sharpness_threshold){ # In case the sharpness of the first image is not above the threshold
        i <- i+1
      }
      # List which images (ID's) should be kept to ensure good quality and enough overlap.
      # While loop used so we don't have to compute all the possible overlap scenario's.
      len <- thermal_uav@Info@dataset_length
      keepID <- rep(0, len)
      keepID[i] <- 1
      keep_unsharp_ID <- c()
      while (i < len){
        overlap_step <- 1
        step <- 1
        overlap_x <- c()
        sharpness_x <- c()
        while (overlap_step >= min_overlap && i+step <= len){
          intersect <-  terra::intersect(thermal_uav@Position@extents_vector[i], thermal_uav@Position@extents_vector[(i+step)])
          area_ext <- terra::expanse(thermal_uav@Position@extents_vector[i], unit="m")
          area_int <- terra::expanse(intersect)
          overlap_step <- area_int/area_ext
          if (length(overlap_step) == 0){
            overlap_step <- 0
          }
          overlap_x <- c(overlap_x, overlap_step)
          sharpness_x <- c(sharpness_x, TSharp[i+step])
          step <- step + 1
        }
        # Last case, incorporate last image as well
        if (i+step-1 == len){
          i <- i+step-1
          keepID[i] <- 1
          # If the drone turned and not enough overlap is ensured, take this image as well
        } else if (length(overlap_x) == 1){
          i <- i+step-1
          keepID[i] <- 1 # Especially keep these images as overlap is even below required
          # Nice, we found an image with just above the right overlap AND sharp enough
        } else if (sharpness_x[step - 2] > sharpness_threshold){
          i <- i+(step-2)
          keepID[i] <- 1
          # Look if there are other images with enough overlap AND sharp enough
        } else if (any(utils::head(sharpness_x, - 1) > sharpness_threshold)){ # remove last element, could be that the last is above threshold, but not enough overlap
          id_keep <- max(which(utils::head(sharpness_x, - 1) > sharpness_threshold))
          i <- i+id_keep
          keepID[i] <- 1
          # No images with enough overlap and sharp enough -> Just take the one with enough overlap and sadly enough not enough sharpness, but otherwise a gap in the data...
        } else {
          i <- i+(step-2)
          keepID[i] <- 1
          keep_unsharp_ID <- c(keep_unsharp_ID, i)
        }
        # New i is assigned and the algorithm will jump to this next i'th image
      }
    } else if (method == "Sharpness"){
      message("Image reduction is being executed using method 'Sharpness'")
      keepID <- rep(1, thermal_uav@Info@dataset_length)
      # Check which images would be deleted due to not sharp enough
      unsharp <- which(TSharp < sharpness_threshold)
      keepID[unsharp] <- 0
      # Calculate how many gaps this produces and how large these gaps are:
      diff <- base::diff(unsharp)
      gaps <- which(diff != 1)+1 # At which index of unsharp does a gap start
      if (diff[1] == 1){
        gaps <- c(1, gaps)
      }
      # Define where the last sharp before and first sharp after the gap is located
      gap_start <- unsharp[gaps]-1 # The last sharp image before the gap starts
      gape <- gaps[-1]-1
      if (diff[length(diff)] == 1){
        gape <- c(gape, length(diff)+1)
      }
      gap_end <- unsharp[gape]+1
      # Would give an error if first or last image of the dataset is unsharp => remove the whole unsharp "gap" at the beginning and end, by removing the corresponding gap. this way their keepID would remain 0
      if (unsharp[1] == 1){
        gap_start <- gap_start[-1]
        gap_end <- gap_end[-1]
      }
      if (unsharp[length(unsharp)] == thermal_uav@Info@dataset_length){
        gap_start <- gap_start[-length(gap_start)]
        gap_end <- gap_end[-length(gap_end)]
      }
      # Now loop over the gaps and check if it is too large or not. in case of too large fill it up with unsharp images until min overlap is reached
      pb <- progress::progress_bar$new(
        format = "Checking and filling gaps if needed: [:bar] :percent ETA: :eta",
        total = length(gap_start)
      )
      for (i in length(gap_start)){
        intersect <-  terra::intersect(thermal_uav@Position@extents_vector[gap_start[i]], thermal_uav@Position@extents_vector[gap_end[i]])
        area_ext <- terra::expanse(thermal_uav@Position@extents_vector[gap_start[i]], unit="m")
        area_int <- terra::expanse(intersect)
        overlap_gap <- area_int/area_ext
        if (length(overlap_gap) == 0){
          overlap_gap <- 0
        }
        start_step <- 0
        end_step <- 0
        keep_unsharp_ID <- c() # to store which id's were kept but are not sharp enough => for report
        while (overlap_gap < min_overlap){ # work systematically inwards
          gap_start_step <- gap_start[i] + start_step
          gap_end_step <- gap_end[i] - end_step
          gap_len <- length(gap_start_step:gap_end_step)-2
          # forward overlap
          step_overlap_start <- c()
          for (g in 1:gap_len){
            intersect <-  terra::intersect(thermal_uav@Position@extents_vector[gap_start_step], thermal_uav@Position@extents_vector[gap_start_step + g])
            area_ext <- terra::expanse(thermal_uav@Position@extents_vector[gap_start_step], unit="m")
            area_int <- terra::expanse(intersect)
            overlap_step <- area_int/area_ext
            if (length(overlap_step) == 0){
              overlap_step <- 0
            }
            step_overlap_start[g] <- overlap_step
          }
          # backward overlap
          step_overlap_end <- c()
          for (g in 1:gap_len){
            intersect <-  terra::intersect(thermal_uav@Position@extents_vector[gap_end_step], thermal_uav@Position@extents_vector[gap_end_step - (gap_len+1) + g])
            area_ext <- terra::expanse(thermal_uav@Position@extents_vector[gap_end_step], unit="m")
            area_int <- terra::expanse(intersect)
            overlap_step <- area_int/area_ext
            if (length(overlap_step) == 0){
              overlap_step <- 0
            }
            step_overlap_end[g] <- overlap_step
          }
          # now check if it's all right or if we need to fill some more gaps
          if (length(which(step_overlap_start > min_overlap & step_overlap_end > min_overlap)) != 0){
            indices <- which(step_overlap_start > min_overlap & step_overlap_end > min_overlap) + gap_start_step
            keep <- indices[which(TSharp[indices] == max(TSharp[indices]))]
            keepID[keep] <- 1
            keep_unsharp_ID <- c(keep_unsharp_ID, keep)
            intersect <-  terra::intersect(thermal_uav@Position@extents_vector[gap_start_step], thermal_uav@Position@extents_vector[keep])
            area_ext <- terra::expanse(thermal_uav@Position@extents_vector[gap_start_step], unit="m")
            area_int <- terra::expanse(intersect)
            overlap_gap <- area_int/area_ext
          } else {
            if (max(step_overlap_start) < min_overlap){
              start_step <- 1
              keep_start <- gap_start_step + start_step
              keepID[keep_start] <- 1
              keep_unsharp_ID <- c(keep_unsharp_ID, keep_start)
            } else {
              start_step <- max(which(step_overlap_start > min_overlap))
              keep_start <- gap_start_step + start_step
              keepID[keep_start] <- 1
              keep_unsharp_ID <- c(keep_unsharp_ID, keep_start)
            }
            if (max(step_overlap_end) < min_overlap){
              end_step <- 1
              keep_end <- gap_end_step + end_step
              keepID[keep_end] <- 1
              keep_unsharp_ID <- c(keep_unsharp_ID, keep_end)
            } else {
              end_step <- gap_len - min(which(step_overlap_end > min_overlap)) + 1
              keep_end <- gap_end_step + end_step
              keepID[keep_end] <- 1
              keep_unsharp_ID <- c(keep_unsharp_ID, keep_end)
            }
            intersect <-  terra::intersect(thermal_uav@Position@extents_vector[keep_start], thermal_uav@Position@extents_vector[keep_end])
            area_ext <- terra::expanse(thermal_uav@Position@extents_vector[keep_start], unit="m")
            area_int <- terra::expanse(intersect)
            overlap_gap <- area_int/area_ext
          }
        }
        pb$tick()
      }
    }
    # (Re)move the unsharp images and continue working with the sharp images only.
    # Remove the unsharp images and continue working with the sharp images only.
    # Alter the lengths of all variables
    message("(Re)moving images")
    Rem <- thermal_uav@Info@images[keepID == 0]
    thermal_uav@Sharpness@unsharp_kept_ID <- thermal_uav@Info@images[keep_unsharp_ID]
    # Info
    thermal_uav@Info@images <- thermal_uav@Info@images[keepID == 1]
    thermal_uav@Info@TTime <- thermal_uav@Info@TTime[keepID == 1]
    if (length(thermal_uav@Info@SubSecTime) != 0){
      thermal_uav@Info@SubSecTime <- thermal_uav@Info@SubSecTime[keepID == 1]
    }
    thermal_uav@Info@dataset_length <- length(which(keepID == 1))
    thermal_uav@Info@exif <- thermal_uav@Info@exif[keepID == 1, ]
    if (length(thermal_uav@Info@meta_df) != 0){
      thermal_uav@Info@meta_df <- thermal_uav@Info@meta_df[keepID == 1, ]
      df <- thermal_uav@Info@meta_df
      df$FileName <- thermal_uav@Info@images
      utils::write.csv(df, paste0(thermal_uav@Info@path, "metadata_update.csv"), sep = ",", row.names = FALSE)
    }
    # Sharpness
    thermal_uav@Sharpness@Tsharp <- thermal_uav@Sharpness@Tsharp[keepID == 1]
    # Position
    if (length(thermal_uav@Position@Lat) != 0){
      thermal_uav@Position@Lat <- thermal_uav@Position@Lat[keepID == 1]
    }
    if (length(thermal_uav@Position@Lon) != 0){
      thermal_uav@Position@Lon <- thermal_uav@Position@Lon[keepID == 1]
    }
    thermal_uav@Position@FH <- thermal_uav@Position@FH[keepID == 1]
    if (length(thermal_uav@Position@Yaw) != 0){
      thermal_uav@Position@Yaw <- thermal_uav@Position@Yaw[keepID == 1]
    }
    if (length(terra::unwrap(thermal_uav@Position@locations_vector)) != 0){
      thermal_uav@Position@locations_vector <- terra::wrap(terra::unwrap(thermal_uav@Position@locations_vector)[keepID == 1])
    }
    if (length(thermal_uav@Position@extents_vector) != 0){
      thermal_uav@Position@extents_vector <- thermal_uav@Position@extents_vector[keepID == 1]
    }
    thermal_uav@Position@overlap <- 0
    # Atmosphere
    if (length(thermal_uav@Atmosphere@T_air) != 0){
      thermal_uav@Atmosphere@T_air <- thermal_uav@Atmosphere@T_air[keepID == 1]
      thermal_uav@Atmosphere@rel_hum <- thermal_uav@Atmosphere@rel_hum[keepID == 1]
      thermal_uav@Atmosphere@flight_height <- thermal_uav@Atmosphere@flight_height[keepID == 1]
      thermal_uav@Atmosphere@omega <- thermal_uav@Atmosphere@omega[keepID == 1]
      thermal_uav@Atmosphere@Tau_atm <- thermal_uav@Atmosphere@Tau_atm[keepID == 1]
    }
    # Thermal.Data
    if (length(thermal_uav@Thermal.Data) != 0){
      thermal_uav@Thermal.Data <- thermal_uav@Thermal.Data[[keepID == 1]]
    }
    # (Re)move files
    if (remove == TRUE){
      for (i in seq_along(Rem)) {
        file.remove(paste0(thermal_uav@Info@path, Rem[i]))
      }
    } else {
      if (!("Removed by clean_reduc" %in% thermal_uav@Info@path)) {
        # Create a new map
        dir.create(paste0(thermal_uav@Info@path, "Removed by clean_reduc"))
      }
      for (i in seq_along(Rem)){
        fs::file_move(path = paste0(thermal_uav@Info@path, Rem[i]), new_path = paste0(thermal_uav@Info@path, "Removed by clean_reduc", "/", Rem[i]))
      }
    }
    # Calculate the eventual overlap
    len <- thermal_uav@Info@dataset_length-1
    overlap <- rep(NaN, len)
    pb <- progress::progress_bar$new(
      format = "Calculating new overlap: [:bar] :percent ETA: :eta",
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
    # Wrap the extents to make it exportable
    thermal_uav@Position@extents_vector <- terra::wrap(thermal_uav@Position@extents_vector)
    # end of function
    stop_no_images <- thermal_uav@Info@dataset_length
    no_images_removed <- start_no_images - stop_no_images
    thermal_uav@Sharpness@T_uav_clean_reduc <- "Yes"
    thermal_uav@Sharpness@img_reduc_remove <- no_images_removed
    message("Number of images (re)moved: ", no_images_removed)
    thermal_uav@Sharpness@Thresh <- sharpness_threshold
    thermal_uav@Position@min_overlap <- min_overlap
    thermal_uav@Sharpness@method <- method
  })
  return(thermal_uav)
}
