#ifndef GET_TEMP_DIRP_CPP_H
#define GET_TEMP_DIRP_CPP_H

#include <Rcpp.h>
#include <string>

using namespace Rcpp;

//' @title Thermal Image Temperature Extraction
//' @description Extracts temperature data from an R-JPEG thermal image using DJI's Thermal SDK.
//' @param filepath_image Path to the R-JPEG thermal image file.
//' @param image_height Image height in pixels.
//' @param image_width Image width in pixels.
//' @param object_distance Distance to the target in meters (1-25m).
//' @param relative_humidity Relative humidity of the environment (20-100\%).
//' @param emissivity Emissivity of the target surface (0.10-1.00).
//' @param reflected_apparent_temperature Reflected apparent temperature in Celsius (-40.0 to 500.0 deg C).
//' @param filepath_dll Path to the dynamic link library (DLL) for processing.
//' @return A numeric matrix representing the temperature values of the image.
// [[Rcpp::export]]
NumericMatrix get_temp_dirp_cpp(
    std::string filepath_image,
    int image_height,
    int image_width,
    double object_distance,
    double relative_humidity,
    double emissivity,
    double reflected_apparent_temperature,
    std::string filepath_dll);

#endif // GET_TEMP_DIRP_CPP_H
