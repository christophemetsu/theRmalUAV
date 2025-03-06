#include <Rcpp.h>
#include <iostream>
#include <fstream>
#include <vector>
#include <windows.h>
#include "DJI.h"

using namespace std;
using namespace Rcpp;

// Structure definitions
struct dirp_rjpeg_version_t {
  uint32_t rjpeg;
  uint32_t header;
  uint32_t curve;
};

struct dirp_resolution_t {
  uint32_t width;
  uint32_t height;
};

struct dirp_measurement_params_t {
  float distance;
  float humidity;
  float emissivity;
  float reflection;
};

typedef void* DIRP_HANDLE;
typedef int32_t (*dirp_create_from_rjpeg_t)(uint8_t*, int32_t, DIRP_HANDLE*);
typedef int32_t (*dirp_destroy_t)(DIRP_HANDLE);
typedef int32_t (*dirp_get_rjpeg_version_t)(DIRP_HANDLE, dirp_rjpeg_version_t*);
typedef int32_t (*dirp_get_rjpeg_resolution_t)(DIRP_HANDLE, dirp_resolution_t*);
typedef int32_t (*dirp_get_measurement_params_t)(DIRP_HANDLE, dirp_measurement_params_t*);
typedef int32_t (*dirp_set_measurement_params_t)(DIRP_HANDLE, dirp_measurement_params_t*);
typedef int32_t (*dirp_measure_ex_t)(DIRP_HANDLE, float*, int32_t);

NumericMatrix get_temp_dirp_cpp(
    std::string filepath_image,
    int image_height,
    int image_width,
    double object_distance,
    double relative_humidity,
    double emissivity,
    double reflected_apparent_temperature,
    std::string filepath_dll) {

  HINSTANCE hDLL = LoadLibrary(filepath_dll.c_str());
  if (!hDLL) {
    stop("Failed to load DLL.");
  }

  auto dirp_create_from_rjpeg = (dirp_create_from_rjpeg_t)GetProcAddress(hDLL, "dirp_create_from_rjpeg");
  auto dirp_destroy = (dirp_destroy_t)GetProcAddress(hDLL, "dirp_destroy");
  auto dirp_get_rjpeg_version = (dirp_get_rjpeg_version_t)GetProcAddress(hDLL, "dirp_get_rjpeg_version");
  auto dirp_get_rjpeg_resolution = (dirp_get_rjpeg_resolution_t)GetProcAddress(hDLL, "dirp_get_rjpeg_resolution");
  auto dirp_get_measurement_params = (dirp_get_measurement_params_t)GetProcAddress(hDLL, "dirp_get_measurement_params");
  auto dirp_set_measurement_params = (dirp_set_measurement_params_t)GetProcAddress(hDLL, "dirp_set_measurement_params");
  auto dirp_measure_ex = (dirp_measure_ex_t)GetProcAddress(hDLL, "dirp_measure_ex");

  if (!dirp_create_from_rjpeg || !dirp_destroy || !dirp_get_rjpeg_version ||
      !dirp_get_rjpeg_resolution || !dirp_get_measurement_params ||
      !dirp_set_measurement_params || !dirp_measure_ex) {
      FreeLibrary(hDLL);
    stop("Failed to get function addresses.");
  }

  ifstream file(filepath_image, ios::binary);
  vector<uint8_t> raw((istreambuf_iterator<char>(file)), istreambuf_iterator<char>());

  DIRP_HANDLE handle;
  int32_t status = dirp_create_from_rjpeg(raw.data(), raw.size(), &handle);
  if (status != 0) {
    FreeLibrary(hDLL);
    stop("dirp_create_from_rjpeg error: " + std::to_string(status));
  }

  dirp_rjpeg_version_t rjpeg_version;
  dirp_get_rjpeg_version(handle, &rjpeg_version);

  dirp_resolution_t rjpeg_resolution;
  dirp_get_rjpeg_resolution(handle, &rjpeg_resolution);

  dirp_measurement_params_t params;
  dirp_get_measurement_params(handle, &params);

  params.distance = object_distance;
  params.humidity = relative_humidity;
  params.emissivity = emissivity;
  params.reflection = reflected_apparent_temperature;

  dirp_set_measurement_params(handle, &params);

  vector<float> temp(image_width * image_height, 0);
  dirp_measure_ex(handle, temp.data(), image_width * image_height * sizeof(float));

  dirp_destroy(handle);
  FreeLibrary(hDLL);

  NumericMatrix result(image_height, image_width);
  for (int i = 0; i < image_height; ++i) {
    for (int j = 0; j < image_width; ++j) {
      result(i, j) = temp[i * image_width + j];
    }
  }

  return result;
}
