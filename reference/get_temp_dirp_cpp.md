# Thermal Image Temperature Extraction

Extracts temperature data from an R-JPEG thermal image using DJI's
Thermal SDK.

## Usage

``` r
get_temp_dirp_cpp(
  filepath_image,
  image_height,
  image_width,
  object_distance,
  relative_humidity,
  emissivity,
  reflected_apparent_temperature,
  filepath_dll
)
```

## Arguments

- filepath_image:

  Path to the R-JPEG thermal image file.

- image_height:

  Image height in pixels.

- image_width:

  Image width in pixels.

- object_distance:

  Distance to the target in meters (1-25m).

- relative_humidity:

  Relative humidity of the environment (20-100%).

- emissivity:

  Emissivity of the target surface (0.10-1.00).

- reflected_apparent_temperature:

  Reflected apparent temperature in Celsius (-40.0 to 500.0 deg C).

- filepath_dll:

  Path to the dynamic link library (DLL) for processing.

## Value

A numeric matrix representing the temperature values of the image.
