# Thermal SDK from DJI: corrections using DJI's algorithm

`tuav_dji()` Processes the data using the DJI thermal processing tool.
There are some limitations: one parameter for all images, limited
ranges...

## Usage

``` r
tuav_dji(
  thermal_uav,
  obj_dist = NA,
  rel_hum = NA,
  emissivity = NA,
  refl_temp = NA,
  return = TRUE,
  export = FALSE,
  export_path = NA
)
```

## Arguments

- thermal_uav:

  [`ThermalUAV`](https://christophemetsu.github.io/theRmalUAV/reference/ThermalUAV-Class.md)
  object retrieved by previous functions (e.g.:
  [`tuav_create()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_create.md))

- obj_dist:

  (numerical) Indicates the distance (in meter) between the camera and
  the object of interest \[1-25\]. Default = 5m

- rel_hum:

  (numerical) the relative humidity (in percentage), typically in the
  range \[20 - 100\]. Default = 70

- emissivity:

  (numerical) How strongly the target surface is emitting energy as
  thermal radiation. Value range is \[0.10 - 1.00\]. Default = 0.98

- refl_temp:

  (numerical) Reflected temperature in degree Celsius. The surface of
  the target that is measured could reflect the energy radiated by the
  surrounding objects. Value range is \[-40.0 - 500.0\]. Default = 23

- return:

  (logical) indicate if you want the matrix/list of matrices returned in
  your R environment. Default = TRUE

- export:

  (logical) Indicate TRUE if you want to write the output as tiff file.
  Default = FALSE

- export_path:

  (character) Path for the export file, only used when export = TRUE, if
  NA, the function will create a nuw map called 'dirp_Ts' in the folder
  where the images are stored

## Value

A matrix or list of matrices containing the corrected temperature if
parameter return = TRUE
