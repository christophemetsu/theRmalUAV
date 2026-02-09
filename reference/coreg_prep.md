# Prepare data for co-registration

`coreg_prep()` prepares the data from a different camera (with higher
GPS accuracy) to use in the
[`tuav_coreg()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_coreg.md)
function.

## Usage

``` r
coreg_prep(
  img_path,
  SfM_option = NA,
  opt_camera_path = NA,
  epsg = "4326",
  camera_name = "Altum-PT_MSP",
  label = NA,
  timezone = NA
)
```

## Arguments

- img_path:

  the folder containing the high resolution images

- SfM_option:

  if the cameras were first aligned in a SfM software, for now the only
  option is "Agisoft Metashape". Use NA if this was not the case and you
  want to run it without previous alignement. Default is NA

- opt_camera_path:

  path to the .txt or .csv file where the optimized camera positions is
  stored (only if SfM_option is not NA), defualt is NA

- epsg:

  the epsg in which the coordinates are exported/stored, by default
  "4326" (WGS 84, latitude/longitude coordinate system based on the
  Earth's center of mass)

- camera_name:

  The name of the high resolution camera of which the optimized GPS data
  will be used

- label:

  needed for multispectral data, it is the subscript/sequence describing
  the band/camera on which every position offset is calculated (in case
  of the Micasence Altum-PT it is the green band: "\_2"). If no label
  enter ”

- timezone:

  the timezone in which the datetime data is stored. Default is NA and
  this preset will use the timezone of your computer system

## Value

This function returns a data.frame containing positional information
about optimized cameras which can be used as input in the
[`tuav_coreg()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_coreg.md)
function

## See also

[`tuav_coreg()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_coreg.md)
