# Get sharpness threshold

`tuav_sharp_thresh()` estimates sharpness by iteratively asking whether
an image is sharp enough, until it is sharp enough. The goal is to set
your own sharpness threshold which is used in the image reduction
function.

## Usage

``` r
tuav_sharp_thresh(thermal_uav)
```

## Arguments

- thermal_uav:

  [`ThermalUAV`](https://christophemetsu.github.io/theRmalUAV/reference/ThermalUAV-Class.md)
  object retrieved by previous functions (e.g.:
  [`tuav_create()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_create.md)).
  If ThermalData is empty, the raw TIFFs are loaded directly from the
  folder containing the tiff files.

## Value

This function returns a sharpness value which can be set as threshold in
the
[`tuav_reduc()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_reduc.md)
function
