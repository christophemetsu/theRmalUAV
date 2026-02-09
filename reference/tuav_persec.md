# Keep the best image(s) per second

`tuav_persec()` ranks all images within 1 second based on sharpness and
keeps x sharpest images. This function is primarily for
streaming/filming thermal cameras like the ThermalCapture. The goal is
to reduce the number of images before further processing.

## Usage

``` r
tuav_persec(thermal_uav, number_keep = 1, remove = FALSE)
```

## Arguments

- thermal_uav:

  [`ThermalUAV`](https://christophemetsu.github.io/theRmalUAV/reference/ThermalUAV-Class.md)
  object retrieved by previous functions (e.g.:
  [`tuav_create()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_create.md))

- number_keep:

  (numerical) the number of images you want to keep per second (default
  = 1)

- remove:

  (logical) use TRUE to delete the images for your local disk space,
  default is FALSE where images are simple replaced in a subfolder
  called "Removed by img_persec"

## Value

an updated
[`ThermalUAV`](https://christophemetsu.github.io/theRmalUAV/reference/ThermalUAV-Class.md)
object and (re)moves the least sharp images
