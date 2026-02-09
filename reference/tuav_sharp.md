# Estimates image sharpness

`tuav_sharp()` estimates sharpness of the thermal images.

## Usage

``` r
tuav_sharp(thermal_uav)
```

## Arguments

- thermal_uav:

  [`ThermalUAV`](https://christophemetsu.github.io/theRmalUAV/reference/ThermalUAV-Class.md)
  object retrieved by previous functions (e.g.:
  [`tuav_create()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_create.md)).
  If ThermalData is empty, the raw TIFFs are loaded directly from the
  folder containing the tiff files.

## Value

This function returns an updated
[`ThermalUAV`](https://christophemetsu.github.io/theRmalUAV/reference/ThermalUAV-Class.md)
object containing a vector with the sharpness for each image.
