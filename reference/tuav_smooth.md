# Smooths thermal data

`tuav_smooth()` smooths the thermal dataset to avoid large fluctuations.
Two methods are possible: (i) based on a provided high temporal
resolution air temperature dataset or (ii) empirical smoothing using the
temperatures of the images.

## Usage

``` r
tuav_smooth(thermal_uav, method = "image", T_air = NA, smooth_length = NA)
```

## Arguments

- thermal_uav:

  [`ThermalUAV`](https://christophemetsu.github.io/theRmalUAV/reference/ThermalUAV-Class.md)
  object retrieved by previous functions (e.g.:
  [`tuav_create()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_create.md)).
  Note in order to smooth the thermal data, tuav_correct should be ran
  at first

- method:

  One of the two methods: (i) "T_air" the dataset is smoothed based on a
  high resolution air temperature dataset or (ii) "image" smoothing is
  done based on the average image temperature, default is "image"

- T_air:

  For method "T_air": the air temperature in deg C during the flight.
  Can be a vector of same length as the number of images or a data.frame
  containing temperature data ("T_air") and datetime ("datetime") in UTC
  ("%d.%m.%Y %H:%M:%OS" format). If already provided in
  [`tuav_correct()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_correct.md),
  you can leave it black here. Default is NA

- smooth_length:

  (numerical) is the amount of images that will be taken into account to
  smooth the temperature. Default is NA where the smooth length is the
  number of overlapping images. The smooth length is ignored for method
  "T_air".

## Value

This function returns an updated
[`ThermalUAV`](https://christophemetsu.github.io/theRmalUAV/reference/ThermalUAV-Class.md)
object with smoothed ThermalData
