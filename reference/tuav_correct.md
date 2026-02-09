# Computes image-level corrections

`tuav_correct()` executes atmospheric corrections. Based on the amount
of input different corrections will be done.

## Usage

``` r
tuav_correct(
  thermal_uav,
  flight_height = NA,
  T_air = NA,
  rel_hum = NA,
  T_bg = NA,
  SKC = TRUE,
  emiss = 0.985
)
```

## Arguments

- thermal_uav:

  [`ThermalUAV`](https://christophemetsu.github.io/theRmalUAV/reference/ThermalUAV-Class.md)
  object retrieved by previous functions (e.g.:
  [`tuav_create()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_create.md))

- flight_height:

  (numerical) the height between the camera and the OBJECT OF INTEREST.
  Can be a single value or a vector of the same length as the number of
  images. If not specified it will search for GPS altitude in the
  exif/meta data, or use the flight_height calculated in the
  [`tuav_loc()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_loc.md)
  function

- T_air:

  (numerical) the air temperature in deg C during the flight. Can be a
  single value, a vector of same length as the number of images or a
  data.frame containing temperature data ("T_air") and datetime
  ("datetime") in UTC ("%d.%m.%Y %H:%M:%OS" format). If not provided the
  mean temperature of the images will be taken

- rel_hum:

  (numerical) the relative humidity in % during the flight. Can be a
  single value, a vector of same length as the number of images or a
  data.frame containing relative humidity data ("rel_hum") and datetime
  ("datetime") in UTC ("%d.%m.%Y %H:%M:%OS" format). If not provided a
  value of 50% is assumed across the entire flight.

- T_bg:

  (numerical) the background temperature in Kelvin, temperature measured
  from the aluminium panel. If not recorded set to NA and the parameter
  will be estimated using ("T_air"), in this case, you need to specify
  whether the sky was clear ("SKC" = TRUE) or overcast ("SKC" = FALSE).

- SKC:

  (logical) if the conditions were sky clear, set to TRUE, if there was
  an overcast set to FALSE (default = TRUE), only needed when T_bg is
  not specified.

- emiss:

  (numerical) the emissivity, here by default an average for plants is
  given (0.985)

## Value

This function returns an updated
[`ThermalUAV`](https://christophemetsu.github.io/theRmalUAV/reference/ThermalUAV-Class.md)
object containing ThermalData which is atmospherically corrected

## See also

[`tuav_create()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_create.md)
to get all the necessary information in the right format.
