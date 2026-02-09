# Simulating the correction

`sim_correct()` Converts the brightness temperature to land surface
temperature of a single value, vector or matrix or
[`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
object. It is basically Equation 3 from `vignette("Background")` in the
form of an R-function. This function is intended to simulate the effects
of the parameters on the output and doesn't process any positional
information/metadata.

## Usage

``` r
sim_correct(
  T_bright,
  flight_height = 50,
  T_air = 25,
  rel_hum = 70,
  T_bg = NA,
  emiss = 0.985
)
```

## Arguments

- T_bright:

  Brightness temperature (in Kelvin) that you want to convert. This can
  either be a single value, a vector or a matrix or
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  object

- flight_height:

  (numerical) the height between the camera and the OBJECT OF INTEREST.
  Default is 50 m

- T_air:

  (numerical) the air temperature in deg C during the flight. Default is
  25 deg C

- rel_hum:

  (numerical) the relative humidity in % during the flight. Default is
  70 %

- T_bg:

  (numerical) the background temperature in Kelvin, temperature measured
  from the aluminium panel. If not recorded set to NA and the parameter
  will not be accounted for.

- emiss:

  (numerical) the emissivity, here by default an average for plants is
  given (0.985)

## Value

Corrected LST in the same format as the T_bright parameter
