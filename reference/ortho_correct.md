# Atmospheric and emissivity correction for othomosaics

`ortho_correct()` executes atmospheric and emissivity corrections on
already stitched maps. This function can be used if only single values
for the atmospheric parameters collected or when data from integrated
cameras have difficulties with stitching thermal data separately. Within
flight variations are not captured in this function.

## Usage

``` r
ortho_correct(
  thermal_ortho = thermal_ortho,
  temp = "C",
  flight_height = NA,
  T_air = NA,
  rel_hum = NA,
  T_bg = NA,
  SKC = TRUE,
  emiss = 0.985,
  method = NA,
  corrmap = NA,
  return_emis = FALSE,
  NDVI_veg = 0.8,
  NDVI_soil = 0.2,
  emiss_veg = 0.988,
  emiss_soil = 0.935,
  LC_emiss_matrix = NA
)
```

## Arguments

- thermal_ortho:

  Map
  ([`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html))
  or path to geotiff file (chr) containing brightness temperatures

- temp:

  Indicate in which scale the "thermal_ortho" map represents the
  temperatures. Use "K" for Kelvin and "C" for degree Celcius.

- flight_height:

  (numerical) the height between the camera and the OBJECT OF INTEREST
  expressed in meters.

- T_air:

  (numerical) the air temperature in deg C during the flight. If NA it
  will use a trimmed mean of the temperatures in the thermal_ortho map.

- rel_hum:

  (numerical) the relative humidity in % during the flight. If not
  provided a value of 50% is assumed.

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

- method:

  Indicate which variable the correction map provides. Use "LC" when
  landcover is provided, "NDVI" if you want to use the NDVI method or
  "EM" when emissivity is provided. Set to NA if no spatial information
  on the emissivity can be calculated.

- corrmap:

  Map
  ([`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html))
  or path to geotiff file (chr) for emissivity correction. This can
  either be a landcover map, an NDVI map or a emissivity map.

- return_emis:

  (logical) when set to TRUE, the calcualted emissivty map is returned
  together with the surface temperature as raster stack

- NDVI_veg:

  the NDVI value of vegetation. NDVI values above this threshold will
  receive an emissivity as defined in emiss_veg

- NDVI_soil:

  the NDVI value of bare soil. NDVI values below this threshold will
  receive an emissivity as defined in emiss_soil

- emiss_veg:

  the emissivity for vegetation, will be taken for an NDVI higher than
  NDVI_veg

- emiss_soil:

  the emissivity for bare soil, will be taken for an NDVI lower than
  NDVI_soil

- LC_emiss_matrix:

  a two column matrix with the first column the LC values and in the
  second the corresponding emissivties, only used if method = "LC"

## Value

returns a thermal map with atmospherically corrected temperatures, and
accounted for (spatially varying) emissivity.
