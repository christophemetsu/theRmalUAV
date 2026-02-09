# Spatial emissivity correction

`tuav_emis()` corrects a stitched thermal orthomosaic for emissivity,
based on a high resolution landcover or emissivity map.

## Usage

``` r
tuav_emis(
  thermal_orig,
  thermal_uav,
  temp = "K",
  corrmap,
  method = "LC",
  write_Ts = FALSE,
  filename_Ts = NA,
  write_emiss = FALSE,
  filename_emiss = NA,
  NDVI_veg = 0.8,
  NDVI_soil = 0.2,
  emiss_veg = 0.988,
  emiss_soil = 0.935,
  LC_emiss_matrix = NA
)
```

## Arguments

- thermal_orig:

  Map
  ([`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html))
  or path to geotiff file (chr) to the original land surface temperature
  map

- thermal_uav:

  the
  [`ThermalUAV`](https://christophemetsu.github.io/theRmalUAV/reference/ThermalUAV-Class.md)
  object corresponding to this thermal map. Is needed to extract
  parameters used in
  [`tuav_correct()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_correct.md)

- temp:

  indicate in which scale the thermal map represents the temperatures.
  Use "K" for Kelvin and "C" for degree Celcius.

- corrmap:

  Map
  ([`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html))
  or path to geotiff file (chr) for emissivity correction. This can
  either be a landcover map, an NDVI map or a emissivity map.

- method:

  Indicate which variable the correction map provides. Use "LC" when
  landcover is provided, "NDVI" if you want to use the NDVI method or
  "EM" when emissivity is provided.

- write_Ts:

  (logical) use TRUE if the corrected thermal map must be written.
  Default = FALSE

- filename_Ts:

  the path and filename where the corrected thermal map should be
  stored. If NA, it is written in the thermal path with label
  "\_emis_corr". This variable is only used if write_Ts = TRUE

- write_emiss:

  (logical) use TRUE if the emissivity map must be written. Default =
  FALSE

- filename_emiss:

  the path and filename where the emissivity map should be stored. If
  NA, it is written in the thermal path with label "\_emis". This
  variable is only used if write_emiss = TRUE

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

returns a recalculated thermal map corrected for spatially varying
different emissivities.
