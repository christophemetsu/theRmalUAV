# Get information on camera locations

`tuav_loc()` gives the location of all images with the option to
retrieve the extents overlap. Furthermore, it gives the option to export
the GPS/extent information

## Usage

``` r
tuav_loc(thermal_uav, extent = FALSE, overlap = FALSE, export = FALSE)
```

## Arguments

- thermal_uav:

  [`ThermalUAV`](https://christophemetsu.github.io/theRmalUAV/reference/ThermalUAV-Class.md)
  object retrieved by previous functions (e.g.:
  [`tuav_create()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_create.md))

- extent:

  (logical) use TRUE to get the extents instead of the GPS locations,
  default is FALSE

- overlap:

  (logical) use TRUE if you want to calculate the average overlap
  between the subsequent images (only if extent = TRUE), default is
  FALSE

- export:

  (logical) use TRUE if you want to export the GPS/extents as a
  shapefile with the image names as attributes into a new folder
  "Shapefiles" within the path folder, default is FALSE

## Value

an updated
[`ThermalUAV`](https://christophemetsu.github.io/theRmalUAV/reference/ThermalUAV-Class.md)
object containing additional positional information

## See also

[`tuav_create()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_create.md),
[`tuav_view()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_view.md)
