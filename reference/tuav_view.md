# Plot cameras/image extents on an interactive map

`tuav_view()` shows the extents or camera positions in the viewer as
interactive map. NOTE: the function
[`tuav_loc()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_loc.md)
should have been run previously

## Usage

``` r
tuav_view(thermal_uav, extent = FALSE)
```

## Arguments

- thermal_uav:

  [`ThermalUAV`](https://christophemetsu.github.io/theRmalUAV/reference/ThermalUAV-Class.md)
  object retrieved by previous functions (e.g.:
  [`tuav_create()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_create.md))

- extent:

  (logical) use TRUE if you want to plot the image extents. NOTE: only
  if extents are previously calculated in the
  [`tuav_loc()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_loc.md)
  function

## Value

shows the GPS locations or the extents in an interactive way

## See also

[`tuav_loc()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_loc.md)
