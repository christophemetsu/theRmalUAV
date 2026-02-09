# Export thermal data as TIFF files

`tuav_export()` exports the ThermalData. Note in order to export the
data, at least tuav_correct should have been ran first

## Usage

``` r
tuav_export(thermal_uav, export_path = NA)
```

## Arguments

- thermal_uav:

  [`ThermalUAV`](https://christophemetsu.github.io/theRmalUAV/reference/ThermalUAV-Class.md)
  object retrieved by previous functions (e.g.:
  [`tuav_cameras()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_cameras.md)).

- export_path:

  path to the folder where the images should be stored. If NA, images
  are stored in a new folder in the original path called 'corrected'

## Value

exports the ThermalData as tiff files. Note data is stored as
centikelvin.

## See also

[`tuav_report()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_report.md)
