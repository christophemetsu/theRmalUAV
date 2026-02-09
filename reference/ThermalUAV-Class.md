# Class "ThermalUAV"

`ThermalUAV` groups all the necessary information together in a logical
order. The object of Class ThermalUAV serves in most functions as input
as it provides the data upon which most functions are build. It consists
of other thermal classes, each representing different kinds of
information.

## Slots

- `Info`:

  (S4 ThermalInfo) Holds general information regaring the information of
  the image files and project such as path, filenames, camera info, meta
  data, time etc.

- `Position`:

  (S4 ThermalPosition) Holds information about the position of the tifs
  including gps location, flightheight, overlap as well as shapefiles
  (when
  [`tuav_loc()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_loc.md)
  is run)

- `Sharpness`:

  (S4 ThermalSharpness) Holds information about sharpness which is used
  for cleaning (e.g.: Sharpness threshold, cleaning methods, number of
  images removed. etc.)

- `Atmosphere`:

  (S4 ThermalAtmosphere) Holds information about the atmospheric
  conditions during the flight including air temperature, relative
  humidity, atm. transmission, emissivity

- `Smooth`:

  (S4 ThermalSmooth) Holds information about the thermal smoothing
  correction such as: smooth length, method etc.

- `ThermalData`:

  (list) a list of matrices holding the thermal tiff files. Depending on
  which functions are already run, it represents raw or corrected data.

## Examples

``` r
# Creates an empty ThermalUAV class
thermal_class <- methods::new("ThermalUAV")
```
