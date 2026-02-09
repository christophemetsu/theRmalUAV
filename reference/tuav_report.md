# Make a TheRmalUAV report

`tuav_report()` writes a report with information about the processing
done with this package

## Usage

``` r
tuav_report(
  thermal_uav,
  path = NA,
  project_name = "Thermal_uav",
  flight_name = "",
  pilot_name = "pilot_name",
  location = "location"
)
```

## Arguments

- thermal_uav:

  [`ThermalUAV`](https://christophemetsu.github.io/theRmalUAV/reference/ThermalUAV-Class.md)
  object retrieved by previous functions (e.g.:
  [`tuav_create()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_create.md)).

- path:

  the path to the folder were you want to save the report. If NA the
  report will be saved in the path of the original tifs

- project_name:

  (character) the name of the project to put in the report

- flight_name:

  (character) the name of the flight to put in the report

- pilot_name:

  (character) the name of the pilot to put in the report

- location:

  (character) the location of where the flight was executed

## Value

exports the ThermalData as tif files

## See also

[`tuav_export()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_export.md)
