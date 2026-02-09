# Get all the necessary info and create a ThermalUAV object

`tuav_create()` gets all the information needed for further analysis and
creates a
[`ThermalUAV`](https://christophemetsu.github.io/theRmalUAV/reference/ThermalUAV-Class.md)
object

## Usage

``` r
tuav_create(
  path,
  camera = "ThermalCapture",
  meta_csv = NA,
  flight_height = NA,
  tz = NA
)
```

## Arguments

- path:

  The path to the folder containing the TIFF files, or path to 1 TIFF
  file

- camera:

  (character) Indicate your camera name, you can check through
  [`tuav_cameras()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_cameras.md)

- meta_csv:

  The Path to an additional meta data can be provided in the form of a
  csv, if not provided all info will be derived from exif data (which
  may limit some function options)

- flight_height:

  (numerical) the height between the CAMERA and the GROUND. can be a
  single value or a vector of the same length as the number of images.
  If not specified it will search for GPS altitude in the exif/meta data

- tz:

  The timezone of the flight. Important if you want to correct the data
  using a meteorological dataset. If not provided (default = NA), the
  function will use the system's timezone through Sys.timezone()

## Value

A
[`ThermalUAV`](https://christophemetsu.github.io/theRmalUAV/reference/ThermalUAV-Class.md)
object with all the necessary information, serves as input in the
following functions
