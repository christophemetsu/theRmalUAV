# Clean the dataset based on overlap or sharpness

`tuav_reduc()` reduces the thermal dataset, either based on sharpness or
minimal desired overlap. NOTE: if extents are not yet calculated, this
function will automatically calculate it.

## Usage

``` r
tuav_reduc(
  thermal_uav,
  method = "Overlap",
  min_overlap = 0.8,
  sharpness_threshold = 0.05,
  remove = FALSE
)
```

## Arguments

- thermal_uav:

  [`ThermalUAV`](https://christophemetsu.github.io/theRmalUAV/reference/ThermalUAV-Class.md)
  object retrieved by previous functions (e.g.:
  [`tuav_create()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_create.md)).
  If ThermalData is empty, the raw TIFFs are loaded directly from the
  folder containing the tiff files.

- method:

  the method you want to choose for image reduction. This can be
  "Overlap" to reduce the dataset based on a minimal overlap (the final
  targetted overlap will be around \`min_overlap\`) or "Sharpness" to
  remove unsharp images (based on \`sharpness_threshold\`).

- min_overlap:

  (numerical) the targetted overlap (0-1) for the "Overlap" method or
  the criteria to acertain a minimal overlap in case of a large part of
  consecutive unsharp images. Default is 0.80

- sharpness_threshold:

  (numerical) the threshold to decide if an image is sharp or unsharp.
  Default is 0.05

- remove:

  (logical) use TRUE to delete the images for your local disk space.
  Default is FALSE where images are simple replaced in a subfolder
  called "Removed by img_reduc"

## Value

This function returns an updated (reduced)
[`ThermalUAV`](https://christophemetsu.github.io/theRmalUAV/reference/ThermalUAV-Class.md)
object and (re)moves the least sharp images
