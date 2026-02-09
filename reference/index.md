# Package index

## Image-based workflow

All the basic and additional functions within the image-based workflow

### ThermalUAV Object

S4 class in which the information and data is stored within the
R-environment

- [`ThermalUAV-Class`](https://christophemetsu.github.io/theRmalUAV/reference/ThermalUAV-Class.md)
  : Class "ThermalUAV"

### Basic functions

The basic steps in the image-based workflow, create, correct and export

- [`tuav_cameras()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_cameras.md)
  : A list of supported cameras
- [`tuav_create()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_create.md)
  : Get all the necessary info and create a ThermalUAV object
- [`tuav_correct()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_correct.md)
  : Computes image-level corrections
- [`tuav_export()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_export.md)
  : Export thermal data as TIFF files
- [`tuav_emis()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_emis.md)
  : Spatial emissivity correction
- [`tuav_report()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_report.md)
  : Make a TheRmalUAV report

### Position functions

Calculate, view and optimize camera locations

- [`tuav_loc()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_loc.md)
  : Get information on camera locations
- [`tuav_view()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_view.md)
  : Plot cameras/image extents on an interactive map
- [`coreg_prep()`](https://christophemetsu.github.io/theRmalUAV/reference/coreg_prep.md)
  : Prepare data for co-registration
- [`tuav_coreg()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_coreg.md)
  : Co-register locations with another camera

### Cleaning functions

Clean thermal datasets based on overlap and or sharpness

- [`tuav_sharp()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_sharp.md)
  : Estimates image sharpness
- [`tuav_sharp_thresh()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_sharp_thresh.md)
  : Get sharpness threshold
- [`tuav_persec()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_persec.md)
  : Keep the best image(s) per second
- [`tuav_reduc()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_reduc.md)
  : Clean the dataset based on overlap or sharpness
- [`tuav_smooth()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_smooth.md)
  : Smooths thermal data

## Orthomosaic-based workflow

Correct at-sensor temperature orthomosaics

- [`ortho_correct()`](https://christophemetsu.github.io/theRmalUAV/reference/ortho_correct.md)
  : Atmospheric and emissivity correction for othomosaics

## Extra’s

Functions that do not directly belong to a workflow, but support or
improve understanding

- [`tuav_dji()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_dji.md)
  : Thermal SDK from DJI: corrections using DJI's algorithm
- [`sim_correct()`](https://christophemetsu.github.io/theRmalUAV/reference/sim_correct.md)
  : Simulating the correction
- [`get_temp_dirp_cpp()`](https://christophemetsu.github.io/theRmalUAV/reference/get_temp_dirp_cpp.md)
  : Thermal Image Temperature Extraction
