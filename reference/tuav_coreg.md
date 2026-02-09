# Co-register locations with another camera

`tuav_coreg()` corrects the GPS-info of the thermal data with more
accurate GPS information of a coregistered (high resolution) camera.
Could also be that the cameras of this coregistered camera are already
aligned in a structure form motion software.

## Usage

``` r
tuav_coreg(
  thermal_uav,
  opt_cameras = NA,
  rig_offset = c(0, 0, 0, 0, 0, 0),
  timediff = 0
)
```

## Arguments

- thermal_uav:

  [`ThermalUAV`](https://christophemetsu.github.io/theRmalUAV/reference/ThermalUAV-Class.md)
  object retrieved by previous functions (e.g.:
  [`tuav_create()`](https://christophemetsu.github.io/theRmalUAV/reference/tuav_create.md)).

- opt_cameras:

  (data.frame) containing the optimized camera positions. In the right
  format, can be prepared with the
  [`coreg_prep()`](https://christophemetsu.github.io/theRmalUAV/reference/coreg_prep.md)
  function

- rig_offset:

  (numerical) vector containing the different offsets of the thermal
  camera compared to the reference optimized camera with following
  offsets in mm or ° (x, y, z, yaw, pitch, roll). Default is a vector of
  zeros

- timediff:

  (numerical) if there is a time difference (in seconds) between the
  optimized camera set and the thermal data, it can be specified here,
  default is 0

## Value

This function returns an updated
[`ThermalUAV`](https://christophemetsu.github.io/theRmalUAV/reference/ThermalUAV-Class.md)
object containing optimized thermal camera positions

## See also

[`coreg_prep()`](https://christophemetsu.github.io/theRmalUAV/reference/coreg_prep.md)
