% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/General.R
\name{tuav_create}
\alias{tuav_create}
\title{Get all the necessary info and create a ThermalUAV object}
\usage{
tuav_create(
  path,
  camera = "ThermalCapture",
  meta_csv = NA,
  flight_height = NA,
  tz = NA
)
}
\arguments{
\item{path}{The path to the folder containing the TIFF files, or path to 1 TIFF file}

\item{camera}{(character) Indicate your camera name, you can check through \code{\link[=tuav_cameras]{tuav_cameras()}}}

\item{meta_csv}{The Path to an additional meta data can be provided in the form of a csv, if not provided all info will be derived from exif data (which may limit some function options)}

\item{flight_height}{(numerical) the height between the CAMERA and the GROUND. can be a single value or a vector of the same length as the number of images. If not specified it will search for GPS altitude in the exif/meta data}

\item{tz}{The timezone of the flight. Important if you want to correct the data using a meteorological dataset. If not provided (default = NA), the function will use the system's timezone through Sys.timezone()}
}
\value{
A \code{\link[=ThermalUAV-Class]{ThermalUAV}} object with all the necessary information, serves as input in the following functions
}
\description{
\code{\link[=tuav_create]{tuav_create()}} gets all the information needed for further analysis and creates a \code{\link[=ThermalUAV-Class]{ThermalUAV}} object
}
