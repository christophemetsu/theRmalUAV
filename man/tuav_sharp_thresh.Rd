% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Clean.R
\name{tuav_sharp_thresh}
\alias{tuav_sharp_thresh}
\title{Get sharpness threshold}
\usage{
tuav_sharp_thresh(thermal_uav)
}
\arguments{
\item{thermal_uav}{\code{\link[=ThermalUAV-Class]{ThermalUAV}} object retrieved by previous functions (e.g.: \code{\link[=tuav_create]{tuav_create()}}). If ThermalData is empty, the raw TIFFs are loaded directly from the folder containing the tiff files.}
}
\value{
This function returns a sharpness value which can be set as threshold in the \code{\link[=tuav_reduc]{tuav_reduc()}} function
}
\description{
\code{\link[=tuav_sharp_thresh]{tuav_sharp_thresh()}} estimates sharpness by iteratively asking whether an image is sharp enough, until it is sharp enough. The goal is to set your own sharpness threshold which is used in the image reduction function.
}
