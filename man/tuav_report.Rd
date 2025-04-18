% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/General.R
\name{tuav_report}
\alias{tuav_report}
\title{Make a TheRmalUAV report}
\usage{
tuav_report(
  thermal_uav,
  path = NA,
  project_name = "Thermal_uav",
  flight_name = "",
  pilot_name = "pilot_name",
  location = "location"
)
}
\arguments{
\item{thermal_uav}{\code{\link[=ThermalUAV-Class]{ThermalUAV}} object retrieved by previous functions (e.g.: \code{\link[=tuav_create]{tuav_create()}}).}

\item{path}{the path to the folder were you want to save the report. If NA the report will be saved in the path of the original tifs}

\item{project_name}{(character) the name of the project to put in the report}

\item{flight_name}{(character) the name of the flight to put in the report}

\item{pilot_name}{(character) the name of the pilot to put in the report}

\item{location}{(character) the location of where the flight was executed}
}
\value{
exports the ThermalData as tif files
}
\description{
\code{\link[=tuav_report]{tuav_report()}} writes a report with information about the processing done with this package
}
\seealso{
\code{\link[=tuav_export]{tuav_export()}}
}
