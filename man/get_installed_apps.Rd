% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sensor_functions.R
\name{get_installed_apps}
\alias{get_installed_apps}
\title{Get installed apps}
\usage{
get_installed_apps(db, participant_id = NULL)
}
\arguments{
\item{db}{A database connection to a mpathsenser database.}

\item{participant_id}{A character string identifying a single participant. Use
\code{\link[mpathsenser]{get_participants}} to retrieve all participants from the database.
Leave empty to get data for all participants.}
}
\value{
A tibble containing app names.
}
\description{
Extract installed apps for one or all participants. Contrarily to other get_* functions in
this package, start and end dates are not used since installed apps are assumed to be fixed
throughout the study.
}
