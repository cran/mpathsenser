% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sensor_functions.R
\name{link2}
\alias{link2}
\title{Link two sensors OR one sensor and an external data frame}
\usage{
link2(
  db,
  sensor_one,
  sensor_two = NULL,
  offset,
  participant_id = NULL,
  start_date = NULL,
  end_date = NULL,
  external = NULL,
  reverse = FALSE,
  ignore_large = FALSE
)
}
\arguments{
\item{db}{A database connection to an m-Path Sense database.}

\item{sensor_one}{The name of a primary sensor. See \link[mpathsenser]{sensors} for a list of
available sensors.}

\item{sensor_two}{The name of a secondary sensor. See \link[mpathsenser]{sensors} for a list of
available sensors. Cannot be used together with \code{external}.}

\item{offset}{The time window in which y is to be matched to x. Must be convertible to a period
by \link[lubridate]{as.period}.}

\item{participant_id}{A character string identifying a single participant. Use
\code{\link[mpathsenser]{get_participants}} to retrieve all participants from the database.
Leave empty to get data for all participants.}

\item{start_date}{Optional search window specifying date where to begin search. Must be
convertible to date using \link[base]{as.Date}. Use \link[mpathsenser]{first_date} to find the
date of the first entry for a participant.}

\item{end_date}{Optional search window specifying date where to end search. Must be convertible
to date using \link[base]{as.Date}. Use \link[mpathsenser]{last_date} to find the date of the
last entry for a participant.}

\item{external}{Optionally, specify an external data frame. Cannot be used at the same time as
a second sensor. This data frame must have a column called \code{time}.}

\item{reverse}{Switch \code{sensor_one} with either \code{sensor_two} or \code{external}?
Particularly useful in combination with \code{external}.}

\item{ignore_large}{Safety override to prevent long wait times. Set to \code{TRUE} to do this
function on lots of data.}
}
\value{
A tibble with the data of \code{sensor_one} with a new column \code{data} with the
matched data of either \code{sensor_two} or \code{external} according to \code{offset}. The
other way around when \code{reverse = TRUE}.
}
\description{
This function is specific to mpathsenser databases. It is a wrapper around
\link[mpathsenser]{link} but extracts data in the database for you.
}
\seealso{
\code{\link[mpathsenser]{link}}
}
