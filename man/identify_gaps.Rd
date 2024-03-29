% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sensor_functions.R
\name{identify_gaps}
\alias{identify_gaps}
\title{Identify gaps in mpathsenser mobile sensing data}
\usage{
identify_gaps(
  db,
  participant_id = NULL,
  min_gap = 60,
  sensor = "Accelerometer"
)
}
\arguments{
\item{db}{A database connection to an m-Path Sense database.}

\item{participant_id}{A character string identifying a single participant. Use
\code{\link[mpathsenser]{get_participants}} to retrieve all participants from the database.
Leave empty to get data for all participants.}

\item{min_gap}{The minimum time (in seconds) passed between two subsequent measurements for it to
be considered a gap.}

\item{sensor}{One or multiple sensors. See \link[mpathsenser]{sensors} for a list of available
sensors.}
}
\value{
A tibble containing the time period of the gaps. The structure of this tibble is as
follows:

\tabular{ll}{ participant_id \tab the \code{participant_id} of where the gap occurred \cr from
\tab the time of the last measurement before the gap \cr to             \tab the time of the
first measurement after the gap \cr gap            \tab the time passed between from and to, in
seconds }
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Oftentimes in mobile sensing, gaps appear in the data as a result of the participant
accidentally closing the app or the operating system killing the app to save power. This can
lead to issues later on during data analysis when it becomes unclear whether there are no
measurements because no events occurred or because the app quit in that period. For example, if
no screen on/off event occur in a 6-hour period, it can either mean the participant did not
turn on their phone in that period or that the app simply quit and potential events were
missed. In the latter case, the 6-hour missing period has to be compensated by either removing
this interval altogether or by subtracting the gap from the interval itself (see examples).
}
\details{
While any sensor can be used for identifying gaps, it is best to choose a sensor with a
very high, near-continuous sample rate such as the accelerometer or gyroscope. This function
then creates time between two subsequent measurements and returns the period in which this time
was larger than \code{min_gap}.

Note that the \code{from} and \code{to} columns in the output are character vectors in UTC
time.
}
\section{Warning}{
 Depending on the sensor that is used to identify the gaps (though this is
typically the highest frequency sensor, such as the accelerometer or gyroscope), there may be a
small delay between the start of the gap and the \emph{actual} start of the gap. For example, if the
accelerometer samples every 5 seconds, it may be after 4.99 seconds after the last
accelerometer measurement (so just before the next measurement), the app was killed. However,
within that time other measurements may still have taken place, thereby technically occurring
"within" the gap. This is especially important if you want to use these gaps in
\code{\link[mpathsenser]{add_gaps}} since this issue may lead to erroneous results.

An easy way to solve this problem is by taking into account all the sensors of interest when
identifying the gaps, thereby ensuring there are no measurements of these sensors within the
gap. One way to account for this is to (as in this example) search for gaps 5 seconds longer
than you want and then afterwards increasing the start time of the gaps by 5 seconds.
}

\examples{
\dontrun{
# Find the gaps for a participant and convert to datetime
gaps <- identify_gaps(db, "12345", min_gap = 60) |>
  mutate(across(c(to, from), ymd_hms)) |>
  mutate(across(c(to, from), with_tz, "Europe/Brussels"))

# Get some sensor data and calculate a statistic, e.g. the time spent walking
# You can also do this with larger intervals, e.g. the time spent walking per hour
walking_time <- get_data(db, "Activity", "12345") |>
  collect() |>
  mutate(datetime = ymd_hms(paste(date, time))) |>
  mutate(datetime = with_tz(datetime, "Europe/Brussels")) |>
  arrange(datetime) |>
  mutate(prev_time = lag(datetime)) |>
  mutate(duration = datetime - prev_time) |>
  filter(type == "WALKING")

# Find out if a gap occurs in the time intervals
walking_time |>
  rowwise() |>
  mutate(gap = any(gaps$from >= prev_time & gaps$to <= datetime))
}
}
