% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database.R
\name{copy_db}
\alias{copy_db}
\title{Copy (a subset of) a database to another database}
\usage{
copy_db(
  from_db,
  to_db = NULL,
  sensor = "All",
  path = getwd(),
  db_name = "sense.db"
)
}
\arguments{
\item{from_db}{A mpathsenser database connection from where the data will be transferred.}

\item{to_db}{A mpathsenser database connection where the data will be transferred to. If no
new_db is specified, a path (and possibly a db_name) must be specified for
\link[mpathsenser]{create_db} to create a new database.}

\item{sensor}{A character vector containing one or multiple sensors. See
\code{\link[mpathsenser]{sensors}} for a list of available sensors. Use "All" for all available
sensors.}

\item{path}{The path to the database. Use NULL to use the full path name in db_name.}

\item{db_name}{The name of the database.}
}
\value{
No return value, called for side effects.
}
\description{
Copy (a subset of) a database to another database
}
