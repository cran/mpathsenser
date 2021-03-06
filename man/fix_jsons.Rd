% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{fix_jsons}
\alias{fix_jsons}
\title{Fix the end of JSON files}
\usage{
fix_jsons(path = getwd(), files = NULL, recursive = TRUE, parallel = FALSE)
}
\arguments{
\item{path}{The path name of the JSON files.}

\item{files}{Alternatively, a character list of the input files}

\item{recursive}{Should the listing recurse into directories?}

\item{parallel}{A logical value whether you want to check in parallel. Useful for a lot of files.}
}
\value{
A message indicating how many files were fixed.
}
\description{
When copying data directly coming from m-Path Sense, JSON files are sometimes corrupted due to
the app not properly closing them. This function attempts to fix the most common
problems associated with improper file closure by m-Path Sense.
}
\section{Progress}{

You can be updated of the progress by this function by using the
\code{\link[progressr]{progress}} package. See \code{progressr}'s
\href{https://cran.r-project.org/package=progressr/vignettes/progressr-intro.html}{vignette}
on how to subscribe to these updates.
}

\examples{
\dontrun{
future::plan(future::multisession)
files <- test_jsons()
fix_jsons(files = files)
}
}
