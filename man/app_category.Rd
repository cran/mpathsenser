% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sensor_functions.R
\name{app_category}
\alias{app_category}
\title{Find the category of an app on the Google Play Store}
\usage{
app_category(name, num = 1, rate_limit = 5)
}
\arguments{
\item{name}{The name of the app to search for.}

\item{num}{Which result should be selected in the list of search results. Defaults to one.}

\item{rate_limit}{The time interval to keep between queries, in seconds. If the rate limit is too
low, the Google Play Store may reject further requests or even ban your entirely.}
}
\value{
A list containing the following fields:

\tabular{ll}{
package \tab the package name that was selected from the Google Play search \cr
genre   \tab the corresponding genre of this package
}
}
\description{
This function scrapes the Google Play Store by using \code{name} as the search term. From there
it selects the first result in the list and its corresponding category and package name.
}
\section{Warning}{

Do not abuse this function or you will be banned by the Google Play Store. The minimum delay
between requests seems to be around 5 seconds, but this is untested. Also make sure not to do
batch lookups, as many subsequent requests will get you blocked as well.
}

\examples{
app_category('whatsapp')

# Example of a generic app name where we can't find a specific app
app_category('weather') # Weather forecast channel

# Get OnePlus weather
app_category('net.oneplus.weather')
}
