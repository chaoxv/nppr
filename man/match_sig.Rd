% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match_sig.R
\name{match_sig}
\alias{match_sig}
\title{match_sig}
\usage{
match_sig(file.path, lon, lat, date, time.span = "monthly")
}
\arguments{
\item{file.path}{The folder path where the ocean productivity data saved.}

\item{lon}{Your longitude.}

\item{lat}{Your latitude.}

\item{date}{Your date.}

\item{time.span}{The time span of your data. There two time spans: 'monthly' represent monthly data.
'dayly' represent 8 days data. The default is 'monthly'.}
}
\value{
A value
}
\description{
match_sig() is used to export single value from the download files
by your longitude, latitude and date
}
\examples{
\dontrun{
library(nppr)
library(lubridate)
librray(tidyverse)
match_sig(file.path = 'C:\\\\Users\\\\xucha\\\\Desktop\\\\DATA',
              lon = 120, lat = 20, date = '2017-09-01')
}
}
