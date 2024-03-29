% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match_df.R
\name{match_df}
\alias{match_df}
\title{match_df}
\usage{
match_df(data, file.path, lon = NULL, lat = NULL)
}
\arguments{
\item{data}{Your data, it should be a data frame or a tribble.}

\item{file.path}{The folder path where the ocean productivity data saved.}

\item{lon}{lon of match_sig}

\item{lat}{lat of match_sig}
}
\value{
A data frame
}
\description{
match_df() is used to export multiple ocean productivity values from the download files
by your longitude, latitude and date
}
\note{
The different between match_sig and match_df is that the input of match_sig is a value and
the input of match_df is a data frame.
}
\examples{
\dontrun{
library(nppr)
librray(tidyverse)
library(lubridate)
# Your data must contain both 'date' variables
match_df(mydata, file.path = 'C:\\\\Users\\\\xucha\\\\Desktop\\\\DATA')
}
}
