#' match_sig() is used to export single value from the download files
#' by your longitude, latitude and date
#'
#' @title match_sig
#' @param file.path The folder path where the ocean productivity data seved.
#' @param lon Your longitude.
#' @param lat Your latitude.
#' @param date Your date.
#' @param time.span The time span of npp data. There two time spans: 'monthly' represent monthly npp data.
#' 'dayly' represent 8 days data. The default is 'monthly'.
#'
#' @return A value
#' @export
#' @examples
#' \dontrun{
#' library(nppr)
#' library(lubridate)
#' librray(tidyverse)
#' match_sig(file.path = 'C:\\Users\\xucha\\Desktop\\DATA',
#'               lon = 120, lat = 20, date = '2017-09-01')
#' }


match_sig <- function(file.path,
                      lon,
                      lat,
                      date,
                      time.span = 'monthly'){
  year <- year(date)

  month <- month(date)

  day <- yday(date)

  new_day <- ifelse(day >= 100, day, ifelse(day >=10 & day < 100,
                                                       paste0('0', day), paste0('00', day)))

  month1 <- ifelse(month >= 10, month, paste0('0', month))

  if(time.span == 'monthly'){

    filename <- paste0(file.path, '/', year, month1, '.hdf')
  }

  if(time.span == 'dayly'){

    filename <- paste0(file.path, '/', newday, '.hdf')
  }

  new_data <- read_hdf(filename)

  lat2 <- min(new_data$lat[new_data$lat >= lat])

  lon2 <- min(new_data$lon[new_data$lon >= lon])

  var <- filter(new_data, lat == lat2 & lon == lon2)$var

  return(var)
}

