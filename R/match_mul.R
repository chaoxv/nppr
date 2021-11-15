#' match_mon_day() is used to export multiple ocean productivity values from the download files
#' by your longitude, latitude and date
#'
#' @title match_mon_day
#' @param data Your npp data, it can be a data frame or a tribble.
#' @param file.path The folder path where the npp data seved.
#' @importFrom tidyr unnest
#' @note This function should only be used when you have the 'month' variable.
#' Any other time, match_sig and match_df should be used. Please see examples.
#'
#' @return A data frame
#' @export
#' @examples
#' \dontrun{
#' library(nppr)
#' librray(tidyverse)
#' library(lubridate)
#' # Your data must contain both 'date' and 'month' variables
#' mydata %>% mutate(month = as.yearmon(format(date, '%Y-%m'))) %>% # create 'month' variable
#' group_by(month) %>% nest() %>%
#' mutate(npp = map(data, ~match_mon_day(data, file.path = 'C:\\Users\\xucha\\Desktop\\DATA'))) %>%
#' unnest(npp) %>% select(date, npp))
#' }

match_mon_day <- function(data, file.path){

  year <- unique(data$year)
  month <- unique(data$month)
  month1 <- ifelse(month >= 10, month, paste0('0', month))
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

  npp_data <- read_hdf(filename)

  match_sig_month <- function(data){

  lon1 <- unique(data$lon)
  lat1 <- unique(data$lat)

  lat2 <- min(npp_data$lat[npp_data$lat >= lat1])
  lon2 <- min(npp_data$lon[npp_data$lon >= lon1])

  npp <- filter(npp_data, lat == lat2 & lon == lon2)$npp

  return(npp)

}

  mydata1 <- data %>% group_by(date) %>% nest() %>% mutate(npp = map(data, ~xc_match_sigmonth(.))) %>%
    unnest(c(date, npp)) %>% select(date, npp)

return(mydata1)

}
