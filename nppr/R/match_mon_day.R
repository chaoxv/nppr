#' match_mon_day() is used to export multiple ocean productivity values from the download files
#' by your longitude, latitude and date
#'
#' @title match_mon_day
#' @param data Your npp data, it can be a data frame or a tribble.
#' @param file.path The folder path where the ocean productivity data saved.
#' @param time.span The time span of your data. There two time spans: 'monthly' represent monthly data.
#' 'dayly' represent 8 days data. The default is 'monthly'.
#' @importFrom tidyr unnest
#' @note This function should only be used when you have the 'month' variable.
#' Any other time, match_sig should be used. Please see examples.
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
#' mutate(var = map(data, ~match_mon_day(data, file.path = 'C:\\Users\\xucha\\Desktop\\DATA'))) %>%
#' unnest(var) %>% select(date, var))
#' }

match_mon_day <- function(data,
                          file.path,
                          time.span = 'monthly'){

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
    filename <- paste0(file.path, '/', .data$newday, '.hdf')
  }

  new_data <- read_hdf(filename)

  match_sig_month <- function(data,
                              lon = NULL,
                              lat = NULL,
                              var = NULL){

  lon1 <- unique(data$lon)
  lat1 <- unique(data$lat)

  lat2 <- min(new_data$lat[new_data$lat >= lat1])
  lon2 <- min(new_data$lon[new_data$lon >= lon1])

  var <- filter(new_data, lat == lat2 & lon == lon2)$var

  return(var)

}

  mydata1 <- data %>% group_by(date) %>% nest() %>% mutate(var = purrr::map(data, ~match_sig_month(.))) %>%
    unnest(c(date, .data$var)) %>% select(date, .data$var)

return(mydata1)

}
