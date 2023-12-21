#' match_df() is used to export multiple ocean productivity values from the download files
#' by your longitude, latitude and date
#'
#' @title match_df
#' @param data Your data, it should be a data frame or a tribble.
#' @param file.path The folder path where the ocean productivity data saved.
#' @param lon lon of match_sig
#' @param lat lat of match_sig
#' @importFrom tidyr unnest
#' @note The different between match_sig and match_df is that the input of match_sig is a value and
#' the input of match_df is a data frame.
#'
#' @return A data frame
#' @export
#' @examples
#' \dontrun{
#' library(nppr)
#' librray(tidyverse)
#' library(lubridate)
#' # Your data must contain both 'date' variables
#' match_df(mydata, file.path = 'C:\\Users\\xucha\\Desktop\\DATA')
#' }

match_df <- function(data,
                     file.path,
                     lon = NULL,
                     lat = NULL){

  mydata <- data %>% group_by(date) %>%
    mutate(var = match_sig(file.path = file.path, lon = .data$lon, lat = .data$lat, date = date))

return(mydata)

}
