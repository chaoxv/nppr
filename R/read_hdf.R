#' read_hdf() can be used to read the ocean productivity(npp, sst, chl and par) hdf file in your R work environment
#'
#' @title read_hdf
#' @param file.path The file path of which one you want to import.
#' @importFrom raster raster
#' @importFrom raster extent
#' @importFrom raster projection
#'
#' @return A data frame
#' @export
#' @examples
#' \dontrun{
#' library(nppr)
#' library(raster)
#' librray(tidyverse)
#' read_hdf(file.path = 'C:\\Users\\xucha\\Desktop\\DATA\\cbpm.201506.hdf')
#' }

read_hdf <- function(file.path){

  ori_data <- raster::raster(file.path)

  raster::extent(ori_data) <- c(xmin = -180, xmax = 180, ymin = -90, ymax = 90)

  raster::projection(ori_data) <- '+init=epsg:4326'

  names(ori_data) <- 'var'

  new_data <- raster::as.data.frame(ori_data, xy = TRUE) %>%

    filter(var != -9999) %>%

    mutate(lon = .data$x, lat = .data$y) %>%

    select(.data$lon, .data$lat, .data$var)

  return(new_data)

}

