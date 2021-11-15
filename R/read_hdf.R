#' read_hdf() can be used to read the ocean productivity(npp, sst, chl and par) hdf file in your R work environment
#'
#' @title read_hdf
#' @param file.path The file path of which one you want to import.
#' @importFrom raster raster
#'
#' @return A data frame
#' @export
#' @examples
#' \dontrun{
#' library(nppr)
#' library(raster)
#' librray(tidyverse)
#' read_hdf(data, file.path = 'C:\\Users\\xucha\\Desktop\\DATA\\cbpm.201506.hdf')
#' }

read_hdf <- function(file.path){

  ori_data <- raster(file.path)

  extent(ori_data) <- c(xmin = -180, xmax = 180, ymin = -90, ymax = 90)

  projection(ori_data) <- '+init=epsg:4326'

  names(ori_data) <- 'var'

  ori_data[ori_data == -9999] <- NA

  new_data <- raster::as.data.frame(ori_data, xy = TRUE) %>%

    na.omit() %>%

    mutate(lon = round(x, 2), lat = round(y, 2)) %>%

    select(lon, lat, var)

  return(new_data)

}
