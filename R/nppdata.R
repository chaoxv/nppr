#' Test data for geom_oce()
#'
#' This data set contain the net primary production data of VGPM model in January 2021.
#'
#' @docType data
#'
#' @usage data(nppdata)
#'
#' @format An object of class \code{"data.frame"}
#' \describe{
#'  \item{lon}{lontitude}
#'  \item{lat}{latitude}
#'  \item{npp}{net primary prodution(units: mg C m^-2 d^-1)}
#' }
#' @references Data source: http://sites.science.oregonstate.edu/ocean.productivity/index.php
#' @keywords datasets
#' @examples
#'
#' data(nppdata)
#' head(nppdata)
#'
"nppdata"
