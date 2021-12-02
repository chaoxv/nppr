#' get_chla() is used for automatically downloadiing, decompressing and renaming
#' sea chl data by custom grid size, time
#' span and satellite.
#'
#' @title get_sst
#' @author Chao Xu
#' @param file.path The folder(an empty folder) path where your want to
#' save your file (avoid Chinese characters).
#' @param grid.size The grid size that you choose. There are two grid sizes can be choosed:
#' 'low'(default): 2160x1080, 'high': 2160x4320.
#' @param time.span The time span of npp data. There two time spans:
#' 'monthly' represent monthly npp data. 'dayly' represent 8 days data.
#' @param satellite Choose satellites, 'MODIS' and 'VIIRS'. The default is 'MODIS'.
#' @param mindate The minimum date of data you want to download.
#' @param maxdate The maximum date of data you want to download.
#' @note units: mg m^-3

#' @return download some files in your folder.
#' @export
#' @examples
#' \dontrun{
#' library(nppr)
#' library(RCurl)
#' library(XML)
#' library(R.utils)
#' library(tidyverse)
#' library(lubridate)
#' get_chla(file.path = 'C:\\Users\\xucha\\Desktop\\DATA',
#'         mindate = '2016-02-04', maxdate ='2016-06-28')
#' }

get_chla <- function(file.path,
                     grid.size = 'low',
                     time.span = 'monthly',
                     satellite = 'MODIS',
                     mindate,
                     maxdate){

  grid <- if_else(grid.size == 'high', '2x4/', '1x2/')
  time <- if_else(time.span == 'dayly', '8day/', 'monthly/')
  format <- 'hdf/'
  sate <- if_else(satellite == 'MODIS', 'chl.modis.r2018/',
                  if_else(satellite == 'VIIRS', 'chl.viirs.r2018.gsm/', NULL))

  path <- paste0('orca.science.oregonstate.edu/data/', grid, time, sate, format)
  url <- getURL(path)
  filename <- getHTMLLinks(url)

  minyear <- year(mindate)
  maxyear <- year(maxdate)

  if(time.span == 'monthly'){
    minday <- yday(mindate)
    maxday <- yday(maxdate) + 31
  }

  if(time.span == 'dayly'){
    minday <- yday(mindate)
    maxday <- yday(maxdate) + 10
  }

  min_date <- as.numeric(ifelse(minday < 10, paste0(minyear, '00', minday),
                                 ifelse(minday < 100, paste0(minyear, '0', minday), paste0(minyear, minday))))
  max_date <- as.numeric(ifelse(maxday < 10, paste0(maxyear, '00', maxday),
                                 ifelse(maxday < 100, paste0(maxyear, '0', maxday), paste0(maxyear, maxday))))

  name <- filename %>% str_extract('[[:digit:]]+') %>%
    as.numeric() %>% as.data.frame() %>% na.omit() %>%
    rename(name = '.') %>% filter(name > 3000) %>%
    filter(name >= min_date & name <= max_date) %>%
    group_by(name) %>%
    mutate(name1 = paste0('chl.', name, '.hdf', '.gz'),
           dowopath = paste0('http://', path, .data$name1),
           savepath = paste0(file.path, '/', .data$name1))

    name %>% pwalk(~download.file(..3, destfile = ..4))

    dir(file.path, full.names = T) %>%
      map(~gunzip(., remove = T))

    xc_file_rename3 <- function(oldname){
      oridate <- as.numeric(str_extract(oldname, '[[:digit:]]+'))
      year <- oridate %/% 1000
      month <- month(as.Date(oridate - 1000*year, origin = paste0(year, '-01-01')))
      month1 <- ifelse(month >= 10, month, paste0('0', month))
      newname <- paste0(file.path,'/', year, month1, '.hdf')
   return(newname)
  }

   xc_file_rename4 <- function(oldname){
      oridate <- as.numeric(str_extract(oldname, '[[:digit:]]+'))
      newname <- paste0(file.path,'/', oridate, '.hdf')
   return(newname)
  }

    if(time.span == 'monthly'){
      dir(file.path, full.names = T) %>%
      file.rename(., xc_file_rename3(.))
    }

    if(time.span == 'dayly'){
      dir(file.path, full.names = T) %>%
      file.rename(., xc_file_rename4(.))
    }
}
