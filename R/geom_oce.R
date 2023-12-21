#' geom_oce() is used for ocean data visualization
#'
#' @title geom_oce
#' @param data The data for drawing a plot
#' @param mapping Mapping for aes()
#' @param lonlim The lontitude limits: lonlim = c()
#' @param latlim The latitude limits: latlim = c()
#' @importFrom ggspatial annotation_scale
#' @importFrom ggspatial north_arrow_orienteering
#' @importFrom ggspatial annotation_north_arrow
#' @importFrom ggplot2 geom_raster
#' @importFrom ggplot2 geom_sf
#' @importFrom ggplot2 coord_sf
#' @importFrom ggplot2 unit
#' @importFrom ggplot2 guide_colorbar
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#'
#' @return A plot
#' @export
#' @examples
#' \dontrun{
#' library(nppr)
#' library(ggplot2)
#' library(ggspatial)
#' library(rnaturalearth)
#' library(rnaturalearthdata)
#' data(nppdata)
#' ggplot()+
#'   geom_oce(nppdata, aes(x = lon, y = lat, fill = npp),
#'   lonlim = c(100, 120), latlim = c(7, 25))+
#'   scale_fill_gradientn(colors = rev(rainbow(20)), breaks = seq(200, 1000, 100),
#'   limits = c(200, 1000))+
#'   labs(x = 'Longitude', y = 'Latitude',
#'   fill = expression(NPP*~'('*mg~C~m^-2~d^-1*')'))
#' }

geom_oce <- function(data = NULL,
                     mapping = NULL,
                     lonlim,
                     latlim){

  world_shp <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  p <- list(

  geom_raster(data = data, mapping = mapping),

  geom_sf(data = world_shp, fill = '#F4CDA5', col = 'black', size = .1),

  coord_sf(xlim = lonlim, ylim = latlim, expand = T),

  annotation_scale(style = 'ticks', location = "bl", width_hint = .1,

                   bar_cols = c('white', 'grey'),

                   text_cex = 1, pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")),

  annotation_north_arrow(location = 'tl', which_north = "true",

                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),

                         style = north_arrow_orienteering),

  guides(fill = guide_colorbar(barwidth = 1,

                              barheight = 21.4,

                              reverse = T,

                              ticks.colour = 'transparent',

                              frame.colour = "black",

                              frame.linewidth = .5,

                              title.position = 'right')),

  theme_bw(18),

  theme(panel.ontop = TRUE,

        panel.background = element_rect(fill = "transparent"),

        panel.grid = element_blank(),

        legend.key = element_rect(size = 10),

        legend.key.height = unit(1.7, 'cm'),

        legend.title = element_text(angle = 90),

        legend.title.align = 0.5))

  return(p)
}
