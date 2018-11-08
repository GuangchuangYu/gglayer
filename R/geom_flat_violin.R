## source code from https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R

## somewhat hackish solution to:
## https://twitter.com/EamonCaddigan/status/646759751242620928
## based mostly on copy/pasting from ggplot2 geom_violin source:
## https://github.com/hadley/ggplot2/blob/master/R/geom-violin.r


##' flat violin geom
##'
##' 
##' @title geom_flat_violin
##' @param mapping aesthetic mapping
##' @param data the data to display in this layer
##' @param stat The statistical transformation to use on the data for this layer, as a string.
##' @param position position adjustment
##' @param trim If ‘TRUE’ (default), trim the tails of the violins to the range of the data. If ‘FALSE’, don't trim the tails.
##' @param scale if "area" (default), all violins have the same area (before trimming the tails). If "count", areas are scaled proportionally to the number of observations. If "width", all violins have the same maximum width.
##' @param show.legend whether show the legend of this layer
##' @param inherit.aes whether inherit aesthetic mapping from `ggplot`
##' @param ... additional parameters
##' @return ggplot layer
##' @importFrom dplyr group_by
##' @export
##' @author David Robinson
##' @examples
##' library(ggplot2)
##' ggplot(diamonds, aes(cut, carat)) +
##'    geom_flat_violin() +
##'    coord_flip()
geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                        position = "dodge", trim = TRUE, scale = "area",
                        show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

#' @format NULL
#' @usage NULL
#' @importFrom dplyr mutate
#' @importFrom ggplot2 draw_key_polygon
GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
            
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))

            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])

            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },

          draw_key = draw_key_polygon,

          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),

          required_aes = c("x", "y")
)

"%||%" <- getFromNamespace("%||%", "ggplot2")
"%>%"  <- getFromNamespace("%>%", "magrittr")
