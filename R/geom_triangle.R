
##' @importFrom grid polygonGrob
##' @importFrom grid gpar

triangleGrob <- function(fill="red",col=NULL,slash="up",alpha=NULL, vp=NULL, name=NULL,...) {
  if(slash=="up"){
    x = c(0,0,1)
    y = c(0,1,1)
  } else if(slash=="down"){
    x = c(0,1,1)
    y = c(1,1,0)
  }
  polygonGrob(x,y, name=name, vp=vp,
              gp =gpar(fill=fill,
                       col=col,
                       alpha=alpha))
}


##' ggplot2 layer of triangle
##'
##'
##' @title geom_triangle
##' @param mapping aes mapping
##' @param data data
##' @param ... additional parameters
##' @return ggplot2 layer
##' @importFrom ggplot2 layer
##' @export
##' @author Shipeng Guo
geom_triangle <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTriangle,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

##' @importFrom grid viewport
##' @importFrom ggplot2 ggproto
##' @importFrom ggplot2 Geom
##' @importFrom ggplot2 draw_key_blank
##' @importFrom ggplot2 aes
GeomTriangle <- ggproto("GeomHeart", Geom,
                        draw_panel = function(data, panel_params, coord,slash="up") {
                          data <- coord$transform(data, panel_params)
                          data$size <- data$size/100
                          
                          grobs <- lapply(1:nrow(data), function(i) {
                            vp <- viewport(x=data$x[i], y=data$y[i],
                                           width=data$size[i], height=data$size[i],
                                           angle = data$angle[i],
                                           just = c("center", "center"),
                                           default.units = "native")
                            triangleGrob(vp=vp, 
                                         name=i,
                                         fill = data$fill[i],
                                         col = data$colour[i],
                                         alpha = data$alpha[i],
                                         slash=slash)
                          })
                          class(grobs) <- "gList"
                          ggplot2:::ggname("geom_triangle",gTree(children = grobs))
                        },
                        
                        default_aes = aes(colour = NA,fill="red", size = 9, linetype = 1,
                                          alpha = 1,angle=0,slash="up"),
                        required_aes = c("x", "y"),
                        draw_key = draw_key_blank
)
