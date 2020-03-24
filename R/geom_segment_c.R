##' geom_segment_c supports coloring segment with continuous colors
##'
##'
##' @title geom_segment_c
##' @param mapping aes mapping
##' @param data data
##' @param position position
##' @param lineend lineend
##' @param na.rm logical
##' @param show.legend logical
##' @param inherit.aes logical
##' @param arrow specification for arrow heads, as created by arrow().
##' @param arrow.fill fill color to usse for the arrow head (if closed). `NULL` means use `colour` aesthetic.
##' @param ... additional parameter
##' @importFrom ggplot2 layer
##' @export
##' @seealso
##' \link[ggplot2]{geom_segment}
##' @return add segment layer
##' @author Guangchuang Yu
geom_segment_c <- function(mapping = NULL, data = NULL, 
                           position = 'identity', lineend = "butt",
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                           arrow = NULL, arrow.fill = NULL, 
                           ...) {

    structure(list(
        data = data,
        mapping = mapping,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            arrow = arrow,
            lineend = lineend,
            na.rm = na.rm,
            ...
        )
    ), class = "segmentC")
}

##' @importFrom ggplot2 ggplot_add
##' @method ggplot_add segmentC
##' @importFrom ggplot2 aes_string
##' @export
ggplot_add.segmentC <- function(object, plot, object_name) {
    if (object$inherit.aes) {
        mapping <- modifyList(plot$mapping, object$mapping)
    } else {
        mapping <- object$mapping
    }

    v <- rvcheck::get_aes_var(mapping, "col1")

    mapping <- object$mapping
    # mapping["colour"] <- list(v)

    default_aes <- aes_string(colour=v)
    if (is.null(mapping)) {
        mapping <- default_aes
    } else {
        mapping <- modifyList(mapping, default_aes)
    }

    ly <- layer(
        data = object$data,
        mapping = mapping,
        stat = StatSegmentC,
        geom = "segment",
        position = object$position,
        show.legend = object$show.legend,
        inherit.aes = object$inherit.aes,
        params = object$params,
        check.aes = FALSE
    )

    ggplot_add(ly, plot, object_name)
}


StatSegmentC <- ggproto("StatSegmentC", Stat,
                        required_aes = c("x", "y", "xend", "yend", "col0", "col1"),
                        compute_group = function(data, params) {
                            data
                        },
                        compute_panel = function(self, data, scales, params, lineend, extend = 0.002) {
                            setup_data_continuous_color_df(data, nsplit = 20, extend = extend)
                        }
                        )





setup_data_continuous_color_df <- function(df, nsplit = 100, extend = 0.002, pool = FALSE) {
    if (pool) {
        rr <- range(df$x)
        if (nrow(df) == 1)
            rr <- c(df$x, df$xend)        
    }

    lapply(1:nrow(df), function(i) {
        if (!pool)
            rr <- c(df$x[i], df$xend[i])
        df2 <- setup_data_continuous_color(x = df$x[i],
                                           xend = df$xend[i],
                                           y = df$y[i],
                                           yend = df$yend[i],
                                           col = df$col0[i],
                                           col2 = df$col1[i],
                                           xrange = rr,
                                           nsplit = nsplit,
                                           extend = extend)

        res <- lapply(df[i,, drop = FALSE], rep, each = nrow(df2)) %>%
            do.call('cbind', .) %>% as.data.frame
        res$x <- df2$x
        res$xend <- df2$xend
        res$y <- df2$y
        res$yend <- df2$yend
        res$colour <- df2$col
        return(res)
    }) %>% do.call('rbind', .)
}


## setup_data_continuous_color <- getFromNamespace("setup_data_continuous_color", "ggtree")


setup_data_continuous_color <- function(x, xend, y, yend, col, col2,
                                        xrange = NULL, nsplit = 100, extend = 0.002) {
    if (is.null(xrange))
        xrange <- c(x, xend)

    ## xstep <- diff(xrange)/nsplit
    ## xn <- floor((xend - x)/xstep)
    xn <- floor((xend - x) * nsplit /diff(xrange))
    ## slope <- (yend - y)/(xend - x)
    ydiff <- yend - y
    xdiff <- xend - x

    if (xn > 0) {
        ## x <- x + 0:xn * xstep
        x <- x + 0:xn * diff(xrange) / nsplit 
        tmp <- x[-1] * (1 + extend)
        tmp[tmp > xend] <- xend
        xend <- c(tmp, xend)
        ## y <- y + 0:xn * xstep * slope
        y <- y + 0:xn * diff(xrange) * ydiff / (nsplit * xdiff)
        ## yend <- y + (xend - x) * slope
        yend <- y + (xend - x) * ydiff / xdiff 
    }

    n <- length(x)
    if (is.numeric(col) && is.numeric(col2)) {
        colour <- seq(col, col2, length.out = n)
    } else if (is.character(col) && is.character(col2)) {
        colour <- grDevices::colorRampPalette(c(col, col2))(n)
    } else {
        stop("col and col2 should be both numeric or character..." )
    }

    data.frame(x = x,
               xend = xend,
               y = y,
               yend = yend,
               colour = colour)
}


