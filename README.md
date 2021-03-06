## geom_cake


[<img src="http://guangchuangyu.github.io/blog_images/R/yyplot/cake_size_angle.png" width="600" />](https://guangchuangyu.github.io/cn/2017/12/geom-cake/)

<https://guangchuangyu.github.io/cn/2017/12/geom-cake/>


## geom_ord_ellipse

[![](https://guangchuangyu.github.io/blog_images/R/yyplot/geom_ord_ellipse_files/figure-markdown_strict/unnamed-chunk-1-2.png)](https://guangchuangyu.github.io/cn/2018/01/geom-ord-ellipse/)

<https://guangchuangyu.github.io/cn/2018/01/geom-ord-ellipse/>

## geom_segment_c

see also <https://yulab-smu.github.io/treedata-book/chapter4.html#fig:continuousColor>.

```r
set.seed(2019-06-28)
d = data.frame(x = rnorm(10),
        xend = rnorm(10),
        y = rnorm(10),
        yend = rnorm(10),
        v1 = rnorm(10),
        v2 = rnorm(10))

library(ggplot2)
library(gglayer)

ggplot(d) + geom_segment_c(aes(x = x, xend = xend, y=y, yend =yend, col0 = v1, col1 = v2)) +
    scale_color_viridis_c(name = "continuous colored lines") + 
    theme_minimal() + theme(legend.position=c(.2, .85)) + xlab(NULL) + ylab(NULL)
```


[<img src="figures/segment-continuous.png" width="600" />](https://yulab-smu.github.io/treedata-book/chapter4.html#fig:continuousColor)
