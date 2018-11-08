## geom_cake


[<img src="http://guangchuangyu.github.io/blog_images/R/yyplot/cake_size_angle.png" width="600" />](https://guangchuangyu.github.io/cn/2017/12/geom-cake/)

<https://guangchuangyu.github.io/cn/2017/12/geom-cake/>

## geom_flat_violin

source code from <https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R>, authored by [David Robinson](https://github.com/dgrtwo).

Useful for creating [raincloud plot](https://micahallen.org/2018/03/15/introducing-raincloud-plots/).

Alternatively, you can use [ggridges](https://CRAN.R-project.org/package=ggridges) for the raincloud effect.

see also <https://peerj.com/preprints/27137v1/>.


## geom_ord_ellipse


```r
library(MASS)
ord <- lda(Species ~ ., iris, prior = rep(1, 3)/3)

## devtools::install_github('fawda123/ggord')
library(ggord)
p <- ggord(ord, iris$Species)
p + geom_ord_ellipse(ellipse_pro=0.99) +
   geom_ord_ellipse(ellipse_pro=0.9, color='black') 
```

![](https://user-images.githubusercontent.com/626539/48194813-fc22da00-e388-11e8-82a8-11fadb319e30.png)

