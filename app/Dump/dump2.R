
# options(rgl.useNULL = TRUE)
library(rgl)
library(x3ptools)
library(bulletxtrctr)
# bull <- read_bullet("~/Desktop/bull")

bull <- read_bullet("~/Desktop/csafe/bullet/bulletanalyzer/dump/bull")
bull$x3p <- lapply(bull$x3p,x3p_m_to_mum)
bull$x3p <- lapply(bull$x3p,function(x) y_flip_x3p(rotate_x3p(x,angle = -90)))
bull$x3p <- lapply(bull$x3p,function(x) x3p_add_hline(x,yintercept = 375, size = 10, color = "#e6bf98"))
x3p_scaled <- x3p_interpolate(bull$x3p[[1]],resx=8)
image_x3p(x3p_scaled,zoom=.75)


# snapshot3d("~/Downloads/test.png")













options(rgl.useNULL = TRUE)
library(rgl)
library(x3ptools)
library(bulletxtrctr)
bull <- read_bullet("Dump/bull")
# bull <- read_bullet("~/Desktop/csafe/bullet/bulletanalyzer/dump/bull")
bull$x3p <- lapply(bull$x3p,x3p_m_to_mum)
bull$x3p <- lapply(bull$x3p,function(x) y_flip_x3p(rotate_x3p(x,angle = -90)))
bull$crosscut <- sapply(bull$x3p,x3p_crosscut_optimize)
bull$x3p <- lapply(bull$x3p,function(x) x3p_add_hline(x,yintercept = 51.5625, size = 20, color = "#ea2b1f"))
# x3p_scaled <- x3p_interpolate(bull$x3p[[1]],resx=8)
x3p_sampled <- x3p_sample(bull$x3p[[1]],m=5)
image_x3p(x3p_sampled,zoom=.75)


# snapshot3d("~/Downloads/test.png")

