library(dplyr)
library(bulletxtrctr)
library(x3ptools)
library(rgl)
library(randomForest)
library(ggplot2)
library(readr)
library(nbtrd)


cts <- read_bullet("/Users/stephanie/Documents/cts_set/scans/test_19-527_sample_F2/Item 1/Bullet A")
image_x3p(cts$x3p[[1]])
snapshot3d(filename ="~/Documents/CTS19.1.A.png")

hamby <- read_bullet("/Users/stephanie/Documents/Hamby Set 44 Rescan Preprocessed/Barrel 1/Bullet 1")
image_x3p(hamby$x3p[[1]])
snapshot3d(filename ="~/Documents/hamby44.1.2.png")
