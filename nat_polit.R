library(rgdal)
library(sp)
library(raster)
library(rgeos)
library(spdep)
library(maptools)

library(ggplot2)
library(nlme)
library(plyr)
library(dplyr)


setwd("D:\\Dropbox\\My Projects\\Fulbright\\Classes\\GIS modelling\\Group project\\georgia_dasym")

### Source data processing script 
source('src/read.data.r')


###
source('src/lisa.r')

###
source('src/inter.R')