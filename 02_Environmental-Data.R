#### Adding Environmental Data ####
## Charlotte Aston ##
## 24/09/2021 ##
## Code to add environmental data to the dataset ##

#### Packages #####
library(tidyverse)
library(dplyr)
library(lubridate)
library(chron)
library(lunar)
library(sp)
library(raster)
library(ggplot2)
library(leaflet)
library(rgeos)
library(rgdal)
library(sf)
library(ggsn)
library(nngeo)
library(spatstat)

#### Functions ####
devtools::source_url("https://github.com/UWA-Marine-Ecology-Group-projects/Useful-spatial-functions/blob/master/Useful-spatial-functions.R?raw=TRUE")
devtools::source_url("https://github.com/aodn/imos-user-code-library/blob/master/R/commons/NetCDF/ncParse.R?raw=TRUE")

outerBuffer<-function(x, dist){ #Calculates outer buffer of a polygon
  buff<-buffer(x, width = dist - 1, dissolve = T)
  e<-erase(buff,x)
  return(e)
}

st_centroid_within_poly <- function (poly) { #returns true centroid if inside polygon otherwise makes a centroid inside the polygon
  
  # check if centroid is in polygon
  centroid <- poly %>% st_centroid() 
  in_poly <- st_within(centroid, poly, sparse = F)[[1]] 
  
  # if it is, return that centroid
  if (in_poly) return(centroid) 
  
  # if not, calculate a point on the surface and return that
  centroid_in_poly <- st_point_on_surface(poly) 
  return(centroid_in_poly)
}


#### #DATA + DIRECTORIES ####
working.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # to directory of current file - or type your own

## Save these directory names to use later
data_dir <- paste(working.dir, "Raw Data", sep="/")
fig_dir <- paste(working.dir, "Figures", sep="/")
clean_dir <- paste(working.dir, "Clean Data", sep="/")











