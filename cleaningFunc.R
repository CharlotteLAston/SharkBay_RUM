# Functions 

# extract_mean_from_raster
# extract_mean_from_raster is a function to extract the average value of a raster over each polygon in a shapefile. 
# It is essentially a wrapper for the extract function in the raster package. The function needs to be provided with an sf 
# shapefile, and will return an sf shapefile

# Arguments
# raster: is a raster file stored as a "Formal class RasterLayer" - load using the raster function. Check projections. 
# polygon: a polygon shapefile stored as a sf - load using st_read - need to ensure there are no null geometries

extract_mean_from_raster<-function(raster, polygon){
  r.vals <- raster::extract(raster, polygon) #extract values of raster
  r.mean <- unlist(lapply(r.vals, FUN=mean)) #take mean for each polygon
  
  polygon$mean <- r.mean #merge mean back in to polygon
  polygon
}


# hhmm2dec
# hhmm2dec is a function that turns hh:mm formatted time into decimal time. 
# Arguments 
# x = a time variable formatted hh:mm

hhmm2dec <- function(x) {
  xlist <- strsplit(x,split=":")
  h <- as.numeric(sapply(xlist,"[",1))
  m <- as.numeric(sapply(xlist,"[",2))
  s <- as.numeric(sapply(xlist,"[",3))
  xdec <- h+(m/60)
  return(xdec)
}