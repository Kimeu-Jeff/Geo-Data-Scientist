# load data
load("C:/Users/kimeu/Desktop/cropnuts/Tests-for-GeoSpatial-Data-Scientist-master/inputData/Input_Data.RData")

# view spatial points and polygons
View(gadm)
View(po)

# change column names for coords in spatial points
po@coords <- data.frame(po@coords)
colnames(po@coords) <- c("x", "y")

# plot both the polygons and points
library(sp)
plot(gadm, axes=TRUE, add = TRUE)
# add the points
plot(po, add = TRUE)

# detect points outside polygon
options(max.print=999999)
test.points <- po
coordinates(test.points) <- ~x+y
proj4string(test.points) <- CRS(proj4string(gadm))
over(test.points, gadm)

# remove the outlier point data
new_po <- po[-c(2,7,11,32,64,82,83,118,119,120,277,278,279,280,281,282,283,284,301),]
# plot again to confirm
library(sp)
plot(gadm, axes=TRUE, add = TRUE)
# add the points
plot(new_po, add = TRUE)

# interpolation
library(rgdal)
library(tmap)

# Load precipitation data
#z <- gzcon(url("http://colby.edu/~mgimond/Spatial/Data/precip.rds"))
#P <- readRDS(z)

# Load Texas boudary map
#z <- gzcon(url("http://colby.edu/~mgimond/Spatial/Data/texas.rds"))
#W <- readRDS(z)

# Replace point boundary extent with that of Texas
new_po@bbox <- gadm@bbox

tm_shape(gadm) + tm_polygons() +
  tm_shape(new_po) +
  tm_dots(col="pH", palette = "RdBu", auto.palette.mapping = FALSE,
          title="SOIL PH", size=0.7) +
  tm_text("pH", just="left", xmod=.5, size = 0.7) +
  tm_legend(legend.outside=TRUE)

# IDW Interpolation
library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function

# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(new_po, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("x", "y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(grd) <- proj4string(new_po$pH)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
new_po.idw <- gstat::idw(pH ~ 1, new_po, newdata=grd, idp=2.0)

# Convert to raster object then clip to Texas
r       <- raster(new_po.idw)
r.m     <- mask(r, gadm)

# Plot
tm_shape(r.m) + 
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Predicted pH") + 
  tm_shape(new_po) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)


library(gstat)
gs <- gstat(formula=pH~1, locations=new_po)
idw <- interpolate(r, gs)
## [inverse distance weighted interpolation]
idwr <- mask(idw, vr)
plot(idwr)