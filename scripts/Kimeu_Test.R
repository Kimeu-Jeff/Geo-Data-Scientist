# ================================================
# Block 1: Set working directory and folders
# & load and view data
# ================================================
# Set path to working directory
Root <- "C:/Users/kimeu/Desktop/cropnuts/Tests-for-GeoSpatial-Data-Scientist-master"
setwd(Root)

# Output directory
Path_out <- paste(Root, "/outputData",sep="")
# load data
load("C:/Users/kimeu/Desktop/cropnuts/Tests-for-GeoSpatial-Data-Scientist-master/inputData/Input_Data.RData")

# view spatial points and polygons
View(gadm)
View(po)

# ================================================
# Block 2: Clean data and transform coordinates from 
# dd to utm
# ================================================
# convert dd to utm (wgs84)
po1 <- spTransform(po, CRS("+proj=utm +zone=36 ellps=WGS84"))
po1

gadm1 <- spTransform(gadm, CRS("+proj=utm +zone=36 ellps=WGS84"))
gadm1

# plot both the polygons and points
library(sp)
plot(gadm1, axes=TRUE, add = TRUE)
# add the points
plot(po1, add = TRUE)

# detect points outside polygon
options(max.print=999999)
test.points <- po1
coordinates(test.points) <- ~long+lat
proj4string(test.points) <- CRS(proj4string(gadm1))
over(test.points, gadm1)

# remove the outlier point data (points outside polygon)
new_po <- po1[-c(2,7,11,32,64,82,83,118,119,120,277,278,279,280,281,282,283,284,301),]
# plot again to confirm
library(sp)
plot(gadm1, axes=TRUE, add = TRUE)
# add the points
plot(new_po, add = TRUE)

# ===================================================
# Block 3: Interpolation of Soil pH and OM using IDW
# ===================================================
# 
library(rgdal)
library(tmap)
P <- new_po
W <- gadm1

# Replace point boundary extent with that of Bungoma
P@bbox <- W@bbox

# plot sampled soil PH
tm_shape(W) + tm_polygons() +
  tm_shape(P) +
  tm_dots(col="pH", palette = "RdBu", stretch.palette = TRUE, midpoint = NA,
          title="Sampled pH", size=0.1) +
  tm_text("pH", just="left", xmod=.5, size = 0.2) +
  tm_legend(legend.outside=TRUE)

# plot sampled Organic Matter
tm_shape(W) + tm_polygons() +
  tm_shape(P) +
  tm_dots(col="OM", palette = "RdBu", stretch.palette = TRUE, midpoint = NA,
          title="Sampled OM", size=0.1) +
  tm_text("OM", just="left", xmod=.5, size = 0.2) +
  tm_legend(legend.outside=TRUE)

# Soil pH interpolation
library(gstat) # Using gstat's idw 
library(sp)    
library(raster)

# Create an empty grid with n cells
grd              <- as.data.frame(spsample(P, "regular", n=50000))
names(grd)       <- c("long", "lat")
coordinates(grd) <- c("long", "lat")
gridded(grd)     <- TRUE  # SpatialPixel object
fullgrid(grd)    <- TRUE  # SpatialGrid object

# Set grid projection to points projection
proj4string(grd) <- proj4string(P)

# Interpolate the grid cells using (idp=2.0)
P.idw <- gstat::idw(pH ~ 1, P, newdata=grd, idp=2.0)

# Convert to raster object and clip to Bungoma
r <- raster(P.idw)
r.m <- mask(r, W)

# Plot interpolated map
tm_shape(r.m) + 
  tm_raster(n=10,palette = "RdBu", stretch.palette = TRUE,midpoint = NA,
            title="Predicted Soil pH") + 
  tm_shape(P) + tm_dots(size=0.1) +
  tm_legend(legend.outside=TRUE)


# Organic Matter interpolation
library(gstat) # Using gstat's idw 
library(sp)    
library(raster)

# Create an empty grid with n cells
grd              <- as.data.frame(spsample(P, "regular", n=50000))
names(grd)       <- c("long", "lat")
coordinates(grd) <- c("long", "lat")
gridded(grd)     <- TRUE  # SpatialPixel object
fullgrid(grd)    <- TRUE  # SpatialGrid object

# Set grid projection to points projection
proj4string(grd) <- proj4string(P)

# Interpolate the grid cells using (idp=2.0)
P.idw <- gstat::idw(OM ~ 1, P, newdata=grd, idp=2.0)

# Convert to raster object and clip to Bungoma
r <- raster(P.idw)
r.m1 <- mask(r, W)

# Plot interpolated map
tm_shape(r.m1) + 
  tm_raster(n=10,palette = "RdBu", stretch.palette = TRUE,midpoint = NA,
            title="Predicted Organic Matter") + 
  tm_shape(P) + tm_dots(size=0.1) +
  tm_legend(legend.outside=TRUE)

# =================================================
# Block 4:Save images as jpeg files in output folder
# =================================================
# save soil ph as jpeg file
library(sp)
library(raster)
library(jpeg)
data <- readJPEG(r.m)
data@extent@xmin <- 651719.89
data@extent@xmax <- 739895.30
data@extent@ymin <- 47014.26
data@extent@ymax <- 127295.60
data@crs@projargs <- "+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
#raster::writeRaster(data, "pH2.jpeg")
jpeg::writeJPEG(r.m, target = raw(), quality = 0.7, bg = "white", "color.space")

# =================================================
# Block 5: Export point data frame to PostGIS
# =================================================
# Export to postgis
library(sp)
data("new_po")
pgInsert(conn, "new_po", new_po, new.id = "gid")

