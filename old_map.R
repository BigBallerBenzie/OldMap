library(raster)
library(rayshader)
library(elevatr)
setwd("D:/Maps/Old Map")
#topo_map <- raster::brick("	https://prd-tnm.s3.amazonaws.com/StagedProducts/Maps/HistoricalTopo/GeoTIFF/WA/WA_Mt%20Rainier_242672_1928_125000_geo.tif")
#topo_map <- raster::stack(topo_map) 

topo_map <- topo_map <- raster::brick("mttate1.tif")
topo_map <- raster::stack(topo_map)
# 
#locations <- data.frame(matrix(extent(topo_map), ncol = 2))  
#projstring <- raster::projection(topo_map)
#elevation <- get_elev_raster(locations, z = 15, prj = projstring)

elevation <- get_elev_raster(topo_map, z = 10)
#elevation1 = raster::raster("N35E138.hgt")

#crop elevation to the full map extent (past neatline)
elevation <- raster::crop(elevation, extent(topo_map))

##this raster will help knockdown the elevation outside the
## neatline in the physical map
base_raster <- elevation * 0 + 450

## I want to crop the elevation raster to the neatlines

x <- c(137.500, 137.500, 137.750, 137.750)
y <- c(36.500, 36.667, 36.500, 36.667)
xy <- cbind(x,y)
S <- SpatialPoints(xy, proj4string = CRS("+proj=longlat +ellps=clrk66 +datum=NAD27 +no_defs "))

S <- spTransform(S, crs(topo_map))

interior_elevation <- raster::crop(elevation, extent(S))

elevation <- merge(interior_elevation, base_raster)

names(topo_map) <- c("r", "g", "b")
topo_r <- rayshader::raster_to_matrix(topo_map$r)
topo_g <- rayshader::raster_to_matrix(topo_map$g)
topo_b <- rayshader::raster_to_matrix(topo_map$b)
topo_rgb_array <- array(0, dim = c(nrow(topo_r), ncol(topo_r), 3))

topo_rgb_array[,,1] <- topo_r/255
topo_rgb_array[,,2] <- topo_g/255
topo_rgb_array[,,3] <- topo_b/255

## the array needs to be transposed, just because.

topo_rgb_array <- aperm(topo_rgb_array, c(2,1,3))

elev_mat <- raster_to_matrix(elevation)
ray_shadow <- ray_shade(elev_mat, sunaltitude = 40, zscale = 30, multicore = TRUE)
ambient_shadow <- ambient_shade(elev_mat, zscale = 30)


#elev_mat1 = resize_matrix(elev_mat, scale = 2)


elev_mat %>%
  sphere_shade(texture = "bw") %>%
  add_overlay(topo_rgb_array) %>%
  add_shadow(ray_shadow, max_darken = 0.7) %>%
  add_shadow(ambient_shadow, 0.25) %>%
  plot_map()

plot_3d(topo_rgb_array,elev_mat, zscale = 30, windowsize = c(1800,2400), 
        phi = 40, theta = 135, zoom = 0.9, 
        background = "grey30", shadowcolor = "grey5", 
        soliddepth = -50, shadowdepth = -100)
render_camera(theta = 0, phi = 80, zoom = 0.7, fov = 0)
#render_camera(theta = -0, phi = 40, zoom = 0.12, fov = 150)
render_highquality(lightintensity = 500, samples = 400,
                   width = 7200, height = 4800)
render_movie(filename = "tate.mp4", type = "orbit",
             phi = 40,theta = 0,frames = 1440, fps = 60)

