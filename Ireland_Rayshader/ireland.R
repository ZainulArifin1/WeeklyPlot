library(rayshader)
library(elevatr)
library(raster)


Ireland <- readRDS(url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_IRL_0_sf.rds"))

dem <- get_elev_raster(Ireland, z = 6)
ireland_dem <- mask(dem,Ireland)
ireland_mat <- raster_to_matrix(ireland_dem)


ireland_mat %>%
  sphere_shade(texture = "imhof1") %>%
  add_shadow(ambient_shade(ireland_mat), 0) %>%
  plot_3d(ireland_mat, windowsize = c(1200,1200),
          zscale = 20, zoom = 0.8, phi = 89, theta = 0, fov = 0
          )
render_highquality(filename = "Ireland6.png", samples = 60, width = 2500, height = 2500)