library(sf)
library(rnaturalearth)

# data_coord <- data_2 |> 
#   st_as_sf(coords = c("longitude", "latitude")) |> 
#   st_set_crs("WGS84")
# 
# data_coord |> 
#   ggplot() +
#   geom_sf()

sf_use_s2(FALSE)

# obtener mapa de países
mapa <- rnaturalearth::ne_countries(scale = 10, 
                                    country = c("israel", "palestine", 
                                                "egypt", "lebanon", "jordan", 
                                                "syria", "saudi arabia"))

# recortar mapa
mapa_b <- mapa |> 
  select(admin, type, label_x, label_y) |> 
  st_crop(xmin = 32, xmax = 37,
          ymin = 35, ymax = 28)

# guardar
readr::write_rds(mapa_b,
                 "palestina/mapa_palestina.rds", compress = "none")
