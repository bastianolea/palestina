# https://acleddata.com/israel-palestine/
# codificación: https://acleddata.com/knowledge-base/coding-of-fatalities-in-gaza-since-7-october-2023/
# fuentes: https://acleddata.com/knowledge-base/israel-palestine-sourcing-profile/
# diccionario de códigos: https://acleddata.com/download/2827/


# tiene coordenadas

library(dplyr)

data <- readr::read_csv("datos/acled/Israel_Palestine_Dec13.csv")


data_2 <- data |>
  filter(event_date >= "2023-10-07") |>
  filter(event_type != "Strategic developments")

data |> glimpse()

data |> 
  count(disorder_type)

data |> 
  count(event_type)

data |> 
  filter(fatalities > 0) |> 
  count(actor1) |> 
  print(n=Inf)

data |> 
  filter(fatalities > 0) |> 
  count(actor1, inter1) |> 
  print(n=Inf)

data |> 
  count(admin1)

library(ggplot2)

data |>
  filter(year >= 2023) |> 
  ggplot() +
  aes(event_date, fatalities) +
  geom_point(aes(color = event_type), alpha = .4) +
  guides(color = guide_legend(position = "bottom"))


data |>
  filter(year >= 2023) |> 
  ggplot() +
  aes(event_date, fatalities) +
  geom_point(aes(color = disorder_type), alpha = .4) +
  guides(color = guide_legend(position = "bottom", nrow = 2))


data |>
  filter(year >= 2023) |> 
  ggplot() +
  aes(event_date, fatalities) +
  geom_point(aes(color = disorder_type), alpha = .4) +
  guides(color = guide_legend(position = "bottom", nrow = 2))





library(sf)

data_coord <- data_2 |> 
  st_as_sf(coords = c("longitude", "latitude")) |> 
  st_set_crs("WGS84")

data_coord |> 
  ggplot() +
  geom_sf()

library(rnaturalearth)

sf_use_s2(FALSE)

# obtener mapa de países
mapa <- rnaturalearth::ne_countries(scale = 10, 
                                    country = c("israel", "palestine", 
                                                "egypt", "lebanon", "jordan", 
                                                "syria", "saudi arabia"))

mapa_b <- mapa |> 
  select(admin, type, label_x, label_y) |> 
  # recortar mapa
  st_crop(xmin = 32, xmax = 37,
          ymin = 35, ymax = 28)

zoom_palestina <- list(coord_sf(xlim = c(34, 36), 
                                ylim = c(33.3, 29.5)))

zoom_gaza <- list(coord_sf(xlim = c(34.1, 34.65), 
                           ylim = c(31.7, 31.1)))

zoom_cisjordania <- list(coord_sf(xlim = c(34.65, 35.8), 
                                  ylim = c(32.6, 31.3)))

ggplot() +
  annotate("rect", xmin = 32, xmax = 37,
           ymin = 35, ymax = 28, fill = "lightblue") +
  geom_sf(data = mapa_b,
          aes(fill = admin)) +
  geom_sf(data = data_coord |> 
            filter(fatalities > 0),
          aes(color = event_type),
          alpha = 0.8, size = 1) +
  zoom_palestina +
  # zoom_gaza +
  # zoom_cisjordania +
  scale_fill_manual(values = c("Palestine" = "grey80", "Israel" = "grey70"))
