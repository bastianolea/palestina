library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)


# palestine datasets ----
# https://data.techforpalestine.org/docs/datasets/

## obtener datos ----

# https://data.techforpalestine.org/docs/killed-in-gaza/
download.file("https://data.techforpalestine.org/api/v2/killed-in-gaza.csv",
              "datos/palestine-datasets/killed-in-gaza.csv")

# https://data.techforpalestine.org/docs/casualties-daily/
download.file("https://data.techforpalestine.org/api/v2/casualties_daily.csv",
              "datos/palestine-datasets/casualties_daily.csv")

# https://data.techforpalestine.org/docs/casualties-daily-west-bank/
download.file("https://data.techforpalestine.org/api/v2/west_bank_daily.csv",
              "datos/palestine-datasets/west_bank_daily.csv")


## nombres víctimas ----
gaza_victimas_0 <- read_csv("datos/palestine-datasets/killed-in-gaza.csv")

gaza_victimas <- gaza_victimas_0 |> 
  rename(edad = age,
         sexo = sex, 
         fuente = source,
         nombre = en_name) |> 
  # edad categórica
  mutate(edad_c = cut(edad, 
                      c(0, 18, 30, 40, 50, 60, Inf),
                      right = FALSE)) |> 
  mutate(sexo = case_match(sexo,
                           "m" ~ "Masculino", 
                           "f" ~ "Femenino"))

gaza_victimas |> 
  count(edad_c)


## diarios gaza ----
gaza_diarios_0 <- read_csv("datos/palestine-datasets/casualties_daily.csv") 

gaza_diarios <- gaza_diarios_0 |> 
  select(-ends_with("cum")) |> 
  select(fecha = report_date,
         # periodo = report_period,
         muertos = ext_killed,
         # muertos_e = ext_killed,
         heridos = ext_injured,
         fuente = report_source) |> 
  mutate(año = year(fecha)) |> 
  mutate(zona = "Gaza",
         pais = "Palestina")

gaza_diarios |> glimpse()

## diarios cisjordania ----
cisjordan_diarios_0 <- read_csv("datos/palestine-datasets/west_bank_daily.csv") 

cisjordan_diarios <- cisjordan_diarios_0 |> 
  select(-ends_with("cum")) |> 
  select(fecha = report_date,
         muertos = verified.killed,
         heridos = verified.injured,
         # muertos_niños = verified.killed_children,
         # heridos_niños = verified.injured_children,
         fuente = flash_source) |> 
  mutate(año = year(fecha)) |> 
  mutate(zona = "Cisjordania",
         pais = "Palestina")

cisjordan_diarios


## unir ----
muertes <- bind_rows(gaza_diarios,
                     cisjordan_diarios) |> 
  arrange(fecha) |> 
  filter(!(is.na(muertos) & is.na(heridos)),
         !(muertos == 0 & heridos == 0))

muertes




# acled ----
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
