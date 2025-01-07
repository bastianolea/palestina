# output: victimas, muertes, eventos

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)



## nombres víctimas ----
victimas_0 <- read_csv("datos/palestine-datasets/killed-in-gaza.csv")

victimas <- victimas_0 |> 
  rename(edad = age,
         sexo = sex, 
         fuente = source,
         nombre = en_name) |> 
  # edad categórica
  mutate(edad_c = cut(edad, 
                      # c(0, 18, 30, 40, 50, 60, Inf),
                      c(0, 10, 20, 30, 40, 50, 60, Inf),
                      right = FALSE)) |> 
  mutate(sexo = case_match(sexo,
                           "m" ~ "Masculino", 
                           "f" ~ "Femenino"))

victimas |> 
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

# ACLED collects and records reported information on political violence, demonstrations
# (rioting and protesting), and other select non-violent, politically important events. It aims
# to capture the modes, frequency, and intensity of political violence and demonstrations.

# tiene coordenadas

library(dplyr)

eventos_0 <- readr::read_csv("datos/acled/Israel_Palestine_Dec13.csv")

eventos <- eventos_0 |> 
  select(fecha = event_date,
         año = year,
         tipo_desorden = disorder_type,
         tipo_evento = event_type,
         tipo_evento2 = sub_event_type,
         muertes = fatalities,
         region, 
         pais = country, 
         everything())

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
