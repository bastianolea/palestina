# output: victimas, muertes, eventos

library(readr)
library(dplyr)
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
  mutate(edad_c = case_match(edad_c,
                             "[0,10)" ~ "0 a 10 años",
                             "[10,20)" ~ "10 a 19 años",
                             "[20,30)" ~ "20 a 29 años",
                             "[30,40)" ~ "30 a 39 años",
                             "[40,50)" ~ "40 a 49 años",
                             "[50,60)" ~ "50 a 59 años",
                             "[60,Inf)" ~ "60 o más")) |> 
  mutate(sexo = case_match(sexo,
                           "m" ~ "Masculino", 
                           "f" ~ "Femenino")) |> 
  select(-id, -name, -dob)


# guardar
readr::write_rds(victimas, "palestina/pdatasets_victimas.rds", compress = "none")


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

# guardar
readr::write_rds(muertes, "palestina/pdatasets_muertes.rds", compress = "none")

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
library(sf)
library(lubridate)

# eventos_0 <- readr::read_csv("datos/acled/Israel_Palestine_Dec13.csv")
eventos_0 <- readr::read_csv("datos/acled/Israel_Palestine_May23.csv")

eventos_1 <- eventos_0 |> 
  select(fecha = event_date,
         año = year,
         tipo_desorden = disorder_type,
         tipo_evento = event_type,
         tipo_subevento = sub_event_type,
         muertes = fatalities,
         admin1,
         pais = country, 
         longitude,
         latitude)

eventos_2 <- eventos_1 |> 
  # zona
  mutate(zona = case_when(admin1 == "Gaza Strip" ~ "Gaza",
                          admin1 == "West Bank" ~ "Cisjordania",
                          .default = "Otros")) |> 
  # traducciones
  mutate(tipo_desorden = case_match(tipo_desorden,
                                    "Political violence; Demonstrations" ~ "Protestas",
                                    "Demonstrations" ~ "Protestas",
                                    "Political violence" ~ "Violencia política",
                                    "Strategic developments" ~ "Sucesos estratégicos",
                                    .default = tipo_desorden)) |> 
  mutate(tipo_evento = case_match(tipo_evento,
                                  "Battles" ~ "Enfrentamientos",
                                  "Explosions/Remote violence" ~ "Violencia remota/explosivos",
                                  "Protests" ~ "Protestas",
                                  "Riots" ~ "Protestas violentas",
                                  "Strategic developments" ~ "Acciones estratégicas",
                                  "Violence against civilians" ~ "Violencia contra civiles",
                                  .default = tipo_evento)) |> 
  mutate(tipo_subevento = case_match(tipo_subevento,
                                   "Mob violence" ~ "Violencia grupal",
                                   "Peaceful protest" ~ "Protesta pacífica",
                                   "Shelling/artillery/missile attack" ~ "Bombardeos o misiles",
                                   "Air/drone strike" ~ "Ataque aéreo/drones",
                                   "Armed clash" ~ "Enfrentamiento armado",
                                   "Other" ~ "Otros",
                                   "Looting/property destruction" ~ "Saqueos/destrucción de propiedad",
                                   "Attack" ~ "Ataques",
                                   "Violent demonstration" ~ "Protesta violenta",
                                   "Protest with intervention" ~ "Protesta con intervención",
                                   "Disrupted weapons use" ~ "Denegación de uso de armas",
                                   "Remote explosive/landmine/IED" ~ "Explosivo remoto/minas",
                                   "Arrests" ~ "Detenciones",
                                   "Grenade" ~ "Granadas",
                                   # .default = tipo_subevento
                                   .default = "Otros"
                                   ))

# eventos_2 |> 
#   count(tipo_subevento, sort = T) |> 
#   print(n=Inf)

eventos_2 |>
  count(tipo_subevento, sort = T) |>
  pull(tipo_subevento) |> 
  cat(sep = "\n")


eventos_3 <- eventos_2 |>
  sf::st_as_sf(coords = c("longitude", "latitude")) |>
    sf::st_set_crs("WGS84")


eventos_mes <- eventos_2 |> 
  mutate(fecha = floor_date(fecha, "month", week_start = 1)) |>
  group_by(año, fecha, tipo_evento) |>
  summarize(n = n(),
            muertes = sum(muertes),
            .groups = "drop")

# guardar
readr::write_rds(eventos_3, "palestina/acled_eventos.rds", compress = "none")
readr::write_rds(eventos_mes, "palestina/acled_eventos_mes.rds", compress = "none")
