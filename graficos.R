library(dplyr)
library(ggplot2)
library(legendry)

# víctimas por edad ----
victimas |> 
  ggplot(aes(edad)) +
  geom_density()

muertes


# víctimas por edad y género ----
victimas |> 
  ggplot() +
  aes(edad, fill = sexo, color = sexo) +
  geom_density(alpha = .5)

victimas_edad_sexo <- victimas |> 
  count(sexo, edad_c)

## pirámide ----
victimas_edad_sexo |> 
  mutate(n = ifelse(sexo == "Femenino", 0-n, n)) |> 
  ggplot() +
  aes(n, edad_c, fill = sexo) +
  geom_col() +
  scale_x_continuous(limits = c(-max(victimas_edad_sexo$n), max(victimas_edad_sexo$n)),
                     labels = ~abs(.x))

# víctimas acumuladas ----
muertes_totales <- muertes |> 
  group_by(fecha) |> 
  summarize(muertos = sum(muertos),
            heridos = sum(heridos)) |> 
  mutate(totales = cumsum(muertos)) |> 
  mutate(alto_prop = if_else(muertos > quantile(muertos, 0.8), "alto", "común"),
         alto_rel = if_else(muertos > 100, "alto", "común"))

muertes_totales |> 
  ggplot() +
  aes(fecha, totales) +
  geom_area() +
  geom_segment(data = ~filter(.x, muertos >= 100),
                             aes(xend = fecha, yend = 0),
               color = "red") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(expand = expansion(c(0.1, 0)))


# muertes por meses ----
muertes_totales_mes <- muertes_totales |> 
  mutate(fecha = floor_date(fecha, "month")) |> 
  group_by(fecha) |> 
  summarize(muertos = sum(muertos),
            heridos = sum(heridos))
  

muertes_totales_mes |> 
  ggplot() +
  aes(fecha, muertos) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0))

# leyenda de años
rango_años <- key_range_manual(
  start = c("2023-01-05", "2024-01-05", "2025-01-05") |> ymd(), 
  end = c("2023-12-25", "2024-12-25", "2025-12-25") |> ymd(), 
  name = c("2023", "2024", "2025")
)

muertes_totales_mes |> 
  ggplot() +
  aes(fecha, muertos) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = "months", date_labels = "%m") +
  guides(x = guide_axis_nested(key = rango_años))

# heridos por meses ----
muertes_totales_mes |> 
  ggplot() +
  aes(fecha, heridos) +
  geom_col() + 
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = "months", date_labels = "%m") +
  guides(x = guide_axis_nested(key = rango_años))




# por tipo de evento ----
eventos |>
  filter(año >= 2023) |> 
  ggplot() +
  aes(fecha, muertes,
      size = muertes) +
  geom_point(aes(color = tipo_evento), alpha = .4) +
  guides(color = guide_legend(position = "bottom"))

# # por tipo de desorden
# eventos |>
#   filter(año >= 2023) |> 
#   ggplot() +
#   aes(fecha, muertes) +
#   geom_point(aes(color = tipo_desorden), alpha = .4) +
#   guides(color = guide_legend(position = "bottom", nrow = 2))

# densidad eventos ----
eventos |> count(tipo_evento)

eventos |>
  filter(año >= 2023) |> 
  filter(tipo_evento %in% c("Battles",
                            "Explosions/Remote violence",
                            "Violence against civilians")) |> 
  ggplot() +
  aes(fecha, 1) +
  annotate("point", x = ymd("2023-10-07"), y = 1,
           size = 18, alpha = .2) +
  geom_violin(scale = "count", adjust = .2, alpha = .6) +
  annotate("point", x = ymd("2023-10-07"), y = 1,
           size = 6, alpha = .2) +
  scale_x_date(date_breaks = "months", date_labels = "%m", 
               expand = expansion(c(0, 0.05))) +
  guides(x = guide_axis_nested(key = rango_años),
         guide_none(title = NULL)) +
  facet_wrap(~tipo_evento, ncol = 1, strip.position = "left")


# densidad subeventos ----
eventos |>
  filter(año >= 2023) |> 
  filter(tipo_evento2 %in% c("Air/drone strike",
                            "Attack",
                            "Armed clash",
                            "Shelling/artillery/missile attack")) |> 
  ggplot() +
  aes(fecha, 1) +
  annotate("point", x = ymd("2023-10-07"), y = 1,
           size = 18, alpha = .2) +
  geom_violin(scale = "count", adjust = .1, alpha = .6) +
  annotate("point", x = ymd("2023-10-07"), y = 1,
           size = 6, alpha = .2) +
  scale_x_date(date_breaks = "months", date_labels = "%m",
               expand = expansion(c(0, 0.05))) +
  guides(x = guide_axis_nested(key = rango_años),
         y = guide_none(title = NULL)) +
  facet_wrap(~tipo_evento2, ncol = 1, strip.position = "left")




# mapa ----

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
            filter(muertes > 0),
          aes(color = tipo_evento),
          alpha = 0.8, size = 1) +
  zoom_palestina +
  # zoom_gaza +
  # zoom_cisjordania +
  scale_fill_manual(values = c("Palestine" = "grey80", "Israel" = "grey70"))
