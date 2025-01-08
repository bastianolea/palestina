library(dplyr)
library(ggplot2)
library(legendry)
library(readr)
library(sysfonts)
library(showtext)
library(lubridate)
library(legendry)
library(scales)


# datos ----
victimas <- read_rds("palestina/pdatasets_victimas.rds")
muertes <- read_rds("palestina/pdatasets_muertes.rds")
eventos <- read_rds("palestina/acled_eventos.rds")

mapa <- read_rds("palestina/mapa_palestina.rds")


# colores ----
color <- list(fondo = "#151515",
              texto = "#DDDDDD",
              borde = "#BBBBBB",
              detalle = "#353535",
              principal = "#666666"
)

tipografia <- list(titulos = "JetBrains Mono",
                   cuerpo = "Space Grotesk")




# tipografías ----
sysfonts::font_add("JetBrains Mono",
                   regular = "palestina/www/fonts/jetbrains-mono-v20-latin-regular.ttf",
                   italic = "palestina/www/fonts/jetbrains-mono-v20-latin-italic.ttf",
                   bold = "palestina/www/fonts/jetbrains-mono-v20-latin-500.ttf",
                   bolditalic = "palestina/www/fonts/jetbrains-mono-v20-latin-500italic.ttf",
)
sysfonts::font_add("Space Grotesk",
                   regular = "palestina/www/fonts/space-grotesk-v16-latin-regular.ttf",
                   bold = "palestina/www/fonts/space-grotesk-v16-latin-600.ttf",
                   # italic = "www/fonts/libre-baskerville-v14-latin-italic.ttf",
                   # bolditalic = "www/fonts/libre-baskerville-v14-latin-italic.ttf",
)

# opciones ----
showtext_auto()

showtext::showtext_opts(dpi = 180)

library(thematic)

thematic_on(font = "Space Grotesk",
            bg = color$fondo, fg = color$texto, accent = color$principal)

# —----

# leyenda de años
rango_años <- key_range_manual(
  start = c("2023-01-05", "2024-01-05", "2025-01-05") |> ymd(), 
  end = c("2023-12-25", "2024-12-25", "2025-12-25") |> ymd(), 
  name = c("2023", "2024", "2025")
)

# tema <- theme_get()

# theme_set(theme(axis.ticks = element_blank()) +
#             theme(text = element_text(family = "Space Grotesk", color = color$texto)) +
#             theme(panel.background = element_rect(fill = color$fondo),
#                   plot.background =  element_rect(fill = color$fondo)) +
#             theme(panel.background = element_rect(fill = color$fondo),
#                   panel.grid = element_line(color = color$detalle, 
#                                             linetype = "dotted"))
#           )


tema_palestina <- theme(text = element_text(family = "Space Grotesk", color = color$texto)) +
  theme(axis.ticks = element_blank()) +
  # fondo
  theme(panel.background = element_rect(fill = color$fondo),
        plot.background =  element_rect(fill = color$fondo)) +
  # grilla
  theme(panel.grid = element_line(color = color$detalle,
                                  linetype = "dotted"),
        panel.grid.major = element_line(linewidth = 0.7),
        panel.grid.minor = element_line(linewidth = 0.4))


# víctimas por edad ----
victimas |> 
  ggplot(aes(edad)) +
  geom_density(fill = color$principal, alpha = .4) +
  scale_y_continuous(expand = expansion(c(0, 0.03))) +
  scale_x_continuous(expand = expansion(c(0, 0))) +
  theme(axis.text.y = element_blank()) +
  labs(y = "proporción de víctimas por edad") +
  tema_palestina


# víctimas por edad y género ----
victimas |> 
  ggplot() +
  aes(edad, fill = sexo, color = sexo) +
  geom_density(alpha = .5) +
  scale_y_continuous(expand = expansion(c(0, 0.03))) +
  scale_x_continuous(expand = expansion(c(0, 0))) +
  theme(axis.text.y = element_blank()) +
  labs(y = "proporción de víctimas\npor edad y género",
       fill = "género", color = "género") +
  tema_palestina

victimas_edad_sexo <- victimas |> 
  count(sexo, edad_c)

## pirámide ----
victimas_edad_sexo |> 
  mutate(n = ifelse(sexo == "Femenino", 0-n, n)) |> 
  ggplot() +
  aes(n, edad_c, fill = sexo, color = sexo) +
  geom_col(width = .5, #color = color$fondo,
           alpha = .5) +
  scale_x_continuous(limits = c(-max(victimas_edad_sexo$n), max(victimas_edad_sexo$n)),
                     labels = ~abs(.x)) +
  theme(axis.title.y = element_blank()) +
  labs(x = "víctimas por edad y género") +
  tema_palestina


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
  geom_area(alpha = .5) +
  geom_segment(data = ~filter(.x, muertos >= 100),
               aes(xend = fecha, yend = 0),
               color = "red3", alpha = .7,
               linewidth = 0.3) +
  scale_y_continuous(expand = c(0, 0),
                     labels = label_comma(big.mark = ".", 
                                                  decimal.mark = ",")) +
  scale_x_date(expand = expansion(c(0.1, 0))) +
  labs(y = "víctimas totales, acumuladas",
       x = "fecha (mes, año)") +
  scale_x_date(date_breaks = "months", date_labels = "%m") +
  guides(x = guide_axis_nested(key = rango_años)) +
  tema_palestina


# muertes por meses ----
muertes_totales_mes <- muertes_totales |> 
  mutate(fecha = floor_date(fecha, "month")) |> 
  group_by(fecha) |> 
  summarize(muertos = sum(muertos),
            heridos = sum(heridos))


muertes_totales_mes |> 
  ggplot() +
  aes(fecha, muertos) +
  geom_col(width = 15) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = "months", date_labels = "%m") +
  guides(x = guide_axis_nested(key = rango_años)) +
  labs(y = "muertos por mes", x = "fecha (mes, año)") +
  tema_palestina +
  theme(axis.text.x = element_text(margin = margin(t = 4)))


# heridos por meses ----
muertes_totales_mes |> 
  ggplot() +
  aes(fecha, heridos) +
  geom_col(width = 15) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = "months", date_labels = "%m") +
  guides(x = guide_axis_nested(key = rango_años)) +
  labs(y = "muertos por mes", x = "fecha (mes, año)") +
  tema_palestina +
  theme(axis.text.x = element_text(margin = margin(t = 4)))


# por tipo de evento ----
eventos |> count(tipo_evento)

eventos |>
  filter(año >= 2023) |> 
  ggplot() +
  aes(fecha, muertes,
      size = muertes) +
  geom_point(aes(color = tipo_evento), alpha = .2) +
  guides(color = guide_legend(position = "bottom")) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = "months", date_labels = "%m",
               expand = c(0, 0)) +
  guides(x = guide_axis_nested(key = rango_años)) +
  tema_palestina +
  coord_cartesian(clip = "off") +
  theme(axis.text.x = element_text(margin = margin(t = 4)))

# # por tipo de desorden
# eventos |>
#   filter(año >= 2023) |> 
#   ggplot() +
#   aes(fecha, muertes) +
#   geom_point(aes(color = tipo_desorden), alpha = .4) +
#   guides(color = guide_legend(position = "bottom", nrow = 2))



# desorden ----
eventos_semana <- eventos |> 
  mutate(tipo_desorden = case_match(tipo_desorden,
                                    "Political violence; Demonstrations" ~ "Demonstrations",
                                    .default = tipo_desorden)) |> 
  mutate(fecha = floor_date(fecha, "month", week_start = 1)) |> 
  group_by(fecha, tipo_desorden) |> 
  summarize(n = n())


eventos_semana |>
  # filter(fecha > "2022-01-01") |> 
  ggplot() +
  aes(fecha, n, color = tipo_desorden) +
  geom_line(linewidth = 0.7, alpha = .8) +
  scale_y_continuous(expand = c(0.01, 0)) +
  scale_x_date(date_breaks = "years", date_labels = "%Y",
               expand = c(0, 0)) +
  tema_palestina +
  guides(color = guide_legend(position = "inside")) +
  theme(legend.position.inside = c(0.15, 0.8)) +
  labs(y = "cantidad de eventos", x = NULL,
       color = "Eventos")



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
  geom_violin(scale = "count", 
              adjust = .2, alpha = .6,
              fill = color$principal) +
  annotate("point", x = ymd("2023-10-07"), y = 1,
           size = 6, alpha = .2) +
  scale_x_date(date_breaks = "months", date_labels = "%m", 
               expand = expansion(c(0.01, 0.05))) +
  guides(x = guide_axis_nested(key = rango_años)) +
  facet_wrap(~tipo_evento, ncol = 1, strip.position = "left") +
  tema_palestina +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "fecha (mes, año)")


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
  geom_violin(scale = "count", 
              adjust = .1, alpha = .6,
              fill = color$principal) +
  annotate("point", x = ymd("2023-10-07"), y = 1,
           size = 6, alpha = .2) +
  scale_x_date(date_breaks = "months", date_labels = "%m",
               expand = expansion(c(0, 0.05))) +
  guides(x = guide_axis_nested(key = rango_años),
         y = guide_none(title = NULL)) +
  facet_wrap(~tipo_evento2, ncol = 1, strip.position = "left") +
  tema_palestina +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "fecha (mes, año)")




# mapa ----
zoom_palestina <- list(coord_sf(xlim = c(34, 36), 
                                ylim = c(33.3, 29.5)))

zoom_gaza <- list(coord_sf(xlim = c(34.15, 34.65), 
                           ylim = c(31.7, 31.1)))

zoom_cisjordania <- list(coord_sf(xlim = c(34.65, 35.8), 
                                  ylim = c(32.6, 31.3)))


# eventos_mapa <- eventos |> 
#   sf::st_as_sf(coords = c("longitude", "latitude")) |> 
#     sf::st_set_crs("WGS84")

ggplot() +
  annotate("rect", xmin = 32, xmax = 37,
           ymin = 35, ymax = 28, fill = NA) +
  geom_sf(data = mapa,
          aes(fill = admin), 
          alpha = .1) +
  geom_sf(data = eventos_mapa |> 
            filter(muertes > 0),
          aes(color = tipo_evento),
          alpha = 0.2, size = 1) +
  zoom_palestina +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = .5)),
         fill = guide_legend(ncol = 2, override.aes = list(height = unit(4, "mm")))) +
  zoom_gaza +
  # zoom_cisjordania +
  # zoom_palestina +
  scale_fill_manual(values = c("Palestine" = "grey60",
                               "Israel" = "grey30"), 
                    na.value = NA) +
  tema_palestina +
  theme(legend.key.height = unit(6, "mm"),
        legend.key.spacing.y = unit(0, "mm"),
        axis.text = element_text(color = color$detalle, size = 8)) +
  labs(fill = "Territorio",
       color = "Evento")
