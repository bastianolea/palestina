# víctimas por edad ----

gaza_victimas |> 
  ggplot(aes(edad)) +
  geom_density()

gaza_victimas |> 
  ggplot(aes(edad,
             fill = sexo)) +
  geom_density()

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
