library(shiny)
library(bslib)
library(htmltools)
library(shinyWidgets)
library(shinycssloaders)

library(readr) |> suppressPackageStartupMessages()
library(dplyr) |> suppressPackageStartupMessages()
library(purrr) |> suppressPackageStartupMessages()
library(lubridate) |> suppressPackageStartupMessages()

library(ggplot2)
library(thematic)
library(legendry)
library(scales)

library(sysfonts)
library(showtext)


# datos ----

victimas <- read_rds("pdatasets_victimas.rds")
muertes <- read_rds("pdatasets_muertes.rds")
eventos <- read_rds("acled_eventos.rds")
eventos_mes <- read_rds("acled_eventos_mes.rds")

mapa <- read_rds("mapa_palestina.rds")

nombres <- victimas |> filter(edad < 18) |> pull(nombre) |> sample(300)

total_victimas <- sum(muertes$muertos)



# colores ----
color <- list(fondo = "#151515",
              texto = "#DDDDDD",
              borde = "#BBBBBB",
              detalle = "#353535",
              principal = "#666666"
)

thematic_shiny(font = "auto")


# tipografías ----
tipografia <- list(titulos = "JetBrains Mono",
                   cuerpo = "Space Grotesk")

sysfonts::font_add("JetBrains Mono",
                   regular = "www/fonts/jetbrains-mono-v20-latin-regular.ttf",
                   italic = "www/fonts/jetbrains-mono-v20-latin-italic.ttf",
                   bold = "www/fonts/jetbrains-mono-v20-latin-500.ttf",
                   bolditalic = "www/fonts/jetbrains-mono-v20-latin-500italic.ttf",
)
sysfonts::font_add("Space Grotesk",
                   regular = "www/fonts/space-grotesk-v16-latin-regular.ttf",
                   bold = "www/fonts/space-grotesk-v16-latin-600.ttf",
                   # italic = "www/fonts/libre-baskerville-v14-latin-italic.ttf",
                   # bolditalic = "www/fonts/libre-baskerville-v14-latin-italic.ttf",
)


# funciones ----

# recuadro blanco que envuelve el contenido
cuadro <- function(..., alto = NULL, flujo = "scroll") {
  if (is.null(alto)) {
    div(style = css(margin = "8px", padding = "18px",
                    margin_bottom = "58px",
                    border = paste("2px solid", color$borde)),
        ...
    )
  } else {
    div(style = css(margin = "8px", padding = "18px",
                    margin_bottom = "58px",
                    height = alto,
                    overflow_y = flujo,
                    border = paste("2px solid", color$borde)),
        ...
    )
  }
}

cuadro_negro <- function(..., alto = NULL) {
  if (is.null(alto)) {
    div(style = css(margin = "8px", padding = "24px",
                    padding_bottom = "14px",
                    margin_bottom = "34px",
                    background_color = color$fondo,
                    opacity = "100%",
                    border = paste("2px solid", color$borde)),
        ...
    )
  } else {
    div(style = css(margin = "8px", padding = "24px",
                    padding_bottom = "14px",
                    margin_bottom = "34px",
                    background_color = color$fondo,
                    opacity = "100%",
                    height = alto,
                    border = paste("2px solid", color$borde)),
        ...
    )
  }
}

# título sobre bloque negro y sobre borde superior de un cuadro
subtitulo <- function(..., ancho = "200px") {
  div(style = css(text_align = "center", 
                  margin = "auto", margin_top = "-33px",
                  margin_bottom = "14px",
                  background_color = color$fondo,
                  max_width = ancho),
      ...
  )
}

# bloque de fondo negro tras un título
bloque <- function(..., ancho = "200px") {
  div(style = css(background_color = color$fondo, max_width = ancho),
      ...)
}


comando <- function(...) {
  div(style = css(padding_left = "1em",
                  text_indent = "-1em",
                  line_height = "1.2",
                  font_size = "85%",
                  margin_bottom = "22px",
                  margin_top = "12px",
                  background_color = color$fondo,
                  # color = color$principal,
                  # opacity = "70%"
  ),
  p(...,
    style = "font-family: JetBrains Mono !important;")
  )
}

# leyenda de años
rango_años <- key_range_manual(
  start = c("2023-01-05", "2024-01-05", "2025-01-05") |> ymd(), 
  end = c("2023-12-25", "2024-12-25", "2025-12-25") |> ymd(), 
  name = c("2023", "2024", "2025")
)

# tema ggplot
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

# opciones ----
showtext::showtext_opts(dpi = 180)
alto_cuadros_victimas = 700
options(spinner.type = 1, spinner.color = color$principal)

# ui ----

ui <- page_fluid(
  
  ## tema ----
  theme = bslib::bs_theme(fg = color$texto, bg = color$fondo,
                          primary = color$principal
  ),
  
  ## tipografías ----
  
  # tipografía de cuerpo
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/space-grotesk.css"),
    tags$style("* {font-family:'Space Grotesk' !important;}")
  ),
  
  # tipografía de títulos
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/jetbrains-mono.css"),
    tags$style("h1, h2, h3, h4, h5, h6, label, item {font-family:'JetBrains Mono' !important;}")
  ),
  
  ## estilos ----
  
  # fondo negro de titulares
  # tags$style(
  # HTML("h1, h2 {background-color:", color$fondo, ";}")),
  
  # subrayado de titulares
  tags$style(
    HTML("h3, h4, h4, h6 {text-decoration: underline;
                          text-underline-position: under;
                          text-decoration-color:", color$borde, ";
                          text-decoration-thickness: 2px;
                          line-height: 1.4;}")),
  
  tags$style(
    HTML("h2 {font-size: 150%;}")),
  
  tags$style(
    HTML("h3 {font-size: 140%; margin-bottom: 18px;}")),
  
  # color de enlaces
  tags$style(
    HTML("a {color:", color$principal, ";}")),
  
  # separador
  tags$style(
    HTML("hr {
    margin: 0.5rem;
    margin-bottom: 3.3rem;
    color: inherit;
    border: 0;
    border-top: 2px dashed", color$borde, ";
    opacity: 1;
  }")),
  
  # sliders
  tags$style(
    HTML(".irs-min, .irs-max, .irs-single {
         border-radius: 0px !important; padding: 4px !important;
         top: 2px !important;
         background-color:", color$fondo, "!important}
     .irs-grid-text {
         bottom: 1px !important;
         font-size: 75%;
         background-color:", color$fondo, "!important}
     .irs-grid-pol {width: 2px !important; opacity: 1 !important;} /* ticks */
     .irs-handle {
         border-radius: 0px !important; 
         width: 19px !important; height: 19px !important;
         border: 2px solid", color$borde, "!important;
         background-color:", color$fondo, "!important;
         margin-top: 0.5px !important;}
     .irs-handle:hover {
         background-color:", color$borde, "!important;}
     .irs-line {
         background-color:", color$borde, "!important;
         height: 2px !important;
         opacity: 1 !important;}
     .irs-bar {display: none !important;} /* barra izq eleccionada */
     .control-label {
         font-family:", tipografia$titulos, "!important;}")
  ),
  
  # botones radio
  tags$style(
    HTML(".radiobtn {
          border: 2px solid", color$borde, "!important;
          border-radius: 0 !important;
          color:", color$texto, "!important;
          font-size: 100% !important;
          background-color:", color$fondo, "!important;}
     .radiobtn:hover { /* hover */
          background-color:", color$borde, "!important;
          color:", color$fondo, "!important;}
     .btn-check:checked+.btn { /* botón activo */
          background-color:", color$borde, "!important;
          color:", color$fondo, "!important;
          }")),
  
  # botones
  tags$style(
    HTML(".action-button {
          border: 2px solid", color$borde, "!important;
          border-radius: 0 !important;
          color:", color$texto, "!important;
          font-size: 100% !important;
          background-color:", color$fondo, "!important;
          }
    .action-button:hover {
          background-color:", color$borde, "!important;
          color:", color$fondo, "!important;
          }")),
  
  
  # fondo de cuadrícula
  tags$style(
    HTML("body {
                background:", color$fondo, ";
                background-image: radial-gradient(", color$detalle, "1.2px, transparent 0);
                background-size: 18px 18px;
                }")),
  
  
  # fade in
  tags$style(
    HTML(".fade_in {
    animation: fade 2.5s linear;
    }

    @keyframes fade {
      0% { opacity: 0; }
	    100% { opacity: 1; }
      }")),
  
  
  
  # ancho máximo de la app
  div(style = css(max_width = "800px", margin = "auto"),
      
      
      ## header ----
      div(style = css(height = "20px")),
      
      cuadro(
        # h1(style = css(background_color = color$fondo, max_width = "200px"), 
        bloque(ancho = "170px",
               h1("Palestina")
        ),
        p("Prueba de texto")
      ),
      
      
      # —----
      
      hr(), 
      
      # contenido ----
      
      ## víctimas ----
      cuadro(
        subtitulo(ancho = "160px", 
                  h2("Víctimas")
        ),
        
        p("Visualizaciones de datos sobre las víctimas letales de la guerra en territorio palestino."),
        ### acumuladas ----
        
        
        cuadro_negro(
          bloque(h3("Muertes totales acumuladas"), ancho = "230px"),
          
          sliderInput("muertes_acumuladas", 
                      "Destacar masacres", 
                      min = 0, max = 500, value = 100, step = 50),
          
          textOutput("texto_muertes_acumuladas", container = comando),
          
          plotOutput("muertes_acumuladas") |> withSpinner(),
          
          comando("> un gráfico acumulado representa un conteo aditivo, es decir, la cifra total de muertes acumulada día tras día."),  
        ),
        
        
        
        ### mes ----
        
        cuadro_negro(
          bloque(h3("Muertes por mes"), ancho = "230px"),
          
          radioGroupButtons(
            inputId = "muertes_totales_mes",
            label = NULL,
            choices = c("Asesinados", "Heridos")
          ),
          
          plotOutput("muertes_totales_mes") |> withSpinner()
          
        )
        
        
      ), #fin cuadro
      
      hr(),
      
      ## caracterización víctimas ----
      cuadro(
        subtitulo(ancho = "300px",
                  h2("Caracterización de víctimas")
        ),
        
        
        layout_columns(#fill = TRUE, min_height = 520,
          
          ### distribución edad ----
          cuadro_negro(alto = "800px",
                       bloque(h3("Víctimas letales por edad"), ancho = "260px"),
                       
                       p("distribución de las víctimas de las Fuerzas de Defensa de Israel y sus aliados"),
                       
                       radioGroupButtons(
                         inputId = "victimas_edad",
                         label = NULL,
                         choices = c("Edad", "Género")
                       ),
                       
                       plotOutput("victimas_edad", height = 330) |> withSpinner(),
                       
                       comando("> los gráficos de densidad representan la distribución de los datos horizontalmente, donde la altura de la curva representa la cantidad de casos correspondientes a ese punto de la variable horizontal (edad). Vemos que la mayoría de las víctimas son menores de 30 años, pero si desagregamos por edad, se evidencia una diferencia en la edad promedio de las víctimas."),
          ),
          
          ### nombres ----
          # palabras que aparecen una tras otra
          cuadro(alto = "800px", flujo = "hidden",
                 bloque(h3("Víctimas menores de edad"), ancho = "230px"),
                 
                 p(em("Cada nombre corresponde a una víctima confirmada de la guerra con menos de 18 años a la fecha de su muerte.")),
                 
                 p(em("Los nombres son elegidos al azar de entre las decenas de miles de víctimas registradas.")),
                 
                 
                 div(style = css(max_height = "600px", 
                                 overflow_y = "scroll"),
                     
                     # style = "column-count: 2;", # texto en múltiples columnas
                     purrr::map(1:length(nombres), ~{
                       # p("texto", .x), # para que sean líneas independientes
                       span(nombres[.x], " / ", # para texto con flujo continuado
                            # animación
                            style = paste0("opacity: 0; font-size: 90%;",
                                           "animation: fade 4s ease forwards;", # controla duración de animación
                                           "animation-delay: ", .x*0.8, "s;") # controla velocidad de aparición
                       )
                     })
                 )
          )
        ), # fin columnas
        
        
        ### pirámide ----
        
        div(style = css(margin_top = "-36px"),
            cuadro_negro(
              bloque(h3("Víctimas letales por género"), ancho = "260px"),
              
              p("pirámide de población con las edades y géneros de las víctimas"),
              
              plotOutput("victimas_piramide") |> withSpinner(),
              
              comando("> las pirámides de población permiten analizar rápidamente la distribución poblacional afectada; en este caso, podemos ver una concentración de las víctimas jóvenes y de mediana edad, más sesgada hacia los hombres.")
            )
        ),
        
        
      ),
      
      
      hr(),
      
      
      ## eventos ----
      cuadro(
        subtitulo(ancho = "140px",
                  h2("Eventos")
        ),
        
        ### tipo ----
        cuadro_negro(
          bloque(h3("Tipo de evento"), ancho = "200px"),
          
          plotOutput("evento_tipo") |> withSpinner()
          
        ),
        
        ### densidad ----
        cuadro_negro(
          bloque(h3("Frecuencia de eventos"), ancho = "200px"),
          
          plotOutput("evento_densidad", height = 480) |> withSpinner()
          
        ),
        
        
        ### desordenes ----
        cuadro_negro(
          h3("Desórdenes"),
          
          sliderInput("evento_desorden",
                      NULL,
                      min = 2016, max = 2023,
                      value = 2019, sep = ""),
          
          plotOutput("evento_desorden") |> withSpinner()
          
        ),
        
        ### ataques ----
        cuadro_negro(
          h3("Ataques"),
          
          plotOutput("evento_densidad_ataques", height = 600) |> withSpinner()
        )
        
        
      ),
      
      
      # # cruces de víctimas animadas (son demasiadas)
      # cuadro(
      #   bloque(h3("Víctimas"), ancho = "190px"),
      #   
      #   p("cada cruz representa a una víctima registrada"),
      #   
      #   div(style = css(height = "400px", 
      #                   overflow_y = "scroll"),
      #       
      #       # purrr::map(1:300, ~span("x")) # versión básica
      #       purrr::map(1:nrow(victimas), ~{
      #         # p("texto", .x), # para que sean líneas independientes
      #         span("x",
      #              # animación
      #              style = paste0("opacity: 0; ",
      #                             "animation: fade 2s ease forwards;", # controla duración de animación
      #                             "animation-delay: ", .x*0.1, "s;") # controla velocidad de aparición
      #         )
      #       })
      #   )
      # ),
      
      hr(),
      br(),
      
      ## cruces ----
      cuadro(
        subtitulo(ancho = "140px",
                  h2("Magnitud")
        ),
        
        bloque(h3("Cantidad total de víctimas"), ancho = "190px"),
        
        p("Cada cruz representa a una de las", 
          format(total_victimas, big.mark = ".", decimal.mark = ","), 
          "víctimas registradas."),
        
        p(em("Desplázate hacia abajo para comprender la gravedad de la cifra.")),
        
        div(style = css(height = paste0(alto_cuadros_victimas, "px"),
                        margin_left = "12px",
                        font_size = "135%",
                        letter_spacing = "5px",
                        overflow_y = "scroll",
                        overflow_x = "hidden"),
            
            # cantidad total, en texto
            rep("x", total_victimas) |>
              paste(collapse = " ")
        )
      ),
      
      
      hr(),
      
      ## mapas ----
      
      cuadro(
        subtitulo(ancho = "108px",
                  h2("Mapas")
        ),
        
        cuadro_negro(
          
          div(style = css(width = "320px", margin = "auto", text_align = "center"),
              sliderTextInput(
                inputId = "mapa_zoom",
                label = "Enfocar mapa", 
                hide_min_max = TRUE,
                grid = TRUE,
                force_edges = TRUE,
                choices = c("Palestina", "Gaza", "Cisjordania")
              )
          ),
          
          textOutput("texto_mapa", container = comando),
          
          plotOutput("mapa", height = 800) |> withSpinner()
        )
      ),
      
      
      
      
      # ), # fin columnas víctimas
      
      
      hr(),
      
      ## firma ----
      div(style = "padding: 16px; font-size: 90%; margin-top: -30px;",
          
          bloque(h4("Fuentes:"), ancho = "110px"),
          
          div(style = css(height = "8px")),
          
          markdown("- Palestine Datasets: https://data.techforpalestine.org/docs/killed-in-gaza/
                    - Armed Conflict Location & Event Data (ACLED): https://acleddata.com/israel-palestine/"),
          # markdown("- [redacted]
          #           - [redacted]"),
          
          div(style = css(margin_top = "26px"),
          
          
          markdown("Desarrollado en R por [Bastián Olea Herrera.](https://bastianolea.rbind.io)"),
          
          markdown("Puedes explorar mis otras [aplicaciones interactivas sobre datos sociales en mi portafolio.](https://bastianolea.github.io/shiny_apps/)"),
          
          markdown("Los datos y el código de fuente de esta app y de la obtención y procesamiento de los datos están [disponibles en el repositorio de GitHub.](https://github.com/bastianolea/palestina)")
          )
      )
  )
  
)


# —----

server <- function(input, output) {
  
  # víctimas ----
  ## muertes totales ----
  
  muertes_totales <- reactive({
    muertes |> 
      group_by(fecha) |> 
      summarize(muertos = sum(muertos),
                heridos = sum(heridos),
                .groups = "drop") |> 
      mutate(totales = cumsum(muertos))
    # mutate(alto_prop = if_else(muertos > quantile(muertos, 0.8), "alto", "común"),
    # alto_rel = if_else(muertos > input$muertes_acumuladas, "alto", "común"))
  })
  
  
  output$texto_muertes_acumuladas <- renderText({
    paste("> destacando eventos registrados con cantidad de víctimas letales mayor o igual a", input$muertes_acumuladas) 
  })
  
  ## víctimas acumuladas ----
  output$muertes_acumuladas <- renderPlot({
    muertes_totales() |> 
      ggplot() +
      aes(fecha, totales) +
      geom_area(alpha = .5) +
      geom_segment(data = ~filter(.x, muertos >= input$muertes_acumuladas),
                   aes(xend = fecha, yend = 0),
                   color = "red3", alpha = .8,
                   linewidth = 0.4) +
      scale_y_continuous(expand = c(0, 0),
                         labels = label_comma(big.mark = ".", 
                                              decimal.mark = ",")) +
      scale_x_date(expand = expansion(c(0.1, 0))) +
      labs(y = "víctimas totales, acumuladas",
           x = "fecha (mes, año)") +
      scale_x_date(date_breaks = "months", date_labels = "%m") +
      guides(x = guide_axis_nested(key = rango_años)) +
      tema_palestina
  })
  
  
  
  ## muertes por meses ----
  muertes_totales_mes <- reactive({
    muertes_totales() |> 
      mutate(fecha = floor_date(fecha, "month")) |> 
      group_by(fecha) |> 
      summarize(muertos = sum(muertos),
                heridos = sum(heridos),
                .groups = "drop")
  })
  
  output$muertes_totales_mes <- renderPlot({
    if (input$muertes_totales_mes == "Asesinados") {
      
      muertes_totales_mes() |> 
        ggplot() +
        aes(fecha, muertos) +
        geom_col(width = 15, alpha = .7) +
        scale_y_continuous(expand = c(0, 0),
                           labels = label_comma(big.mark = ".", 
                                                decimal.mark = ",")) +
        scale_x_date(date_breaks = "months", date_labels = "%m") +
        guides(x = guide_axis_nested(key = rango_años)) +
        labs(y = "muertos por mes", x = "fecha (mes, año)") +
        tema_palestina +
        theme(axis.text.x = element_text(margin = margin(t = 4)))
      
    } else if (input$muertes_totales_mes == "Heridos") {
      muertes_totales_mes() |> 
        ggplot() +
        aes(fecha, heridos) +
        geom_col(width = 15, alpha = .7) +
        scale_y_continuous(expand = c(0, 0),
                           labels = label_comma(big.mark = ".", 
                                                decimal.mark = ",")) +
        scale_x_date(date_breaks = "months", date_labels = "%m") +
        guides(x = guide_axis_nested(key = rango_años)) +
        labs(y = "heridos por mes", x = "fecha (mes, año)") +
        tema_palestina +
        theme(axis.text.x = element_text(margin = margin(t = 4)))
    }
  })
  
  
  
  # caracterización ----
  ## víctimas por edad o género ----
  
  output$victimas_edad <- renderPlot({
    
    if (input$victimas_edad == "Edad") {
      victimas |> 
        ggplot(aes(edad)) +
        geom_density(fill = color$principal, alpha = .4) +
        scale_y_continuous(expand = expansion(c(0, 0.03))) +
        scale_x_continuous(expand = expansion(c(0, 0.02))) +
        theme(axis.text.y = element_blank()) +
        labs(y = "proporción de víctimas por edad") +
        tema_palestina
      
    } else {
      victimas |> 
        ggplot() +
        aes(edad, fill = sexo, color = sexo) +
        geom_density(alpha = .5) +
        scale_y_continuous(expand = expansion(c(0, 0.03))) +
        scale_x_continuous(expand = expansion(c(0, 0.04))) +
        theme(axis.text.y = element_blank()) +
        labs(y = "proporción de víctimas\npor edad y género",
             fill = "género", color = "género") +
        guides(color = guide_legend(position = "inside"),
               fill = guide_legend(position = "inside")) +
        theme(legend.position.inside = c(0.8, 0.85),
              legend.title = element_blank(),
              legend.key.spacing.y = unit(2, "mm")) +
        tema_palestina
    }
  })
  
  ## víctimas por género pirámide ----
  
  output$victimas_piramide <- renderPlot({
    victimas_edad_sexo <- victimas |> 
      count(sexo, edad_c)
    
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
      tema_palestina +
      theme(legend.position = "top",
            legend.title = element_blank())
  })
  
  
  
  # eventos ----
  
  ## tipo ----
  output$evento_tipo <- renderPlot({
    eventos |>
      filter(fecha >= "2023-08-01",
             muertes > 0) |> 
      ggplot() +
      aes(fecha, muertes,
          size = muertes) +
      geom_point(aes(color = tipo_evento), alpha = .2) +
      guides(color = guide_legend(position = "right")) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_date(date_breaks = "months", date_labels = "%m",
                   expand = c(0, 0)) +
      guides(x = guide_axis_nested(key = rango_años),
             color = guide_legend(override.aes = list(size = 3, alpha = .5))) +
      tema_palestina +
      coord_cartesian(clip = "off") +
      theme(axis.text.x = element_text(margin = margin(t = 4)))
  })
  
  ## densidad ----
  output$evento_densidad <- renderPlot({
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
  })
  
  ## desorden ----
  output$evento_desorden <- renderPlot({
    # browser()
    
    # eventos_semana <- eventos |> 
    #   mutate(tipo_desorden = case_match(tipo_desorden,
    #                                     "Political violence; Demonstrations" ~ "Demonstrations",
    #                                     .default = tipo_desorden)) |> 
    #   mutate(fecha = floor_date(fecha, "month", week_start = 1)) |>
    #   group_by(año, fecha, tipo_desorden) |>
    #   summarize(n = n(),
    #             .groups = "drop")
    
    
    eventos_mes |>
      filter(año >= input$evento_desorden) |>
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
  })
  
  
  ## ataques ----
  output$evento_densidad_ataques <- renderPlot({
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
  })
  
  # mapa ----
  
  output$texto_mapa <- renderText({
    paste("> enfocando mapa en:", input$mapa_zoom)
  })
  
  
  zoom_palestina <- list(coord_sf(xlim = c(34, 36), 
                                  ylim = c(33.3, 29.5)))
  
  zoom_gaza <- list(coord_sf(xlim = c(34.15, 34.65), 
                             ylim = c(31.7, 31.1)))
  
  zoom_cisjordania <- list(coord_sf(xlim = c(34.65, 35.8), 
                                    ylim = c(32.6, 31.3)))
  
  # generar mapa base
  mapa_base <- reactive({
    
    ggplot() +
      annotate("rect", xmin = 32, xmax = 37,
               ymin = 35, ymax = 28, fill = NA) +
      geom_sf(data = mapa,
              aes(fill = admin), 
              alpha = .2) +
      geom_sf(data = eventos |> 
                filter(muertes > 0),
              aes(color = tipo_evento),
              alpha = 0.3, size = 1.3) +
      # zoom_palestina +
      guides(color = guide_legend(override.aes = list(size = 3, alpha = .5)),
             fill = guide_legend(ncol = 2, override.aes = list(height = unit(4, "mm"))))
  })
  
  
  # acercar mapa
  output$mapa <- renderPlot({
    
    if (input$mapa_zoom == "Palestina") {
      mapa_zoom <- mapa_base() + zoom_palestina
      
    } else if (input$mapa_zoom == "Gaza") {
      mapa_zoom <- mapa_base() + zoom_gaza  
      
    } else if (input$mapa_zoom == "Cisjordania") {
      mapa_zoom <- mapa_base() + zoom_cisjordania
    }
    
    # generar
    mapa_zoom +
      scale_fill_manual(values = c("Palestine" = "grey60",
                                   "Israel" = "grey30"), 
                        na.value = NA) +
      tema_palestina +
      theme(legend.key.height = unit(6, "mm"),
            legend.key.spacing.y = unit(0, "mm"),
            axis.text = element_text(color = color$detalle, size = 8)) +
      labs(fill = "Territorio",
           color = "Evento")
  })
  
  
  
  
}

shinyApp(ui = ui, server = server)
