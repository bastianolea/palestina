library(shiny)
library(bslib)
library(htmltools)
library(shinyWidgets)

library(readr)
library(dplyr) |> suppressPackageStartupMessages()
library(purrr)

library(ggplot2)
library(thematic)

library(sysfonts)
library(showtext)


# datos ----

victimas <- read_rds("pdatasets_victimas.rds")
muertes <- read_rds("pdatasets_muertes.rds")
eventos <- read_rds("acled_eventos.rds")

nombres <- victimas |> filter(edad < 18) |> pull(nombre) |> sample(300)

total_victimas <- nrow(victimas)



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
cuadro <- function(...) {
  div(style = css(margin = "8px", padding = "18px",
                  margin_bottom = "28px",
                  border = paste("2px solid", color$borde)),
      ...
  )
}

# título sobre bloque negro y sobre borde superior de un cuadro
subtitulo <- function(..., ancho = "200px") {
  div(style = css(text_align = "center", 
                  margin = "auto", margin_top = "-33px",
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




# opciones ----
showtext::showtext_opts(dpi = 180)
alto_cuadros_victimas = 700

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
                          text-decoration-thickness: 2px;}")),
  
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
    margin: 1.5rem 0;
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
      
      
      
      # contenido ----
      
      cuadro(
        subtitulo(ancho = "160px",
                  h2("Subtítulo")
        ),
        p("Prueba de texto"),
        p("Otro párrafo"),
        
        hr(),
        
        radioGroupButtons(
          inputId = "grupo",
          label = "Label",
          choices = c("A", 
                      "B", "C", "D"),
          selected = "B"
        ),
        
        
        div(class = "fade_in", p("texto")),
      ),
      
      
      cuadro(
        bloque(h3("Otro subtítulo"), ancho = "190px"),
        
        p("Prueba de texto"),
        
        sliderInput("slider", 
                    "Slider", 
                    min = 1, max = 10, value = 5),
        
        bloque(h3("Otro subtítulo"), ancho = "190px"),
        p("Prueba de texto", span("con énfasis", style = css(background_color = color$detalle))),
        
        actionButton("boton",
                     "Botón")
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
      
      ## víctimas ----
    
      # dos columnas
      layout_columns(
        
        ### cruces ----
        cuadro(
          bloque(h3("Cantidad total de víctimas"), ancho = "190px"),
          
          p("Cada cruz representa a una de las", 
            format(total_victimas, big.mark = ".", decimal.mark = ","), 
            "víctimas registradas."),
          
          p(em("Desplázate hacia abajo para comprender la gravedad de la cifra.")),
          
          div(style = css(height = paste0(alto_cuadros_victimas, "px"),
                          margin_left = "4px",
                          overflow_y = "scroll",
                          overflow_x = "hidden"),
              
              # cantidad total, en texto
              rep("x", total_victimas) |>
                paste(collapse = " ")
          )
        ),
        
        
        
        ### nombres ----
        # palabras que aparecen una tras otra
        cuadro(
          bloque(h3("Víctimas menores de edad"), ancho = "230px"),
          
          p(em("Cada nombre corresponde a una víctima confirmada de la guerra con menos de 18 años a la fecha de su muerte.")),
          
          p(em("Los nombres son elegidos al azar de entre las decenas de miles de víctimas registradas.")),
          
          
          div(style = css(height = paste0(alto_cuadros_victimas, "px"), 
                          overflow_y = "scroll"),
              
              # style = "column-count: 2;", # texto en múltiples columnas
              purrr::map(1:length(nombres), ~{
                # p("texto", .x), # para que sean líneas independientes
                span(nombres[.x], " / ", # para texto con flujo continuado
                     # animación
                     style = paste0("opacity: 0; ",
                                    "animation: fade 4s ease forwards;", # controla duración de animación
                                    "animation-delay: ", .x*0.5, "s;") # controla velocidad de aparición
                )
              })
          )
        )
      ), # fin columnas víctimas
      
      
      
      # firma ----
      div(style = "padding: 26px; font-size: 90%;",
          
          bloque(h4("Fuentes:"), ancho = "110px"),
          div(style = css(height = "8px")),
          markdown("- Palestine Datasets: https://data.techforpalestine.org/docs/killed-in-gaza/
                    - Armed Conflict Location & Event Data (ACLED): https://acleddata.com/israel-palestine/"),
          # markdown("- [redacted]
          #           - [redacted]"),
          
          br(),
          
          markdown("Desarrollado en R por [Bastián Olea Herrera.](https://bastianolea.rbind.io)"),
          
          markdown("Puedes explorar mis otras [aplicaciones interactivas sobre datos sociales en mi portafolio.](https://bastianolea.github.io/shiny_apps/)"),
          
          
          markdown("Los datos y el código de fuente de esta app y de la obtención y procesamiento de los datos están [disponibles en el repositorio de GitHub.](https://github.com/bastianolea/palestina)")
      )
  )
  
)

server <- function(input, output) {
}

shinyApp(ui = ui, server = server)
