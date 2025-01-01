library(shiny)
library(bslib)
library(htmltools)

# colores ----
color <- list(fondo = "#151515",
              texto = "#DDDDDD",
              borde = "#BBBBBB",
              detalle = "#353535",
              principal = "#666666"
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


ui <- page_fluid(
  
  # tema ----
  theme = bslib::bs_theme(fg = color$texto, bg = color$fondo,
                          heading_font = font_google("JetBrains Mono"),
                          base_font = font_google("Space Grotesk"),
  ),
  
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
    HTML("h3 {font-size: 140%;}")),
  
  # color de enlaces
  tags$style(
    HTML("a {color:", color$principal, ";}")),
  
  
  ## fondo ----
  tags$style(
    HTML("body {
                background:", color$fondo, ";
                background-image: radial-gradient(", color$detalle, "1.2px, transparent 0);
                background-size: 18px 18px;
                }")),
  
  
  # ancho máximo de la app
  div(style = css(max_width = "1000px", margin = "auto"),
      
      
      # header ----
      div(style = css(height = "20px")),
      
      cuadro(
        # h1(style = css(background_color = color$fondo, max_width = "200px"), 
        bloque(
           h1("Palestina")
           ),
        p("Prueba de texto")
      ),
      
      
      
      # contenido ----
      cuadro(
        subtitulo(ancho = "160px",
          h2("Subtítulo")
        ),
        p("Prueba de texto")
      ),
      
      
      cuadro(
        bloque(h3("Otro texto"), ancho = "160px"),
        p("Prueba de texto"),
        
        bloque(h3("Otro texto"), ancho = "160px"),
        p("Prueba de texto", span("con énfasis", style = css(background_color = color$detalle))),
        
      ),
      
      # firma ----
      div(style = "padding: 26px; font-size: 90%;",
          
          bloque(h4("Fuentes:"), ancho = "110px"),
          div(style = css(height = "8px")),
          markdown("- Palestine Datasets: https://data.techforpalestine.org/docs/killed-in-gaza/
- Armed Conflict Location & Event Data (ACLED): https://acleddata.com/israel-palestine/"),
          
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
