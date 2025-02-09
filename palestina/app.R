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
library(sf)
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

# cifras
total_victimas <- sum(muertes$muertos)
# nombres <- victimas |> filter(edad < 18) |> pull(nombre) |> sample(300)
nombres <- victimas |> 
  filter(edad < 18) |> 
  slice_sample(n = 300) |> 
  mutate(nombre_edad = paste0(nombre, " (", edad, ")")) |> 
  pull(nombre_edad)


# listas
lista_desorden <- unique(eventos$tipo_desorden)
lista_evento <- unique(eventos$tipo_evento)
# lista_subevento <- unique(eventos$tipo_subevento)[1:13]
lista_subevento <- c("Violencia grupal",
                     "Protesta pacífica",
                     "Bombardeo/artillería/misiles",
                     "Ataque aéreo/drones",
                     "Enfrentamiento armado",
                     "Saqueos/destrucción de propiedad",
                     "Ataques",
                     "Protesta violenta",
                     "Denegación de uso de armas",
                     "Explosivo remoto/minas",
                     "Detenciones")


# colores ----
color <- list(fondo = "#151515",
              cuadricula = "#303030",
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

cuadro_negro <- function(..., alto = NULL, flujo = "scroll") {
  if (is.null(alto)) {
    div(style = css(margin = "8px", padding = "24px",
                    padding_bottom = "14px",
                    margin_bottom = "34px",
                    background_color = color$fondo,
                    # opacity = "100%",
                    overflow_y = flujo,
                    border = paste("2px solid", color$borde)),
        ...
    )
  } else {
    div(style = css(margin = "8px", padding = "24px",
                    padding_bottom = "14px",
                    margin_bottom = "34px",
                    background_color = color$fondo,
                    # opacity = "100%",
                    height = alto,
                    overflow_y = flujo,
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
                  text_indent = "-1.2em",
                  line_height = "1.3",
                  font_size = "75%",
                  margin_bottom = "14px",
                  margin_top = "12px",
                  background_color = color$fondo,
                  # color = color$principal,
                  # opacity = "70%"
  ),
  p(...,
    style = "font-family: JetBrains Mono !important;")
  )
}

salto <- function() {
  div(hr(), style = css(margin_top = "46px", 
                        margin_bottom = "-20px",
                        margin_left = "-8px"))
}

enlaces <- function(titulo, enlace, texto = NULL, ...) {
  div(style = css(...),
      a(style = css(color = color$texto),
        h3(titulo),
        href = enlace),
      if (!is.null(texto)) p(texto) 
  )
}

# leyenda de años
rango_años <- key_range_manual(
  start = c("2023-01-05", "2024-01-05", "2025-01-05") |> ymd(), 
  end = c("2023-12-25", "2024-12-25", "2025-12-25") |> ymd(), 
  name = c("2023", "2024", "2025")
)

# tema ggplot ----
tema_palestina <- theme(text = element_text(family = "Space Grotesk", color = color$texto, size = 5)) +
  theme(axis.ticks = element_blank()) +
  # fondo
  theme(panel.background = element_rect(fill = color$fondo),
        plot.background =  element_rect(fill = color$fondo)) +
  # grilla
  theme(panel.grid = element_line(color = color$detalle,
                                  linetype = "dotted"),
        panel.grid.major = element_line(linewidth = 0.3),
        panel.grid.minor = element_line(linewidth = 0.2)) +
  # texto
  theme(strip.text = element_text(size = 10),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 6))

# opciones ----
options(spinner.type = 1, spinner.color = color$principal)
library(ragg)
options(shiny.useragg = TRUE)
# showtext_auto()
# showtext::showtext_opts(dpi = 180) # tamaño del texto
resolucion = 130


# ui ----

ui <- page_fluid(
  title = "Palestina",
  lang = "es",
  
  ## tema ----
  theme = bslib::bs_theme(fg = color$texto, 
                          bg = color$fondo,
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
    margin-top: -0.7rem;
    margin-bottom: 3.3rem;
    color: inherit;
    border: 0;
    border-top: 2px dashed", color$borde, ";
    opacity: 1;
  }")),
  
  
  # pestañas 
  tags$style(
    HTML(".card-body {
    padding: 0 !important;
    
    }
    .card, .bslib-card, .tab-content {
    background-color: transparent !important; /* fondo de las pestañas */
    }
    
         .nav-link {
         font-size: 150% !important;
         margin-bottom: 18px;
         }")),
  
  # labels
  tags$style(
    HTML(".control-label {
    margin-bottom: 0px !important;}")
  ),
  
  # selectores
  tags$style(
    HTML(".selectize-input {
    margin-top: 8px !important;
         border: 2px solid", color$borde, "!important;
          border-radius: 0 !important;
         }")),
  
  # picker
  tags$style(
    HTML(".dropdown-toggle {
    margin-top: 8px !important;
         border: 2px solid", color$borde, "!important;
          border-radius: 0 !important;
          background-color:", color$fondo, "!important;
          font-family:", tipografia$titulos, "!important;
    }
         .filter-option-inner-inner {
         font-size: 90%;
          font-family:", tipografia$titulos, "!important;
         }")),
  
  # checkbox
  tags$style(
    HTML(".shiny-input-checkbox {
    border: 2px solid", color$borde, "!important;
          border-radius: 0 !important;
          background-color:", color$fondo, "!important;
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
          height: 36.5px !important;
          padding-top: 6px !important;
          padding-bottom: 6px !important;
          padding-left: 18px !important;
          padding-right: 18px !important;
          font-size: 90% !important;
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
                background-image: radial-gradient(", color$cuadricula, "1.2px, transparent 0);
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
        
        div(style = css(opacity = "45%", margin_bottom = "10px"),
            comando("> desarrollado por", 
                    a("bastián olea", href = "https://bastianolea.rbind.io",
                      style = paste("font-family:", tipografia$titulos, "!important;",
                                    "color:", color$texto, ";"))
            )
        ),
        
        div(style = css(margin_bottom = "-14px"),
            p("Visualizador de datos que busca reflejar el horror del genocidio que se está llevando a cabo en el territorio palestino por obra de Israel y sus aliados."),
        )
        
      ),
      
      hr(),
      
      div(
        style = css(margin_top = "-30px",
                    margin_bottom = "35px"),
        comando(paste("> datos actualizados:"),
                "muertes, heridos y nombres de víctimas (Palestine Datasets)", max(muertes$fecha) |> format("%d/%m/%Y"),
                " / sucesos de violencia política (ACLED)", max(eventos$fecha) |> format("%d/%m/%Y")
        )
      ),
      
      div(style = css(margin_bottom = "-25px"),
          hr()
      ),
      
      # —----
      
      
      # contenido ----
      
      
      navset_card_underline(
        
        nav_panel("Datos",
                  
                  ## víctimas ----
                  cuadro(
                    subtitulo(ancho = "160px", 
                              h2("Víctimas")
                    ),
                    
                    p("Datos sobre las víctimas letales del conflicto en Palestina."),
                    ### acumuladas ----
                    
                    
                    cuadro_negro(
                      flujo = "hidden",
                      bloque(h3("Muertes totales acumuladas"), ancho = "230px"),
                      
                      p("Esto gráfico de área retrata el conteo total de víctimas acumuladas a través del tiempo. Utiliza el primer selector para destacar los días de mayor cantidad de víctimas, y el segundo selector para filtrar las víctimas letales según la zona en que ocurre la masacre."),
                      
                      layout_columns(
                        sliderInput("muertes_acumuladas", 
                                    "Destacar masacres", 
                                    min = 0, max = 500, value = 100, step = 50),
                        
                        div(
                          p(class = "control-label", "Zona"),
                          selectInput("muertes_zona",
                                      NULL,
                                      choices = c("Palestina", "Gaza", "Cisjordania"))
                        )
                      ),
                      
                      textOutput("texto_muertes_acumuladas", container = comando),
                      
                      plotOutput("muertes_acumuladas", height = 300) |> withSpinner(),
                      
                      comando("> un gráfico acumulado representa un conteo aditivo, es decir, la cifra total de muertes acumulada día tras día."),  
                    ),
                    
                    
                    
                    ### mes ----
                    
                    cuadro_negro(
                      flujo = "hidden",
                      bloque(h3("Víctimas por mes"), ancho = "230px"),
                      
                      p("Esta visualización resume mensualmente la cantidad de víctimas del conflicto. Filtra la visualización entre personas asesinadas o personas heridas, y utiliza el segundo selector para distinguir entre la zona de ocurrencia de los hechos."),
                      
                      layout_columns(
                        div(
                          p(class = "control-label", "Variable",
                            style = "margin-bottom: 8px !important;"),
                          
                          radioGroupButtons(
                            inputId = "muertes_totales_mes",
                            label = NULL,
                            choices = c("Asesinados", "Heridos"), 
                            width = "100%"
                          )
                        ),
                        
                        div(
                          p(class = "control-label", "Zona"),
                          selectInput("muertes_mes_zona",
                                      NULL,
                                      choices = c("Palestina", "Gaza", "Cisjordania"))
                        )
                      ),
                      
                      plotOutput("muertes_totales_mes", height = 300) |> withSpinner()
                    )
                    
                    
                  ), #fin cuadro
                  
                  hr(),
                  
                  ## caracterización víctimas ----
                  cuadro(
                    subtitulo(ancho = "300px",
                              h2("Caracterización de víctimas")
                    ),
                    
                    p("Apartado en el que se intenta caracterizar, con la poca información actualmente existente, a las víctimas letales de la incursión colonialista de Israel en Palestina."),
                    
                    layout_columns(
                      
                      ### distribución edad ----
                      cuadro_negro(alto = "800px", flujo = "scroll",
                                   bloque(h3("Víctimas letales por edad"), ancho = "260px"),
                                   
                                   p("Distribución de las víctimas según su edad y género."),
                                   
                                   radioGroupButtons(
                                     inputId = "victimas_edad",
                                     label = NULL,
                                     choices = c("Edad", "Género")
                                   ),
                                   
                                   plotOutput("victimas_edad", height = 330) |> withSpinner(),
                                   
                                   comando("> los gráficos de densidad distribuyen los casos horizontalmente, donde la altura de la curva representa la cantidad de casos en ese punto de la variable horizontal (edad). La mayoría de las víctimas son menores de 30 años, pero si desagregamos por género, se evidencia una diferencia en la edad promedio."),
                      ),
                      
                      ### nombres ----
                      # palabras que aparecen una tras otra
                      cuadro(alto = "800px", flujo = "hidden",
                             bloque(h3("Víctimas menores de edad"), ancho = "230px"),
                             
                             p(em("Cada nombre corresponde a una víctima confirmada con menos de 18 años a la fecha de su muerte.")),
                             
                             p(em("Los nombres son elegidos al azar de entre las decenas de miles de víctimas registradas.")),
                             
                             
                             div(style = css(max_height = "500px", 
                                             overflow_y = "scroll"),
                                 
                                 # style = "column-count: 2;", # texto en múltiples columnas
                                 purrr::map(1:length(nombres), ~{
                                   # p("texto", .x), # para que sean líneas independientes
                                   span(nombres[.x], " / ", # para texto con flujo continuado
                                        # animación
                                        style = paste0("opacity: 0; font-size: 90%;",
                                                       "animation: fade 4s ease forwards;", # controla duración de animación
                                                       "animation-delay: ", .x*1.1, "s;") # controla velocidad de aparición
                                   )
                                 })
                             )
                      )
                    ), # fin columnas
                    
                    
                    ### pirámide ----
                    
                    div(style = css(margin_top = "-36px"),
                        cuadro_negro(
                          flujo = "hidden",
                          bloque(h3("Víctimas letales por género"), ancho = "260px"),
                          
                          p("Pirámide de población con las edades y géneros de las víctimas."),
                          
                          plotOutput("victimas_piramide") |> withSpinner(),
                          
                          comando("> las pirámides de población permiten analizar rápidamente la distribución poblacional afectada; en este caso, podemos ver una concentración de las víctimas jóvenes y de mediana edad, más sesgada hacia los hombres.")
                        )
                    )
                  ),
                  
                  
                  hr(),
                  
                  
                  ## eventos ----
                  cuadro(
                    subtitulo(ancho = "140px",
                              h2("Sucesos")
                    ),
                    
                    p("En este apartado se presentan visualizaciones de datos que corresponden a eventos de violencia política, protestas, hechos de violencia, y otros eventos políticamente relevantes."),
                    
                    p("Cada evento se refiere a un suceso político puntual, con una fecha y ubicación específicas."),
                    
                    ### tipo ----
                    cuadro_negro(
                      flujo = "hidden",
                      bloque(h3("Sucesos políticos mensuales"), ancho = "280px"),
                      
                      p("Gráfico de serie de tiempo que indica la cantidad de eventos o sucesos políticos mensuales a través del tiempo. Utiliza el selector de fechas para acotar la fecha de inicio del gráfico, el selector de sucesos para agregar o remover eventos políticos específicos, y el botón cuadrado para indicar en el gráfico la víctimas letales asociadas a cada mes."),
                      
                      layout_columns(
                        sliderInput("evento_tipo_año",
                                    "Fecha",
                                    min = 2016, max = 2023,
                                    value = 2019, sep = ""),
                        
                        pickerInput("evento_tipo",
                                    label = "Sucesos",
                                    choices = lista_evento,
                                    selected = c("Violencia remota/explosivos",
                                                 "Enfrentamientos",
                                                 "Protestas",
                                                 "Violencia contra civiles"),
                                    multiple = TRUE)
                      ),
                      
                      checkboxInput("evento_tipo_muertes", label = "Mostrar muertes", value = FALSE),
                      
                      plotOutput("evento_tipo") |> withSpinner(),
                      
                      comando("> en esta serie de tiempo, cada línea representa un tipo de suceso. La posición de la línea en el eje horizontal identifica el momento en el tiempo, mientras que su posición vertical o altura representa la cantidad de sucesos de su tipo ocurridos en ese momento."),
                      
                      comando("> esta visualización representa el aumento de la violencia ejercida contra el pueblo palestino en distintos momentos del tiempo, así como focos de resistencia ante ella.")
                      
                    ),
                    
                    ### densidad ----
                    cuadro_negro(
                      flujo = "hidden",
                      bloque(h3("Frecuencia de eventos"), ancho = "200px"),
                      
                      p("Gráfico que refleja la frecuencia de ocurrencia de los distintos sucesos a través del tiempo. Las figuras representan la distribución de todos los sucesos políticos y hechos de violencia, engrosándose en las fechas donde estos fueron más frecuentes."),
                      
                      pickerInput("evento_densidad",
                                  label = "Eventos",
                                  choices = lista_evento,
                                  selected = c("Enfrentamientos",
                                               "Violencia remota/explosivos",
                                               "Violencia contra civiles"),
                                  multiple = TRUE, 
                                  options = pickerOptions(maxOptions = 3L,
                                                          maxOptionsText = "Máximo 3")
                      ),
                      
                      plotOutput("evento_densidad", height = 480) |> withSpinner(),
                      
                      comando("> los gráficos de densidad permiten expresar visualmente la distribución de los hechos a través de una variable."),
                      comando("> en este caso, la variable horizontal es el tiempo, por lo que cada figura aumenta o disminuye su tamaño en un momento del tiempo dependiendo de si el suceso que representa fue más o menos frecuente.")
                      
                    ),
                    
                    
                    ### ataques ----
                    cuadro_negro(
                      flujo = "hidden",
                      bloque(h3("Eventos específicos"), ancho = "200px"),
                      
                      p("Los siguientes gráficos, al igual que los del apartado anterior, representan la frecuencia de ocurrencia de sucesos, pero de un mayor grado de especificidad, y particularmente relacionados a violencia política, hechos de guerra, y resistencia."),
                      
                      pickerInput("evento_densidad_ataques",
                                  label = "Subeventos",
                                  choices = lista_subevento,
                                  selected = c("Bombardeo/artillería/misiles",
                                               "Ataque aéreo/drones",
                                               "Enfrentamiento armado",
                                               "Ataques"),
                                  multiple = TRUE, 
                                  options = pickerOptions(maxOptions = 4L,
                                                          maxOptionsText = "Máximo 4")
                      ),
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
                  # br(),
                  
                  ## cruces ----
                  cuadro(
                    subtitulo(ancho = "140px",
                              h2("Magnitud")
                    ),
                    
                    bloque(h3("Cantidad total de víctimas"), ancho = "190px"),
                    
                    p("Cada cruz representa a una de las", 
                      format(total_victimas, big.mark = ".", decimal.mark = ","), 
                      "víctimas palestinas registradas al momento, a partir del 7 de octubre de 2023. Esta cifra no representa la cantidad total de víctimas, debido a que la escala de la violencia es tal que la tarea de registrar e identificar cuerpos se dificulta."),
                    
                    p(em("Desplázate hacia abajo para comprender la gravedad de la cifra.")),
                    
                    div(style = css(height = "500px",
                                    margin_left = "12px",
                                    margin_top = "18px",
                                    margin_bottom = "18px",
                                    line_height = "1.3em",
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
                    
                    p("Mapa interactivo de todos los eventos georeferenciados con relación al conflicto entre Israel y Palestina."),
                    
                    cuadro_negro(
                      flujo = "hidden",
                      p("Utiliza los siguientes controles para explorar la distribución espacial de los hechos de violencia, enfrentamientos, protestas, y ataques ocurridos en el territorio palestino."),
                      
                      p("Los controles permiten enfocar el mapa en toda Palestina, en la franja de Gaza o en Cisjordania, filtrar el tipo de evento que se visualiza en el mapa, filtrar los sucesos según su ocurrencia en el tiempo, y finalmente filtrar cada punto del mapa según la cantidad de víctimas letales de cada suceso, opcionalmente pudiendo hacer que el tamaño de los puntos dependa de la letalidad de los hechos."),
                      
                      layout_columns(
                        # div(style = css(width = "320px", margin = "auto", text_align = "center"),
                        sliderTextInput(
                          inputId = "mapa_zoom",
                          label = "Enfocar mapa", 
                          hide_min_max = TRUE,
                          grid = TRUE,
                          force_edges = TRUE,
                          choices = c("Palestina", "Gaza", "Cisjordania")
                        ),
                        
                        pickerInput("mapa_tipo",
                                    label = "Subventos",
                                    choices = lista_subevento,
                                    selected = c("Enfrentamiento armado",
                                                 "Ataque aéreo/drones",
                                                 "Bombardeo/artillería/misiles",
                                                 "Protesta pacífica"),
                                    multiple = TRUE, 
                                    # options = pickerOptions(maxOptions = 3L,
                                    #                         maxOptionsText = "Máximo 3")
                        )
                      ),
                      
                      layout_columns(
                        sliderInput("mapa_año",
                                    "Fecha",
                                    min = 2016, max = 2025,
                                    value = c(2023, 2025), ticks = T,
                                    sep = ""),
                        
                        sliderInput("mapa_muertes", 
                                    "Letalidad", 
                                    min = 0, max = 100, value = 0),
                      ),
                      
                      checkboxInput("mapa_muertes_size", label = "Mostrar muertes", value = FALSE),
                      
                      div(style = css(min_width = "600px"),
                          plotOutput("mapa", height = 800) |> withSpinner()
                      ),
                      
                      textOutput("texto_mapa", container = comando)
                    )
                  ),
                  
                  
                  hr(),
                  
                  ## fuentes ----
                  div(style = "padding: 16px; font-size: 90%; margin-top: -30px;",
                      
                      bloque(h4("Fuentes:"), ancho = "110px"),
                      
                      div(style = css(height = "8px")),
                      
                      markdown("- Palestine Datasets: https://data.techforpalestine.org/docs/killed-in-gaza/
                    - Armed Conflict Location & Event Data (ACLED): https://acleddata.com/israel-palestine/"),
                      # markdown("- [redacted]
                      #           - [redacted]"),
                      
                      div(style = css(margin_top = "26px", margin_bottom = "26px"),
                          
                          
                          markdown("Desarrollado en R por [Bastián Olea Herrera.](https://bastianolea.rbind.io)"),
                          
                          markdown("Puedes explorar mis otras [aplicaciones interactivas sobre datos sociales en mi portafolio.](https://bastianolea.github.io/shiny_apps/)"),
                          
                          markdown("Los datos, el código de fuente de esta app, y el código de la obtención y procesamiento de los datos están [disponibles en el repositorio de GitHub.](https://github.com/bastianolea/palestina)")
                      )
                  )
        ),
        
        # campañas ----
        nav_panel("Campañas",
                  
                  
                  cuadro(
                    subtitulo(ancho = "160px", 
                              h2("Campañas")
                    ),
                    
                    
                    p("Campañas de donación verificadas para apoyar directamente a víctimas del genocidio en Palestina."),
                    
                    br(),
                    
                    div(style = css(color = color$texto),
                        a(style = css(color = color$texto),
                          h3("Ayuda a un estudiante de medicina y su familia atrapados en Gaza"), 
                          href = "https://www.gofundme.com/f/help-amedical-student-stuck-in-gaza"),
                        p("Campaña en apoyo a Anas Ayesh, estudiante de medicina palestino, quien luego de estudiar fuera de su país, regresó a Gaza a visitar a su familia pero se vio atrapado ahí por la guerra, viendo sus estudios interrumpidos."),
                        
                        salto(),
                        
                        a(style = css(color = color$texto),
                          h3("Ayuda a una familia a escapar del flagelo de la guerra en Gaza"),
                          href = "https://www.gofundme.com/f/your-donation-help-us-get-out-of-gaza?attribution_id=sl:7694aad2-d014-41ee-a0b1-40c3c9984bc1&utm_campaign=fp_sharesheet&utm_medium=customer&utm_source=copy_link"),
                        p("Campaña en apoyo a Fadwa Hussein (32) y su esposo Ahmed Al-Kurd (38), padres de 4 hijxs de entre 2 y 11 años, para sobrevivir en Gaza y eventualmente huir a un lugar seguro."),
                        
                        salto(),
                        
                        
                        a(style = css(color = color$texto),
                          h3("Apoya a Mohammed Imar, refugiado que evacuó a Egipto, pero cuya familia sigue en Gaza"),
                          href = "https://www.instagram.com/mohammed_n_imad?__pwa=1"),
                        p("Mohammed también imparte clases de árabe-español."),
                        
                        br()
                    )
                  )
                  
        ),
        
        
        # información ----
        nav_panel("Más información",
                  
                  cuadro(
                    subtitulo(ancho = "190px", 
                              h2("Información")
                    ),
                    
                    
                    p("Enlaces de información acerca de los sucesos en Gaza y Palestina."),
                    
                    br(),
                    
                    enlaces("Boicot a empresas internacionales que apoyan a Israel",
                            "https://bdsmovement.net/Act-Now-Against-These-Companies-Profiting-From-Genocide",
                            "Boicot a empresas que apoyan a Israel"
                    ),
                    
                    salto(),
                    
                    enlaces("Boicot a empresas en Chile que apoyan a Israel",
                            "https://www.instagram.com/bdschile/?hl=es"),
                    
                    salto(),
                    
                    
                    enlaces("Mapas: selección de mapas y noticias sobre ataques en Gaza",
                            "https://www.nytimes.com/interactive/2023/10/07/world/middleeast/israel-gaza-maps.html",
                            "New York Times"),
                    
                    salto(),
                    
                    enlaces("Mapas: la destrucción de Gaza",
                            "https://www.reuters.com/graphics/ISRAEL-PALESTINIANS/MAPS/zjvqedgdjvx/",
                            "Reuters"),
                    
                    salto(),
                    
                    enlaces("Datos y gráficos: Israel y Palestina",
                            "https://acleddata.com/israel-palestine/",
                            "Armed Conflict Location and Event Data Project (ACLED)")
                  )
        )
        
      )
  )
  
  
)


# —----

server <- function(input, output) {
  
  # víctimas ----
  
  
  ## víctimas acumuladas ----
  muertes_acumuladas <- reactive({
    if (input$muertes_zona == "Palestina") {
      muertes |> 
        group_by(fecha) |> 
        summarize(muertos = sum(muertos),
                  heridos = sum(heridos),
                  .groups = "drop") |> 
        mutate(totales = cumsum(muertos))
      
    } else {
      muertes |> 
        filter(zona == input$muertes_zona) |> 
        mutate(totales = cumsum(muertos))
    }
  })
  
  output$texto_muertes_acumuladas <- renderText({
    paste("> destacando eventos registrados con cantidad de víctimas letales mayor o igual a", input$muertes_acumuladas) 
  })
  
  output$muertes_acumuladas <- renderPlot({
    muertes_acumuladas() |> 
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
  }, res = resolucion)
  
  
  
  ## muertes por meses ----
  muertes_totales_mes <- reactive({
    if (input$muertes_mes_zona != "Palestina") {
      muertes <- muertes |> 
        filter(zona == input$muertes_mes_zona)
    }
    
    muertes |> 
      mutate(fecha = floor_date(fecha, "month")) |> 
      group_by(fecha) |> 
      summarize(muertos = sum(muertos),
                heridos = sum(heridos),
                .groups = "drop")
  })
  
  output$muertes_totales_mes <- renderPlot({
    if (input$muertes_totales_mes == "Asesinados") {
      plot <- muertes_totales_mes() |> 
        ggplot() +
        aes(fecha, muertos)
      palabra <- "muertes"
    } else if (input$muertes_totales_mes == "Heridos") {
      plot <- muertes_totales_mes() |> 
        ggplot() +
        aes(fecha, heridos)
      palabra <- "heridos"
    }
    
    plot +
      geom_col(width = 15, alpha = .7) +
      scale_y_continuous(expand = c(0, 0),
                         labels = label_comma(big.mark = ".", 
                                              decimal.mark = ",")) +
      scale_x_date(date_breaks = "months", date_labels = "%m",
                   expand = expansion(c(0.02, 0.02))) +
      guides(x = guide_axis_nested(key = rango_años)) +
      labs(y = paste(palabra, "por mes"), x = "fecha (mes, año)") +
      tema_palestina +
      theme(axis.text.x = element_text(margin = margin(t = 4)))
  }, res = resolucion)
  
  
  
  # caracterización ----
  ## víctimas por edad o género ----
  
  output$victimas_edad <- renderPlot({
    
    if (input$victimas_edad == "Edad") {
      victimas |> 
        ggplot(aes(edad)) +
        geom_density(fill = color$principal, alpha = .4) +
        geom_vline(xintercept = 18, color = "red3", linewidth = .7, alpha = .7) +
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
        geom_vline(xintercept = 18, color = "red3", linewidth = .7, alpha = .7) +
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
  }, res = resolucion)
  
  
  
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
            legend.title = element_blank(),
            legend.key.height = unit(4, "mm"),
            legend.text = element_text(margin = margin(l = 4, r = 6)))
  }, res = resolucion)
  
  
  
  # eventos ----
  
  # ## tipo
  # output$evento_tipo <- renderPlot({
  #   eventos |>
  #     filter(tipo_evento %in% input$evento_tipo) |> 
  #     mutate(muertes = ifelse(muertes > 40, 40, muertes)) |> 
  #     filter(fecha >= "2023-08-01") |> 
  #     ggplot() +
  #     aes(fecha, muertes,
  #         size = muertes) +
  #     geom_jitter(aes(color = tipo_evento), 
  #                 alpha = .1, width = 0, height = .2) +
  #     guides(color = guide_legend(position = "right")) +
  #     scale_y_continuous(expand = c(0.02, 0)) +
  #     scale_x_date(date_breaks = "months", date_labels = "%m",
  #                  expand = c(0, 0)) +
  #     guides(x = guide_axis_nested(key = rango_años),
  #            size = guide_none(),
  #            color = guide_legend(title = NULL, position = "top", nrow = 2,
  #                                 override.aes = list(size = 3, alpha = .5))) +
  #     tema_palestina +
  #     coord_cartesian(clip = "off") +
  #     labs(x = "fecha (mes, año)") +
  #     theme(axis.text.x = element_text(margin = margin(t = 4)),
  #           panel.grid.minor = element_blank()) +
  #     theme(legend.text = element_text(margin = margin(l = 1)))
  # })
  # 
  
  
  ## tipo ----
  output$evento_tipo <- renderPlot({
    # browser()
    
    # eventos_semana <- eventos |> 
    #   mutate(tipo_desorden = case_match(tipo_desorden,
    #                                     "Political violence; Demonstrations" ~ "Demonstrations",
    #                                     .default = tipo_desorden)) |> 
    #   mutate(fecha = floor_date(fecha, "month", week_start = 1)) |>
    #   group_by(año, fecha, tipo_desorden) |>
    #   summarize(n = n(),
    #             .groups = "drop")
    # browser()
    # dev.new()
    
    plot <- eventos_mes |>
      filter(año >= input$evento_tipo_año) |>
      filter(tipo_evento %in% input$evento_tipo) |> 
      ggplot() +
      aes(fecha, n, color = tipo_evento)
    
    if (input$evento_tipo_muertes) {
      plot <- plot +
        geom_line(linewidth = 0.7, alpha = .3) +
        geom_point(data = ~filter(.x, muertes > 0),
                   aes(size = muertes),
                   alpha = .8)
    } else {
      plot <- plot +
        geom_line(linewidth = 0.7, alpha = .8)
    }
    
    plot <- plot +
      scale_y_continuous(expand = c(0.01, 0.01)) +
      scale_x_date(date_breaks = "years", date_labels = "%Y",
                   expand = c(0, 0)) +
      scale_size_binned(range = c(.5, 8),
                        labels = label_comma(big.mark = ".", decimal.mark = ",")) +
      tema_palestina +
      coord_cartesian(clip = "off") +
      guides(color = guide_legend(title = NULL, position = "inside",
                                  override.aes = list(size = 3, alpha = .7)),
             size = guide_legend(position = "top")) +
      theme(legend.position.inside = c(0.2, 0.85)) +
      labs(y = "cantidad de eventos", x = NULL)
    
    if (input$evento_tipo_año >= 2022) {
      plot <- plot +
        scale_x_date(date_breaks = "months", date_labels = "%m",
                     expand = c(0, 0)) +
        guides(x = guide_axis_nested(key = rango_años)) +
        theme(axis.text.x = element_text(margin = margin(t = 4)),
              panel.grid.minor = element_blank())
    }
    
    plot
  }, res = resolucion)
  
  ## densidad ----
  output$evento_densidad <- renderPlot({
    # browser()
    # dev.new()
    eventos |>
      filter(año >= 2023) |> 
      filter(tipo_evento %in% input$evento_densidad) |> 
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
      facet_wrap(vars(tipo_evento), ncol = 1) +
      tema_palestina +
      theme(axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            strip.text = element_text(size = 7)) +
      labs(x = "fecha (mes, año)")
  }, res = resolucion)
  
  
  ## ataques ----
  output$evento_densidad_ataques <- renderPlot({
    eventos |>
      filter(año >= 2023) |> 
      filter(tipo_subevento %in% input$evento_densidad_ataques) |> 
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
      facet_wrap(vars(tipo_subevento), ncol = 1) +
      tema_palestina +
      theme(axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            strip.text = element_text(size = 7)) +
      labs(x = "fecha (mes, año)")
  }, res = resolucion)
  
  # mapa ----
  
  # texto inferior del mapa
  output$texto_mapa <- renderText({
    paste0("> enfocando mapa en la zona de ", input$mapa_zoom, ", ",
           "entre los años ", input$mapa_año[1], " y ", input$mapa_año[2], ", ",
           ifelse(input$mapa_muertes_size,
                  "donde el tamaño de los puntos indica la cantidad de muertes, ",
                  ""),
           ifelse(input$mapa_muertes == 0,
                  "incluyendo eventos con o sin víctimas letales.",
                  paste("incluyendo sólo los eventos con más de",
                        input$mapa_muertes, "víctimas letales."))
    )
  })
  
  # definición de los acercamientos al territorio
  zoom_palestina <- list(coord_sf(xlim = c(34, 36), 
                                  ylim = c(33.3, 29.5)))
  
  zoom_gaza <- list(coord_sf(xlim = c(34.15, 34.65), 
                             ylim = c(31.7, 31.1)))
  
  zoom_cisjordania <- list(coord_sf(xlim = c(34.65, 35.8), 
                                    ylim = c(32.6, 31.3)))
  
  # filtrado de datos del mapa
  datos_mapa <- reactive({
    # browser()
    eventos |> 
      mutate(año >= input$mapa_año[1],
             año <= input$mapa_año[2]) |> 
      filter(muertes >= input$mapa_muertes) |> 
      filter(tipo_subevento %in% input$mapa_tipo)
  })
  
  # generar mapa base
  mapa_base <- reactive({
    
    # mapa base
    plot <- ggplot() +
      annotate("rect", xmin = 32, xmax = 37,
               ymin = 35, ymax = 28, fill = NA) +
      geom_sf(data = mapa,
              aes(fill = admin), 
              alpha = .2)
    
    # tamaño según muertes
    if (input$mapa_muertes_size) {
      plot <- plot +
        geom_sf(data = datos_mapa(),
                aes(color = tipo_subevento,
                    size = muertes),
                alpha = 0.2)
    } else {
      plot <- plot +
        geom_sf(data = datos_mapa(),
                aes(color = tipo_subevento),
                alpha = 0.3, size = 1.3) 
    }
    
    # escalas
    plot <- plot +
      scale_size_binned(range = c(.5, 25),
                        breaks = c(0, 20, 60, 100, 300),
                        limits = c(0, 300),
                        labels = label_comma(big.mark = ".", decimal.mark = ",")) +
      guides(color = guide_legend(override.aes = list(size = 3, alpha = .5)),
             size = guide_legend(override.aes = list(color = color$principal)),
             fill = guide_legend(ncol = 2, override.aes = list(height = unit(4, "mm"))))
    
    plot
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
      theme(legend.key.height = unit(4, "mm"),
            legend.key.spacing.y = unit(0, "mm"),
            axis.text = element_text(color = color$detalle, size = 8)) +
      labs(fill = "Territorio",
           color = "Evento")
  }, res = resolucion)
}

shinyApp(ui = ui, server = server)
