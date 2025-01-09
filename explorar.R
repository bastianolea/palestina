library(dplyr)
library(readr)
library(lubridate)


# datos ----
victimas <- read_rds("palestina/pdatasets_victimas.rds")
muertes <- read_rds("palestina/pdatasets_muertes.rds")
eventos <- read_rds("palestina/acled_eventos.rds")

muertes
