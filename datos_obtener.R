# palestine datasets ----
# https://data.techforpalestine.org/docs/datasets/

## obtener datos ----

# https://data.techforpalestine.org/docs/killed-in-gaza/
download.file("https://data.techforpalestine.org/api/v2/killed-in-gaza.csv",
              "datos/palestine-datasets/killed-in-gaza.csv")

# https://data.techforpalestine.org/docs/casualties-daily/
download.file("https://data.techforpalestine.org/api/v2/casualties_daily.csv",
              "datos/palestine-datasets/casualties_daily.csv")

# https://data.techforpalestine.org/docs/casualties-daily-west-bank/
download.file("https://data.techforpalestine.org/api/v2/west_bank_daily.csv",
              "datos/palestine-datasets/west_bank_daily.csv")


# acled (conflictos): descarga manual
# https://acleddata.com/israel-palestine/