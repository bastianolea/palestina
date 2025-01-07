library(sysfonts)
library(showtext)


# descargar tipografía local (descargada con gfonts)
gfonts::setup_font(id = "jetbrains-mono", output_dir = "palestina/www/") # instalar tipografía localmente
gfonts::setup_font(id = "space-grotesk", output_dir = "palestina/www/")

# gfonts::use_font("jetbrains-mono", "palestina/www/css/jetbrains-mono.css")
# gfonts::use_font("space-grotesk", "www/css/space-grotesk.css")

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


showtext_auto()
