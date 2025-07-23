# -----------------------
# [1] PACOTES NECESSÁRIOS
# -----------------------

library(dplyr)
library(tidyr)
library(tidyverse)
library(leaflet)
library(sf)

# ------------------------------
# [2] LEITURA E AJUSTE DOS DADOS
# ------------------------------

# -------------
# [2.1] LEITURA
# -------------

# Caminho do arquivo.rds
path <- "~/PROJETOS/R/DASHBOARD/SHINY DASH/V1/CONJUNTO DE DADOS/GEOPA.rds"

# Leitura do arquivo.rds
GEOPA <- readRDS(path)

# Caminho do arquivo.CSV
path <- "~/PROJETOS/R/DASHBOARD/SHINY DASH/V1/CONJUNTO DE DADOS/DATA_GEN_V1.csv"

# Leitura do arquivo.CSV
VENDAS.DF <- read_csv(path)

# -------------
# [2.2] AJUSTES
# -------------

# Municípios
citys <- c("Belém", "Ananindeua", "Marituba", "Benevides", "Santa Bárbara do Pará")

# Filtro dos Municípios
GEOPA.filter <- GEOPA %>% 
  filter(
    name_muni %in% citys
  ) %>%
  select(name_muni, geom) %>%
  mutate(
    CITY = stringr::str_to_upper(name_muni),
  ) %>%
  select(-name_muni)

# Armazanando Dados Geográficos
df.map <- VENDAS.DF %>%
  left_join(GEOPA.filter, by = "CITY") %>%
  select(CITY, VALOR_TOTAL, geom) %>%
  group_by(CITY, geom) %>%
  summarise(
    VALUE_TOT  = sum(VALOR_TOTAL),
    VALUE_MEAN = sum(VALOR_TOTAL)
  )
df.map$PERCENTUAL <-  (df.map$VALUE_TOT / sum(df.map$VALUE_TOT)) * 100
df.map <- st_as_sf(df.map)

# Visualizar
df.map

# ----------------------
# [3] CONSTRUÇÃO DO MAPA
# ----------------------

# -----------------------------
# [3.1] COM LEAFLET (VALUE TOT)
# -----------------------------

# Paleta melhorada para valores financeiros
pal <- colorNumeric(
  #palette = "YlOrRd",
  palette = "Blues",
  domain = df.map$VALUE_TOT,
  na.color = "#808080"
)

# Mapa profissional
leaflet(
  df.map,
  options = leafletOptions(
    minZoom = 0,
    maxZoom = 15,
    zoomControl = T
  )
) %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap)%>%
  #addProviderTiles(providers$Esri.WorldStreetMap)%>%
  #addProviderTiles("CartoDB.Positron") %>%  # Fundo neutro
  addPolygons(
    fillColor = ~pal(VALUE_TOT),
    weight = 1,                # Borda mais fina
    color = "black",           # Borda branca para contraste
    fillOpacity = 1.0,         # Leve transparência
    smoothFactor = 0.5,        # Suavização de bordas
    label = ~paste0(
      "<b>", CITY, "</b><br>",
      "TOTAL DE VENDAS (R$): ", 
      "<b>", format(VALUE_TOT, big.mark = ".", decimal.mark = ","), "</b><br>",
      "PERCENTUAL DE VENDAS (%): ",
      "<b>", format(PERCENTUAL, decimal.mark = ",", digits = 2), "%", "</b>"
    ) %>% 
      lapply(htmltools::HTML),  # Permite formatação HTML
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#333333",
      fillOpacity = 1,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal = pal,
    values = ~VALUE_TOT,
    title = "TOTAL DE VENDAS (R$)",
    position = "bottomright",
    labFormat = labelFormat(
      prefix = "R$ ",
      big.mark = "."
    )
  )