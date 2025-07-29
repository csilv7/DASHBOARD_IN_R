# --------------------------
# [1] Configurações Iniciais
# --------------------------
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(bs4Dash)

library(dplyr)
library(tidyr)
library(tidyverse)

library(DT)

library(ggplot2)
library(plotly)
library(echarts4r)

library(htmlwidgets)

library(leaflet)
library(sf)

setwd("C:/Users/user/Documents/PROJETOS/R/DASHBOARD/SHINY DASH")

# -----------------------------------
# [2] CARREGAMENTO DAS BASES DE DADOS
# -----------------------------------

# Caminhos dos arquivos
path1 <- "DATASET/DATA_GEN_V1.csv"
path2 <- "DATASET/GEOPA.rds"

# Leitura dos arquivos
VENDAS <- read_csv(path1)
GEOPA  <- readRDS(path2)

# ------------------------
# [3] VARIÁVEIS UTILIZADAS
# ------------------------

# Relação: CATEGORIA <=> PRODUTO
categories.and.products <- list(
  "ELETRÔNICOS" = c("SMARTPHONE PREMIUM", "FONE DE OUVIDO BLUETOOTH"),
  "ROUPAS" = c("CALÇA JEANS MASCULINA", "VESTIDO FLORAL FEMININO"), 
  "ALIMENTOS" = c("CESTA DE FRUTAS ORGÂNICAS", "CAFÉ GOURMET (250G)"), 
  "LIVROS" = c("BEST-SELLER DE FICÇÃO", "LIVRO TÉCNICO/ACADÊMICO"), 
  "BRINQUEDOS" = c("KIT DE CONSTRUÇÃO ROBÓTICA", "BONECA COLECIONÁVEL")
)

# Municípios (Mapa)
citys <- c("Belém", "Ananindeua", "Marituba", "Benevides", "Santa Bárbara do Pará")

# ----------------------
# [4] FUNÇÕES UTILIZADAS
# ----------------------

# --------------------
# [5] CARREGAR MÓDULOS
# --------------------
source("MODULES/overview.R")
source("MODULES/geograph.R")