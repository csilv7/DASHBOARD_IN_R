# --------------------------
# [1] CONFIGURAÇÕES INICIAIS
# --------------------------

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(bs4Dash)

library(dplyr)
library(tidyr)
library(tidyverse)

library(ggplot2)
library(plotly)
library(echarts4r)

library(leaflet)
library(sf)

setwd("C:/Users/user/Documents/PROJETOS/R/DASHBOARD/SHINY DASH")
path <- "DATASET/DATA_GEN_V1.csv"
VENDAS.DF <- read_csv(path)

# https://www.kaggle.com/code/ekrembayar/fifa-19-dashboard-with-r-shiny/report?select=FIFA+19+Player+DB.csv

# -----------------------------------
# [2] FrontEnd - Interface do Usuário
# -----------------------------------
ui <- dashboardPage(
  title = "Dashboard de Vendas",
  
  # ---------------
  # [2.1] CABEÇALHO
  # ---------------
  Header <- dashboardHeader(
    titleWidth = 220,
    title = dashboardBrand(
      title = tags$strong("PAINEL DE VENDAS"),
      image = "LOGOSHINY.png",
      color ="gray"
    ),
    
    skin = "black"
  ),
  # -------------------
  # [2.2] BARRA LATERAL
  # -------------------
  Sidebar <- dashboardSidebar(
    width = 220,
    sidebarMenu(
      menuItem("PROJETO", tabName = "project", icon = icon("book")),
      menuItem(
        "EXPLORATÓRIA", tabName = "AED", icon = icon("chart-bar"),
        menuSubItem("VISÃO GERAL", tabName = "VisionGeral", icon = icon("dashboard")),
        menuSubItem("VISÃO GEOGRÁFICA", tabName = "VisionGeo", icon = icon("map-location-dot")),
        menuSubItem("ANÁLISE TEMPORAL", tabName = "timeAnalisys", icon = icon("chart-line"))
      ),
      menuItem("CONCLUSÕES", tabName = "conclu", icon = icon("book")),
      menuItem("RELATÓRIO PDF", tabName = "relatorio_pdf", icon = icon("file-pdf")),
      br(),
      selectInput("city_input", "CIDADE:", choices = c("TODAS", unique(VENDAS.DF$CITY))),
      selectInput("canal_input", "CANAL DE VENDA:", choices = c("TODOS", unique(VENDAS.DF$CANAL_VENDA))),
      selectInput("escolar_input", "ESCOLARIDADE ESTIMADA:", choices = c("TODOS", unique(VENDAS.DF$ESCOLARIDADE_ROTULADA))),
      dateRangeInput(
        "date_range", "SELECIONE O PERÍODO", 
        start = min(VENDAS.DF$DATA_VENDA), 
        end   = max(VENDAS.DF$DATA_VENDA)
      )
    )
  ),
  
  # -----------
  # [2.3] CORPO
  # -----------
  Body <- dashboardBody(
    tabItems(
      # ---------------------
      # [2.3.1] ABA - PROJETO
      # ---------------------
      tabItem(
        tabName = "project",
        tabBox(
          id = "tabs", width = 12, height = "600px",
          tabPanel("REFERENCIAL", icon = icon("book"), "Tab I"),
          tabPanel("MATERIAL E MÉTODOS", icon = icon("chart-bar"), "Tab II"),
          tabPanel("RECURSO COMPUTACIONAL", icon = icon("desktop"), "Tab III"),
          tabPanel("RESPONSÁVEL TÉCNICO", icon = icon("user"), "Tab IV")
        )
      ),
      # ----------------------------------
      # [2.3.2] ABA - ANÁLISE EXPLORATÓRIA
      # ----------------------------------
      
      # -------------------------------
      # [2.3.2.1] SUB ABA - VISÃO GERAL
      # -------------------------------
      tabItem(
        tabName = "VisionGeral",
        fluidRow(
          valueBoxOutput("rlsValuesQuant"),
          valueBoxOutput("rlsValuesDescont"),
          valueBoxOutput("rlsValuesTotal")
        ),
        box(
          title = strong("ANÁLISE POR CATEGORIA E PRODUTO"), width = 12,
          fluidRow(
            column(
              width = 5,
              echarts4rOutput("echart_categ_prod")
            ),
            column(
              width = 7,
              plotlyOutput("plotly_prod")
            )
          )
        ),
        br(),
        box(
          title = strong("ANÁLISE QUANTITATIVA DO VALOR DA VENDA"), width = 12,
          fluidRow(
            title = "FILTROS PARA MELHOR VISUALIZAÇÃO",
            column(
              width = 3,
              selectInput("f_sexo", "SEXO:", choices = c("TODOS", unique(VENDAS.DF$SEX)))
            ),
            column(
              width = 3,
              uiOutput("filtro_categoria")
              
            ),
            column(
              width = 3,
              uiOutput("filtro_produto")
            ),
            column(
              width = 3,
              selectInput("f_outlier", "OUTLIERS:", choices = c("TODOS", "SEM OUTLIERS", "SOMENTE OUTLIERS"))
            )
          )
          ,
          fluidRow(
            column(
              width = 6,
              plotlyOutput("plotly_hist_ValueTot")
            ),
            column(
              width = 6,
              plotlyOutput("plotly_boxplot_ValueTot")
            )
          )
        )
      ),
      # -------------------------------
      # [2.3.2.1] SUB ABA - VISÃO GEOGR
      # -------------------------------
      tabItem(
        tabName = "VisionGeo",
        box(
          title = strong("QUANTITATIVO DO VALOR DA VENDA VIA LOCALIZAÇÃO GEOGRÁFICA DO MUNICÍPIOS"), width = 12,
          fluidRow(
            title = "FILTROS",
            selectInput("map_choice", "ESTATÍSTICA AVALIADA:", choices = c("TOTAL", "MÉDIA"))
          ),
          fluidRow(
            leafletOutput("city_map")
          )
        )
      )
    )
  )
)

# -----------------------------
# [3] BackEnd - Camada Servidor
# -----------------------------
server <- function(input, output) {
  # Carregar Base de Dados
  VENDAS.DF <- read_csv("DATASET/DATA_GEN_V1.csv")
  
  categories.and.products <- list(
    "ELETRÔNICOS" = c("SMARTPHONE PREMIUM", "FONE DE OUVIDO BLUETOOTH"),
    "ROUPAS" = c("CALÇA JEANS MASCULINA", "VESTIDO FLORAL FEMININO"), 
    "ALIMENTOS" = c("CESTA DE FRUTAS ORGÂNICAS", "CAFÉ GOURMET (250G)"), 
    "LIVROS" = c("BEST-SELLER DE FICÇÃO", "LIVRO TÉCNICO/ACADÊMICO"), 
    "BRINQUEDOS" = c("KIT DE CONSTRUÇÃO ROBÓTICA", "BONECA COLECIONÁVEL")
  )
  
  output$filtro_categoria <- renderUI({
    selectInput(
      inputId = "f_categoria",
      label = "CATEGORIA:",
      choices = c("TODAS", names(categories.and.products)),
      selected = "TODAS"
    )
  })
  
  output$filtro_produto <- renderUI({
    req(input$f_categoria)  # Garante que a categoria foi escolhida
    
    if (input$f_categoria == "TODAS") {
      produtos_disponiveis <- unique(VENDAS.DF$PRODUTO)
    } else {
      produtos_disponiveis <- categories.and.products[[input$f_categoria]]
    }
    
    selectInput(
      inputId = "f_produto",
      label = "PRODUTO:",
      choices = c("TODOS", produtos_disponiveis),
      selected = "TODOS"
    )
  })
  
  # --------------
  # [3.1] FILTRO
  # --------------
  
  # FILTRO GERAL
  df.filter <- reactive({
    df <- VENDAS.DF %>% filter(DATA_VENDA >= input$date_range[1] & DATA_VENDA <= input$date_range[2])
    
    if (input$city_input != "TODAS") {
      df <- df %>% filter(CITY == input$city_input)
    }
    if (input$canal_input != "TODOS") {
      df <- df %>% filter(CANAL_VENDA == input$canal_input)
    }
    if (input$escolar_input != "TODOS") {
      df <- df %>% filter(ESCOLARIDADE_ROTULADA == input$escolar_input)
    }
    
    return(df)
  })
  
  # FILTRO PARA HISTOGRAMA E BOXPLOT
  df.filter2 <- reactive({
    df <- VENDAS.DF %>% filter(DATA_VENDA >= input$date_range[1] & DATA_VENDA <= input$date_range[2])
    
    if (input$city_input != "TODAS") {
      df <- df %>% filter(CITY == input$city_input)
    }
    if (input$canal_input != "TODOS") {
      df <- df %>% filter(CANAL_VENDA == input$canal_input)
    }
    if (input$escolar_input != "TODOS") {
      df <- df %>% filter(ESCOLARIDADE_ROTULADA == input$escolar_input)
    }
    
    if (input$f_sexo != "TODOS") {
      df <- df %>% filter(SEX == input$f_sexo)
    }
    if (input$f_categoria != "TODAS") {
      df <- df %>% filter(CATEGORIA == input$f_categoria)
    }
    if (input$f_produto != "TODOS") {
      df <- df %>% filter(PRODUTO == input$f_produto)
    }
    if (input$f_outlier == "SEM OUTLIERS") {
      q <- quantile(df$VALOR_TOTAL, probs = c(0.25, 0.75), na.rm = TRUE)
      iqr <- q[2] - q[1]
      df <- df %>% filter(
        VALOR_TOTAL >= (q[1] - 1.5 * iqr),
        VALOR_TOTAL <= (q[2] + 1.5 * iqr)
      )
    } else if (input$f_outlier == "SOMENTE OUTLIERS") {
      q <- quantile(df$VALOR_TOTAL, probs = c(0.25, 0.75), na.rm = TRUE)
      iqr <- q[2] - q[1]
      df <- df %>% filter(
        VALOR_TOTAL < (q[1] - 1.5 * iqr) | VALOR_TOTAL > (q[2] + 1.5 * iqr)
      )
    }
    
    return(df)
  })
  
  # ---------------
  # [3.2] VALUE BOX
  # ---------------
  output$rlsValuesQuant <- renderValueBox({
    valueBox(
      sum(df.filter()$QUANTIDADE), "QUANTIDADE VENDIDA", icon = icon("cart-shopping"),
      color = "lightblue"
    )
  })
  output$rlsValuesDescont <- renderValueBox({
    valueBox(
      paste0(round(mean(df.filter()$DESCONTO_APLICADO) * 100, 2), "%"), "DESCONTO MÉDIO", icon = icon("handshake-simple"),
      color = "danger"
    )
  })
  output$rlsValuesTotal <- renderValueBox({
    valueBox(
      sum(df.filter()$VALOR_TOTAL), "VALOR TOTAL", icon = icon("file-invoice-dollar"),
      color = "success"
    )
  })
  # --------------------
  # [3.3] PLOTS INICIAIS
  # --------------------
  output$echart_categ_prod <- renderEcharts4r({
    df.filter() %>%
      group_by(CATEGORIA) %>%
      summarise(FREQUENCIA = n()) %>%
      mutate(
        PERCENTUAL = FREQUENCIA / sum(FREQUENCIA)
      ) %>%
      arrange(desc(FREQUENCIA)) %>%
      e_chart(CATEGORIA) %>%
      e_pie(
        FREQUENCIA,
        radius = c("30%", "70%")
      ) %>%
      e_tooltip(
        trigger = "item", 
        formatter = htmlwidgets::JS(
          "
          function(params){
          return '<b>' + params.name + '</b><br/>' +
                 'Frequência: ' + params.value + '<br/>' +
                 'Percentual: ' + params.percent + '%';
          }
          "
        )
      )
  })
  
  output$plotly_prod <- renderPlotly({
    p.product <- df.filter() %>%
      group_by(PRODUTO) %>%
      summarise(FREQ = n()) %>%
      arrange(-FREQ) %>%
      mutate(PERCENT = (FREQ / sum(FREQ)) * 100) %>%
      ggplot(aes(y = reorder(PRODUTO, FREQ), x = FREQ)) +
      geom_bar(aes(fill = PRODUTO), stat = "identity") +
      geom_text(aes(label = paste0(round(PERCENT, 2), "%")), # Formata para "XX.XX%"
                position = position_stack(vjust = 0.5),      # Centraliza verticalmente
                hjust = 1.1,                                 # Ajusta a posição horizontal (dentro da barra)
                size = 4,
                fontface = "bold",
                color = "black") + 
      labs(x = "FREQUÊNCIA", y = "PRODUTO") +
      theme_classic(base_size = 12) +
      theme(
        legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")
      )
    
    ggplotly(p.product)
  })
  
  output$plotly_hist_ValueTot <- renderPlotly({
    p.histVT <- df.filter2() %>%
      ggplot(aes(x = VALOR_TOTAL)) + 
      geom_histogram(color = "white", fill = "darkblue", binwidth = 160) +
      labs(x = "VALOR TOTAL", y = "FREQUÊNCIA") +
      theme_classic(base_size = 12) +
      theme(
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")
      )
    
    ggplotly(p.histVT)
  })
  
  output$plotly_boxplot_ValueTot <- renderPlotly({
    p.BoxPlotVT <- df.filter2() %>%
      ggplot() + 
      geom_boxplot(aes(x = "", y = VALOR_TOTAL, group = 1), color = "darkblue") +
      labs(y = "VALOR TOTAL") +
      theme_classic(base_size = 12) +
      theme(
        axis.title.y = element_text(face = "bold")
      )
    
    ggplotly(p.BoxPlotVT)
  })
  
  # MAPA
  output$city_map <- renderLeaflet({
    # Caminho do arquivo.rds
    path <- "DATASET/GEOPA.rds"
    
    # Leitura do arquivo.rds
    GEOPA <- readRDS(path)
    
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
        ESTATISTICA = ifelse(input$map_choice == "TOTAL", sum(VALOR_TOTAL), mean(VALOR_TOTAL))
      )
    df.map$PERCENTUAL <-  (df.map$ESTATISTICA / sum(df.map$ESTATISTICA)) * 100
    df.map <- st_as_sf(df.map)
    
    # Paleta melhorada para valores financeiros
    pal <- colorNumeric(
      #palette = "YlOrRd",
      palette = "Blues",
      domain = df.map$ESTATISTICA,
      na.color = "#808080"
    )
    
    # Mapa profissional
    leaflet(
      df.map,
      options = leafletOptions(
        minZoom = 0,
        maxZoom = 15,
        zoomControl = F
      )
    ) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap)%>%
      #addProviderTiles(providers$Esri.WorldStreetMap)%>%
      #addProviderTiles("CartoDB.Positron") %>%  # Fundo neutro
      addPolygons(
        fillColor = ~pal(ESTATISTICA),
        weight = 1,                # Borda mais fina
        color = "black",           # Borda branca para contraste
        fillOpacity = 1.0,         # Leve transparência
        smoothFactor = 0.5,        # Suavização de bordas
        label = ~paste0(
          "<b>", CITY, "</b><br>",
          ifelse(input$map_choice == "TOTAL", "TOTAL DE VENDAS (R$)", "MÉDIA DE VENDAS (R$)"), 
          "<b>", format(ESTATISTICA, big.mark = ".", decimal.mark = ","), "</b><br>",
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
        values = ~ESTATISTICA,
        title = ifelse(input$map_choice == "TOTAL", "TOTAL DE VENDAS (R$)", "MÉDIA DE VENDAS (R$)"),
        position = "bottomright",
        labFormat = labelFormat(
          prefix = "R$ ",
          big.mark = "."
        )
      )
  })
  
}

# ----------------------------
# [4] Produto Final: Dashboard
# ----------------------------
shinyApp(ui, server)