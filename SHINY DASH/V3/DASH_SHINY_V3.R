# -------------------------
# [1] CONFIGURAÇÕES INICIAIS
# -------------------------

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(bs4Dash)

library(dplyr)
library(ggplot2)
library(plotly)

path <- "/cloud/project/CONJUNTO DE DADOS/DATA_GEN_V1.csv"
VENDAS.DF <- read.csv(path)

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
      tabItem(
        tabName = "VisionGeral",
        fluidRow(
          valueBoxOutput("rlsValuesQuant"),
          valueBoxOutput("rlsValuesDescont"),
          valueBoxOutput("rlsValuesTotal")
        ),
        fluidRow(
          column(
            width = 5,
            plotlyOutput("plotly_categ_prod")
          ),
          column(
            width = 7,
            plotlyOutput("plotly_prod")
          )
        ),
      )
    )
  )
)

# -----------------------------
# [3] BackEnd - Camada Servidor
# -----------------------------
server <- function(input, output) {
  # Carregar Base de Dados
  VENDAS.DF <- read.csv("/cloud/project/CONJUNTO DE DADOS/DATA_GEN_V1.csv")
  
  # --------------
  # [3.1] FILTRO
  # --------------
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
  output$plotly_categ_prod <- renderPlotly({
    p.category <- df.filter() %>%
      group_by(CATEGORIA) %>%
      summarise(FREQ = n()) %>%
      arrange(-FREQ) %>%
      mutate(PERCENT = (FREQ / sum(FREQ)) * 100) %>%
      ggplot(aes(y = reorder(CATEGORIA, FREQ), x = FREQ)) +
      geom_bar(aes(fill = CATEGORIA), stat = "identity") +
      geom_text(aes(label = paste0(round(PERCENT, 2), "%")), # Formata para "XX.XX%"
                position = position_stack(vjust = 0.5),      # Centraliza verticalmente
                hjust = 1.1,                                 # Ajusta a posição horizontal (dentro da barra)
                size = 4,
                fontface = "bold",
                color = "black") + 
      labs(x = "FREQUÊNCIA", y = "CATEGORIA") +
      theme_classic(base_size = 12) +
      theme(
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")
      )
    
    ggplotly(p.category)
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
  
}

# ----------------------------
# [4] Produto Final: Dashboard
# ----------------------------
shinyApp(ui, server)