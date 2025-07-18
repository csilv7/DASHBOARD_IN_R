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
      title = tags$strong("Painel de Vendas"),
      #href = "https://www.detran.pa.gov.br/index_.php",
      image = "LOGOSHINY.png",
      #width = 230, heigth = 100,
      color ="gray"
    ),
    
    skin = "black",
    scrollToTop = TRUE,
    fullscreen = TRUE,
    help = TRUE,
    options = list(sidebarExpandOnHover = TRUE)
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
        menuSubItem("VISÃO GERAL", tabName = "VisionGeral", icon = icon("dashboard"))
      ),
      menuItem("CONCLUSÕES", tabName = "conclu", icon = icon("book")),
      menuItem("RELATÓRIO PDF", tabName = "relatorio_pdf", icon = icon("file-pdf")),
      br(),
      selectInput("city_input", "CIDADE:", choices = c("TODAS", unique(VENDAS.DF$CITY))),
      selectInput("sex_input", "SEXO:", choices = c("TODOS", unique(VENDAS.DF$SEX))),
      selectInput("categorical_input", "CATEGORIA DO PRODUTO:", choices = c("TODAS", unique(VENDAS.DF$CATEGORIA)))
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
          tabPanel(
            "Referencial", icon = icon("book"),
            div(
              style = "text-align: justify; text-justify: inter-word;",
              HTML(
                "
                <h3>Conteúdo do Referencial</h3>
                <p>O referencial teórico aborda as bases que fundamentam o desenvolvimento do projeto.</p>
                <ul>
                  <li><strong>Ponto 1: Descrição inicial do projeto.</li>
                  <li><strong>Ponto 2: Contexto teórico e objetivo principal.</li>
                  <li><strong>Ponto 3: Revisão de literatura relevante.</li>
                </ul>
                "
              )
            )
          ),
          tabPanel(
            "Material e Métodos", icon = icon("chart-bar"),
            div(
              style = "text-align: justify; text-justify: inter-word;",
              HTML(
                "
                <h3>Material e Métodos</h3>
                <p>Nesta seção, são apresentados os dados simulados e a metodologia aplicada.</p>
                "
              )
            )
          ),
          tabPanel(
            "Recurso Computacional", icon = icon("desktop"),
            fluidRow(
              column(
                6, div(
                  style = "text-align: justify; text-justify: inter-word;",
                  HTML(
                    "
                    <h3>Software</h3>
                    <p>Painel criado com R-Project (Versão 4.4.1).</p>
                    "
                  )
                ),
                tags$img(src = "https://www.r-project.org/Rlogo.png", alt = "Software R", height = "100px")
              ),
              column(
                6, div(
                  style = "text-align: justify; text-justify: inter-word;",
                  HTML(
                    "
                    <h3>IDE</h3>
                    <p>Ambiente utilizado: RStudio (Versão 1.4.1.7).</p>
                    "
                  )
                ),
                tags$img(src = "RStudioLogo.png", alt = "RStudio", height = "100px")
              )
            )
          ),
          tabPanel(
            "Créditos", icon = icon("phone"),
            div(
              style = "text-align: justify; text-justify: inter-word;",
              HTML(
                "
                <h3>Créditos</h3>
                <ul>
                  <li><strong>Desenvolvedor: Breno C R Silva.</li>
                  <li><strong>Contato: breno.silva@icen.ufpa.br.</li>
                </ul>
                "
              )
            )
          )
        )
      ),
      # ---------------------
      # [2.3.2] ABA - EXPLORATÓRIA
      # ---------------------
      tabItem(
        tabName = "VisionGeral",
        fluidRow(
          box(
            title = "FILTROS", width = 12, collapsible = TRUE,
            dateRangeInput(
              "date_range", "SELECIONE O PERÍODO", 
              start = min(VENDAS.DF$DATA_VENDA), 
              end = max(VENDAS.DF$DATA_VENDA)
            )
          )
        )
      )
    )
  )
)

# -----------------------------
# [3] BackEnd - Camada Servidor
# -----------------------------
server <- function(input, output) { }

# ----------------------------
# [4] Produto Final: Dashboard
# ----------------------------
shinyApp(ui, server)