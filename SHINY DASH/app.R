# --------------------------
# [1] Configurações Iniciais
# --------------------------
#setwd("~/PROJETOS/R/DASHBOARD/SHINY DASH")
source("global.R")

# -----------------------------------
# [2] FrontEnd - Interface do Usuário
# -----------------------------------
ui <- dashboardPage(
  title = "Dashboard de Vendas",
  
  # ---------------
  # [2.1] Cabeçalho
  # ---------------
  header = dashboardHeader(
    titleWidth = 220,
    title = dashboardBrand(
      title = tags$strong("PAINEL DE VENDAS"),
      image = "LOGOSHINY.png",
      color ="gray"
    ),
    skin = "black"
  ),
  
  # -------------------
  # [2.2] Barra Lateral
  # -------------------
  sidebar = dashboardSidebar(
    width = 220,
    sidebarMenu(
      menuItem("PROJETO", tabName = "project", icon = icon("book")),
      menuItem(
        "EXPLORATÓRIA", tabName = "AED", icon = icon("chart-bar"),
        menuSubItem("VISÃO GERAL", tabName = "overview", icon = icon("dashboard")),
        menuSubItem("VISÃO GEOGRÁFICA", tabName = "geograph", icon = icon("map-location-dot")),
        menuSubItem("VISÃO TEMPORAL", tabName = "temp_vision", icon = icon("chart-line"))
      ),
      menuItem("CONCLUSÕES", tabName = "conclu", icon = icon("book")),
      menuItem("RELATÓRIO PDF", tabName = "report_pdf", icon = icon("file-pdf"))
    )
  ), 
  
  # -------------------
  # [2.3] Corpo do Dash
  # -------------------
  body = dashboardBody(
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
      # ---------------------------------
      # [2.3.2] ABA: ANÁLISE EXPLORATÓRIA
      # ---------------------------------
      
      # ------------------------------
      # [2.3.2.1] SUB-ABA: VISÃO GERAL
      # ------------------------------
      mod_overview_ui("overview_module"),
      # ------------------------------
      # [2.3.2.2] SUB-ABA: VISÃO GEOGR
      # ------------------------------
      mod_geograph_ui("geograph_module")
    )
  ),
  
  # ------------
  # [2.4] Rodapé
  # ------------
  footer = dashboardFooter(
    left = strong(paste0("Atualizado em ",  format(today(), format = "%d/%m/%Y"))),
    right = strong("Breno C R Silva")
  )
)

# -----------------------------
# [3] BackEnd - Camada Servidor
# -----------------------------
server <- function(input, output) {
  # --------------------------
  # [3.3] SUB-ABA: VISÃO GERAL
  # --------------------------
  mod_overview_server("overview_module")
  # --------------------------
  # [3.4] SUB-ABA: VISÃO GEOGR
  # --------------------------
  mod_geograph_server("geograph_module")
}

# ----------------------------
# [4] Produto Final: Dashboard
# ----------------------------
shinyApp(ui, server)