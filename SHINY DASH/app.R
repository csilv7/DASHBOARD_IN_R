# --------------------------
# [1] Configurações Iniciais
# --------------------------
setwd("~/PROJETOS/R/DASHBOARD/SHINY DASH")
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
  # -------------------------------
  # [3.1] FILTROS PRIMÁRIOS (GERAL)
  # -------------------------------
  df.general_filter <- reactive({
    # Filtro Período
    df <- VENDAS %>% filter(DATA_VENDA >= input$date_range[1] & DATA_VENDA <= input$date_range[2])
    
    # Filtro Município
    if (input$city_input != "TODAS") {
      df <- df %>% filter(CITY == input$city_input)
    }
    
    # Filtro Canal de Venda
    if (input$canal_input != "TODOS") {
      df <- df %>% filter(CANAL_VENDA == input$canal_input)
    }
    
    # Retornar Data Frame Filtrado
    return(df)
  })
  
  # -------------------------------------------------------
  # [3.2] FILTROS SECUNDÁRIOS (SOMENTE PARA O QUANTITATIVO)
  # -------------------------------------------------------
  df.secondary_filter <- reactive({
    # Filtros Primários
    df <- df.general_filter()
    
    # Filtro Sexo
    if (input$filter_sex != "TODOS") {
      df <- df %>% filter(SEX == input$filter_sex)
    }
    
    # Filtro Categoria
    if (input$filter_cat != "TODAS") {
      df <- df %>% filter(CATEGORIA == input$filter_cat)
    }
    
    # Filtro Outlier
    if (input$filter_prod != "TODOS") {
      df <- df %>% filter(PRODUTO == input$filter_prod)
    }
    if (input$filter_outlier == "SEM OUTLIERS") {
      q <- quantile(df$VALOR_TOTAL, probs = c(0.25, 0.75), na.rm = TRUE)
      iqr <- q[2] - q[1]
      df <- df %>% filter(
        VALOR_TOTAL >= (q[1] - 1.5 * iqr),
        VALOR_TOTAL <= (q[2] + 1.5 * iqr)
      )
    } else if (input$filter_outlier == "SOMENTE OUTLIERS") {
      q <- quantile(df$VALOR_TOTAL, probs = c(0.25, 0.75), na.rm = TRUE)
      iqr <- q[2] - q[1]
      df <- df %>% filter(
        VALOR_TOTAL < (q[1] - 1.5 * iqr) | VALOR_TOTAL > (q[2] + 1.5 * iqr)
      )
    }
    
    # Retornar Data Frame Filtrado
    return(df)
  })
  
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