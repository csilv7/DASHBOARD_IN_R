# modules/mod_overview_ui.R
mod_overview_ui <- function(id) {
  # Cria um namespace para este módulo
  ns <- NS(id)
  
  tabItem(
    tabName = "overview", # O tabName deve ser o mesmo que você define no menuItem
    # ----------------------
    # [*] Filtros & ValueBox
    # ----------------------
    box(
      title = strong("FILTROS E CAIXA DE VALORES"), width = 12,
      fluidRow(
        column(
          width = 4,
          # Usar ns() para prefixar os IDs de input/output deste módulo
          selectInput(ns("city_input"), "CIDADE:", choices = c("TODAS", unique(VENDAS$CITY)))
        ),
        column(
          width = 4,
          selectInput(ns("canal_input"), "CANAL DE VENDA:", choices = c("TODOS", unique(VENDAS$CANAL_VENDA)))
        ),
        column(
          width = 4,
          dateRangeInput(
            ns("date_range"), "SELECIONE O PERÍODO:",
            start = min(VENDAS$DATA_VENDA),
            end   = max(VENDAS$DATA_VENDA)
          )
        )
      ),
      br(),
      fluidRow(
        bs4InfoBoxOutput(ns("rlsValuesQuant")),
        bs4InfoBoxOutput(ns("rlsValuesDescont")),
        bs4InfoBoxOutput(ns("rlsValuesTotal"))
      )
    ),
    br(),
    # ---------
    # [*] Quant
    # ---------
    box(
      title = strong("ANÁLISE QUANTITATIVA DO VALOR DA VENDA"), width = 12,
      fluidRow(
        title = "FILTROS PARA MELHOR VISUALIZAÇÃO",
        column(
          width = 3,
          selectInput(ns("filter_sex"), "SEXO:", choices = c("TODOS", unique(VENDAS$SEX)))
        ),
        column(
          width = 3,
          selectInput(ns("filter_cat"), "CATEGORIA:", choices = c("TODAS", names(categories.and.products)))
        ),
        column(
          width = 3,
          uiOutput(ns("filter_prod"))
        ),
        column(
          width = 3,
          selectInput(ns("filter_outlier"), "OUTLIERS:", choices = c("TODOS", "SEM OUTLIERS", "SOMENTE OUTLIERS"))
        )
      ),
      fluidRow(
        column(
          width = 6,
          echarts4rOutput(ns("echart_hist_ValueTot"))
        ),
        column(
          width = 6,
          echarts4rOutput(ns("echart_boxplot_ValueTot"))
        )
      )
    ),
    br(),
    # ---------
    # [*] Quali
    # ---------
    box(
      title = strong("ANÁLISE POR CATEGORIA E PRODUTO"), width = 12,
      fluidRow(
        column(
          width = 5,
          echarts4rOutput(ns("echart_categ_prod"))
        ),
        column(
          width = 7,
          plotlyOutput(ns("plotly_prod"))
        )
      )
    )
  )
}

# modules/mod_overview_server.R
mod_overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # ------------------------------------------------------------
    # [*] RenderUI para o filtro de produto (depende da categoria)
    # ------------------------------------------------------------
    output$filter_prod <- renderUI({
      # Garante que a categoria foi escolhida
      req(input$filter_cat) 
      
      if (input$filter_cat == "TODAS") {
        available_prod <- unique(VENDAS$PRODUTO)
      } else {
        available_prod <- categories.and.products[[input$filter_cat]]
      }
      
      selectInput(
        inputId = session$ns("filter_prod"),
        label = "PRODUTO:",
        choices = c("TODOS", available_prod),
        selected = "TODOS"
      )
    })
    
    # --------------
    # [*] ValueBox's
    # --------------
    output$rlsValuesQuant <- renderbs4ValueBox({
      # Total de Produtos Vendidos
      x <- sum(df.general_filter()$QUANTIDADE)
      
      bs4InfoBox(
        title = strong("QUANTIDADE VENDIDA:"),
        subtitle = "Nº Total de Produtos Comercializados",
        value = paste0(format(x, big.mark = "."), " Unidades"),
        icon = icon("cart-shopping"),
        color = "lightblue",
        fill = T
      )
    })
    output$rlsValuesDescont <- renderbs4ValueBox({
      # Média
      x <- mean(df.general_filter()$DESCONTO_APLICADO) * 100
      
      bs4InfoBox(
        title = strong("DESCONTO MÉDIO:"),
        subtitle = "Oferecido ao Cliente",
        value = paste0(
          format(x, big.mark = ".", decimal.mark = ",", digits = 2), "%"
        ),
        icon = icon("handshake-simple"),
        color = "danger",
        fill = T
      )
    })
    output$rlsValuesTotal <- renderbs4ValueBox({
      # Valor Total
      x <- sum(df.general_filter()$VALOR_TOTAL)
      
      bs4InfoBox(
        title = strong("VALOR TOTAL:"),
        subtitle = "Valor de Venda Final",
        value = paste0(
          "R$ ", format(x, big.mark = ".", decimal.mark = ",", decimal.places = 2)
        ), # Adicionado "R$"
        icon = icon("file-invoice-dollar"),
        color = "success",
        fill = T
      )
    })
  
  })
}