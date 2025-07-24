# --------------------
# [1] Visão Geral - UI
# --------------------
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(bs4Dash)

overview_UI <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "overview",
    # ----------------
    # [1.1] ValueBox's
    # ----------------
    fluidRow(
      valueBoxOutput(ns("rlsValuesQuant")),
      valueBoxOutput(ns("rlsValuesDescont")),
      valueBoxOutput(ns("rlsValuesTotal"))
    ),
    # -----------
    # [1.2] QUALI
    # -----------
    box(
      title = strong("ANÁLISE POR CATEGORIA E PRODUTO"), width = 12,
      fluidRow(
        title = "FILTROS",
        selectInput(ns("city_input"), "CIDADE:", choices = c("TODAS", unique(VENDAS$CITY))),
        selectInput(ns("canal_input"), "CANAL DE VENDA:", choices = c("TODOS", unique(VENDAS$CANAL_VENDA))),
        dateRangeInput(
          ns("date_range"), "SELECIONE O PERÍODO", 
          start = min(VENDAS$DATA_VENDA), 
          end   = max(VENDAS$DATA_VENDA)
        )
      ),
      fluidRow(
        column(
          width = 5, echarts4rOutput(ns("echart_categ_prod"))
        ),
        column(
          width = 7, plotlyOutput(ns("plotly_prod"))
        )
      )
    ),
    # ------------
    # [1.2] QUANTI
    # ------------
    box(
      title = strong("ANÁLISE QUANTITATIVA DO VALOR DA VENDA"), width = 12,
      fluidRow(
        title = "FILTROS COMPLEMENTARES",
        column(
          width = 3, selectInput(ns("filter_sex"), "SEXO:", choices = c("TODOS", unique(VENDAS.DF$SEX)))
        ),
        column(
          width = 3, selectInput(ns("filter_cat"), "CATEGORIA:", choices = c("TODAS", names(categories.and.products)))
        ),
        column(
          width = 3, uiOutput(ns("filter_prod"))
        ),
        column(
          width = 3, selectInput(ns("filter_outlier"), "OUTLIERS:", choices = c("TODOS", "SEM OUTLIERS", "SOMENTE OUTLIERS"))
        )
      ),
      fluidRow(
        column(
          width = 6, plotlyOutput(ns("plotly_hist_ValueTot"))
        ),
        column(
          width = 6, plotlyOutput(ns("plotly_boxplot_ValueTot"))
        )
      )
    )
  )
}

# ------------------------
# [2] Visão Geral - SERVER
# ------------------------
overview_SERVER <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # -------------------------
    # [2.1] Filtro para PRODUTO
    # -------------------------
    output$filter_prod <- renderUI({
      # Garante que a categoria foi escolhida
      req(input$filter_cat)
      
      # Filtro Condicional
      if (input$filter_cat == "TODAS") {
        available_prod <- unique(VENDAS$PRODUTO)
      } else {
        available_prod <- categories.and.products[[input$filter_cat]]
      }
      
      # Botão de Seleção (Filtro)
      selectInput(
        inputId = "filter_prod",
        label = "PRODUTO:",
        choices = c("TODOS", available_prod)
      )
    })
    
    # ----------------------
    # [2.3] Plot - CATEGORIA
    # ----------------------
    output$echart_categ_prod <- renderEcharts4r({
      data() %>%
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
    
    # --------------------
    # [2.4] Plot - PRODUTO
    # --------------------
    output$plotly_prod <- renderPlotly({
      p.product <- data() %>%
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
    
  })
}


overview_SERVER_2 <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # ----------------
    # [2.2] ValueBox's
    # ----------------
    output$rlsValuesQuant <- renderValueBox({
      valueBox(
        sum(data()$QUANTIDADE), "QUANTIDADE VENDIDA", icon = icon("cart-shopping"),
        color = "lightblue"
      )
    })
    output$rlsValuesDescont <- renderValueBox({
      valueBox(
        paste0(round(mean(data()$DESCONTO_APLICADO) * 100, 2), "%"), "DESCONTO MÉDIO", icon = icon("handshake-simple"),
        color = "danger"
      )
    })
    output$rlsValuesTotal <- renderValueBox({
      valueBox(
        sum(data()$VALOR_TOTAL), "VALOR TOTAL", icon = icon("file-invoice-dollar"),
        color = "success"
      )
    })
    
    # -----------------------
    # [2.5] Plot - HISTOGRAMA
    # -----------------------
    output$plotly_hist_ValueTot <- renderPlotly({
      p.histVT <- data() %>%
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
    
    # -----------------------
    # [2.5] Plot - BOXPLOT
    # -----------------------
    output$plotly_boxplot_ValueTot <- renderPlotly({
      p.BoxPlotVT <- data() %>%
        ggplot() + 
        geom_boxplot(aes(x = "", y = VALOR_TOTAL, group = 1), color = "darkblue") +
        labs(y = "VALOR TOTAL") +
        theme_classic(base_size = 12) +
        theme(
          axis.title.y = element_text(face = "bold")
        )
      
      ggplotly(p.BoxPlotVT)
    })
    
  })
}