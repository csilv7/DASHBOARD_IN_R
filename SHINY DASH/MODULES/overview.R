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
    # [1] RenderUI para o filtro de produto (depende da categoria)
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
    
    # -------------------------------
    # [2.1] FILTROS PRIMÁRIOS (GERAL)
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
    # [2.2] FILTROS SECUNDÁRIOS (SOMENTE PARA O QUANTITATIVO)
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
    
    # --------------
    # [3] ValueBox's
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
    
    # --------------
    # [4] HISTOGRAMA
    # --------------
    output$echart_hist_ValueTot <- renderEcharts4r({
      df.secondary_filter() %>%
        e_charts() %>%
        e_histogram(VALOR_TOTAL,
                    #breaks = 160,
                    name = "Frequência",
                    itemStyle = list(color = "lightblue", borderColor = "white")
        ) %>%
        e_x_axis(name = "VALOR TOTAL",
                 nameLocation = "middle",
                 nameGap = 30,
                 nameTextStyle = list(fontWeight = "bold", fontSize = 12, color = "#333"),
                 axisLine = list(show = TRUE, lineStyle = list(color = "#333")),
                 splitLine = list(show = FALSE),
                 # Formatação dos rótulos do eixo X
                 axisLabel = list(formatter = htmlwidgets::JS("function(value) { return new Intl.NumberFormat('pt-BR').format(value); }"))
        ) %>%
        e_y_axis(name = "FREQUÊNCIA",
                 nameLocation = "middle",
                 nameGap = 40,
                 nameTextStyle = list(fontWeight = "bold", fontSize = 12, color = "#333"),
                 axisLine = list(show = TRUE, lineStyle = list(color = "#333")),
                 splitLine = list(show = FALSE),
                 # Formatação dos rótulos do eixo Y
                 axisLabel = list(formatter = htmlwidgets::JS("function(value) { return new Intl.NumberFormat('pt-BR').format(value); }"))
        ) %>%
        e_tooltip(trigger = "axis",
                  # Formatação do tooltip
                  formatter = htmlwidgets::JS("
                function(params) {
                  var res = params[0].name + '<br/>';
                  for (var i = 0; i < params.length; i++) {
                    res += params[i].marker + params[i].seriesName + ': ' + new Intl.NumberFormat('pt-BR').format(params[i].value[1]) + '<br/>';
                  }
                  return res;
                }
              ")) %>%
        e_title("Histograma de Valor Total", "Distribuição dos Valores de Venda",
                textStyle = list(fontSize = 18, color = "#333"),
                subtextStyle = list(fontSize = 12, color = "#666")
        ) %>%
        e_legend(show = FALSE) %>%
        e_grid(left = '10%', right = '5%', bottom = '15%', top = '20%') %>%
        e_theme("light")
    })
    
    # -----------
    # [4] BOXPLOT
    # -----------
    output$echart_boxplot_ValueTot <- renderEcharts4r({
      df.secondary_filter() %>%
        e_charts() %>%
        e_boxplot(VALOR_TOTAL,
                  name = "Valor Total",
                  itemStyle = list(color = "lightblue", borderColor = "darkblue", borderWidth = 1),
                  outlierStyle = list(color = "cornflowerblue", symbolSize = 6)
        ) %>%
        e_y_axis(name = "VALOR TOTAL",
                 nameLocation = "middle",
                 nameGap = 50,
                 nameTextStyle = list(fontWeight = "bold", fontSize = 12, color = "#333"),
                 axisLine = list(show = TRUE, lineStyle = list(color = "#333")),
                 splitLine = list(show = FALSE),
                 axisLabel = list(formatter = htmlwidgets::JS("function(value) { return new Intl.NumberFormat('pt-BR').format(value); }"))
        ) %>%
        e_x_axis(name = "",
                 show = TRUE,
                 axisLabel = list(show = FALSE),
                 axisLine = list(show = TRUE, lineStyle = list(color = "#333")),
                 splitLine = list(show = FALSE),
                 axisTick = list(show = FALSE)
        ) %>%
        e_tooltip(trigger = "item",
                  # Formatação do tooltip APERFEIÇOADA
                  formatter = htmlwidgets::JS("
                function(params) {
                  var formatter = new Intl.NumberFormat('pt-BR');
                  // Verifica se é o box principal (array com 6 elementos: [categoria, min, Q1, med, Q3, max])
                  if (params.value && params.value.length === 6) {
                    var res = params.seriesName + '<br/>';
                    res += 'Mínimo: ' + formatter.format(params.value[1]) + '<br/>';
                    res += 'Q1: ' + formatter.format(params.value[2]) + '<br/>';
                    res += 'Mediana: ' + formatter.format(params.value[3]) + '<br/>';
                    res += 'Q3: ' + formatter.format(params.value[4]) + '<br/>';
                    res += 'Máximo: ' + formatter.format(params.value[5]) + '<br/>';
                    return res;
                  }
                  // Se for um outlier (array com 2 elementos: [categoria, valor_outlier])
                  else if (params.value && params.value.length === 2) {
                    return params.marker + 'Valor: ' + formatter.format(params.value[1]);
                  }
                  return ''; // Retorna vazio se não for nem box nem outlier reconhecido
                }
              ")) %>%
        e_title("BoxPlot de Valor Total", "Variação dos Valores de Venda",
                textStyle = list(fontSize = 18, color = "#333"),
                subtextStyle = list(fontSize = 12, color = "#666")
        ) %>%
        e_grid(left = '10%', right = '5%', bottom = '15%', top = '20%') %>%
        e_theme("light")
    })
    
    # ----------------------
    # [5] GRÁFICO DE SETORES
    # ----------------------
    output$echart_categ_prod <- renderEcharts4r({
      df.general_filter() %>%
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
    
    # ---------------------
    # [5] GRÁFICO DE BARRAS
    # ---------------------
    output$plotly_prod <- renderPlotly({
      p.product <- df.general_filter() %>%
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