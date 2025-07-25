# --------------------------
# [1] Configurações Iniciais
# --------------------------
source("~/PROJETOS/R/DASHBOARD/SHINY DASH/global.R")

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
        menuSubItem("VISÃO GERAL", tabName = "overview", icon = icon("dashboard")),
        menuSubItem("VISÃO GEOGRÁFICA", tabName = "geo_vision", icon = icon("map-location-dot")),
        menuSubItem("VISÃO TEMPORAL", tabName = "temp_vision", icon = icon("chart-line"))
      ),
      menuItem("CONCLUSÕES", tabName = "conclu", icon = icon("book")),
      menuItem("RELATÓRIO PDF", tabName = "relatorio_pdf", icon = icon("file-pdf"))
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
      # ---------------------------------
      # [2.3.2] ABA: ANÁLISE EXPLORATÓRIA
      # ---------------------------------
      # ------------------------------
      # [2.3.2.1] SUB-ABA: VISÃO GERAL
      # ------------------------------
      tabItem(
        tabName = "overview",
        # ----------------------
        # [*] Filtros & ValueBox
        # ----------------------
        box(
          title = strong("FILTROS E CAIXA DE VALORES"), width = 12,
          fluidRow(
            column(
              width = 4, selectInput("city_input", "CIDADE:", choices = c("TODAS", unique(VENDAS$CITY)))
            ),
            column(
              width = 4, selectInput("canal_input", "CANAL DE VENDA:", choices = c("TODOS", unique(VENDAS$CANAL_VENDA)))
            ),
            column(
              width = 4,
              dateRangeInput(
                "date_range", "SELECIONE O PERÍODO:", 
                start = min(VENDAS$DATA_VENDA), 
                end   = max(VENDAS$DATA_VENDA)
              )
            )
          ),
          br(),
          fluidRow(
            bs4InfoBoxOutput("rlsValuesQuant"),
            bs4InfoBoxOutput("rlsValuesDescont"),
            bs4InfoBoxOutput("rlsValuesTotal")
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
              selectInput("filter_sex", "SEXO:", choices = c("TODOS", unique(VENDAS$SEX)))
            ),
            column(
              width = 3, 
              selectInput("filter_cat", "CATEGORIA:", choices = c("TODAS", names(categories.and.products)))
            ),
            column(
              width = 3,
              uiOutput("filter_prod")
            ),
            column(
              width = 3,
              selectInput("filter_outlier", "OUTLIERS:", choices = c("TODOS", "SEM OUTLIERS", "SOMENTE OUTLIERS"))
            )
          )
          ,
          fluidRow(
            column(
              width = 6,
              echarts4rOutput("echart_hist_ValueTot")
            ),
            column(
              width = 6,
              echarts4rOutput("echart_boxplot_ValueTot")
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
              echarts4rOutput("echart_categ_prod")
            ),
            column(
              width = 7,
              plotlyOutput("plotly_prod")
            )
          )
        )
        
      ),
      # ------------------------------
      # [2.3.2.2] SUB-ABA: VISÃO GEOGR
      # ------------------------------
      tabItem(
        tabName = "geo_vision",
        box(
          title = strong("QUANTITATIVO DO VALOR DA VENDA VIA LOCALIZAÇÃO GEOGRÁFICA DO MUNICÍPIOS"), width = 12,
          fluidRow(
            title = "FILTROS",
            selectInput("map_choice", "ESTATÍSTICA AVALIADA:", choices = c("TOTAL", "MÉDIA"))
          ),
          fluidRow(
            column(
              width = 5, leafletOutput("city_map")
            ),
            column(
              width = 7, DTOutput("city_table")
            )
          )
        )
      ),
      # ---------------------------------
      # [2.3.2.3] SUB-ABA: VISÃO TEMPORAL
      # ---------------------------------
      tabItem(
        tabName = "temp_vision"
      )
      # ----------------------
      # [2.3.3] ABA: CONCLUSÃO
      # ----------------------
      
      # --------------------------
      # [2.3.4] ABA: RELATÓRIO PDF
      # --------------------------
    )
  )
)
# -----------------------------
# [3] BackEnd - Camada Servidor
# -----------------------------
server <- function(input, output) {
  # ---------------------------
  # Botão de Filtro <=> PRODUTO
  # ---------------------------
  output$filter_prod <- renderUI({
    req(input$filter_cat)  # Garante que a categoria foi escolhida
    
    if (input$filter_cat == "TODAS") {
      available_prod <- unique(VENDAS$PRODUTO)
    } else {
      available_prod <- categories.and.products[[input$filter_cat]]
    }
    
    selectInput("filter_prod", "PRODUTO:", choices = c("TODOS", available_prod))
  })
  
  # -----------------------------------
  # FILTRO NO CONJUNTO DE DADOS (QUALI)
  # -----------------------------------
  df.filter <- reactive({
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
  
  # -----------------------------------
  # FILTRO NO CONJUNTO DE DADOS (QUANT)
  # -----------------------------------
  df.filter2 <- reactive({
    # Filtro Quant
    df <- df.filter()
    
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
  
  # ----------
  # ValueBox's
  # ----------
  output$rlsValuesQuant <- renderbs4ValueBox({
    bs4InfoBox(
      title = strong("QUANTIDADE VENDIDA:"),
      subtitle = "Nº Total de Produtos Comercializados",
      value = paste0(format(sum(df.filter()$QUANTIDADE), big.mark = "."), " Unidades"),
      icon = icon("cart-shopping"),
      color = "lightblue",
      fill = T
    )
  })
  output$rlsValuesDescont <- renderbs4ValueBox({
    # Média
    x.bar <- mean(df.filter()$DESCONTO_APLICADO) * 100
    
    bs4InfoBox(
      title = strong("DESCONTO MÉDIO:"),
      subtitle = "Oferecido ao Cliente", # Manter ou considerar "Percentual Médio Aplicado"
      value = paste0(
        format(x.bar, big.mark = ".", decimal.mark = ",", digits = 2), "%"
      ),
      icon = icon("handshake-simple"),
      color = "danger",
      fill = T
    )
  })
  output$rlsValuesTotal <- renderbs4ValueBox({
    # Valor Total
    x.sum <- sum(df.filter()$VALOR_TOTAL)
    
    bs4InfoBox(
      title = strong("VALOR TOTAL:"),
      subtitle = "Valor de Venda Final",
      value = paste0("R$ ", format(x.sum, big.mark = ".", decimal.mark = ",", decimal.places = 2)), # Adicionado "R$"
      icon = icon("file-invoice-dollar"),
      color = "success",
      fill = T
    )
  })
  
  # ------------
  # PLOT - QUANT
  # ------------
  output$echart_hist_ValueTot <- renderEcharts4r({
    df.filter2() %>%
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
  
  output$echart_boxplot_ValueTot <- renderEcharts4r({
    df.filter2() %>%
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
  
  # ------------
  # PLOT - QUALI
  # ------------
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
  
  # ---------
  # CITY MAPA
  # ---------
  output$city_map <- renderLeaflet({
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
    df.map <- VENDAS %>%
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
        fillOpacity = 1,         # Leve transparência
        smoothFactor = 0.5,        # Suavização de bordas
        label = ~paste0(
          "<b>", CITY, "</b><br>",
          ifelse(input$map_choice == "TOTAL", "TOTAL DE VENDAS (R$): ", "MÉDIA DE VENDAS (R$): "), 
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
  
  # ----------
  # CITY TABLE
  # ----------
  output$city_table <- renderDT({
    VENDAS %>%
      group_by(CITY) %>%
      summarise(
        FREQ = n(),
        ESTATISTICA = ifelse(input$map_choice == "TOTAL", sum(VALOR_TOTAL), mean(VALOR_TOTAL))
      ) %>%
      mutate(
        PERCENT = FREQ / sum(FREQ),
        PERCENT_EST = ESTATISTICA / sum(ESTATISTICA)
      ) %>%
      select(CITY, FREQ, PERCENT, ESTATISTICA, PERCENT_EST) %>%
      datatable(
        colnames = c(
          "MUNICÍPIO", "FREQUÊNCIA", "PERCENTUAL (%)",
          ifelse(input$map_choice == "TOTAL", "TOTAL DE VENDAS (R$)", "MÉDIA DE VENDAS (R$)"),
          "PERCENTUAL DE VENDAS (%)"
        ),
        extensions = c("Buttons", "Scroller"),
        options = list(
          deferRender = TRUE,
          scrollY = 200,
          scroller = TRUE,
          dom = "Bfrtip",
          buttons = c("csv", "excel", "pdf"),
          info = FALSE,
          language = list(url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json")
        )
      ) %>%
      formatRound("FREQ", digits = 0, mark = ".") %>%
      formatRound("ESTATISTICA", digits = 2, mark = ".", dec.mark = ",") %>%
      formatPercentage(c("PERCENT", "PERCENT_EST"), digits = 2, dec.mark = ",")
  })
  
}

# ----------------------------
# [4] Produto Final: Dashboard
# ----------------------------
shinyApp(ui, server)