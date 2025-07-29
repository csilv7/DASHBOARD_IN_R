# modules/mod_visionGeo_ui.R
mod_geograph_ui <- function(id) {
  # Cria um namespace para este módulo
  ns <- NS(id)
  
  tabItem(
    tabName = "geograph",
    box(
      title = strong("QUANTITATIVO DO VALOR DA VENDA VIA LOCALIZAÇÃO GEOGRÁFICA DO MUNICÍPIOS"), width = 12,
      fluidRow(
        title = "FILTROS",
        selectInput(ns("map_choice"), "ESTATÍSTICA AVALIADA:", choices = c("TOTAL", "MÉDIA"))
      ),
      fluidRow(
        column(
          width = 5, leafletOutput(ns("city_map"))
        ),
        column(
          width = 7, DTOutput(ns("city_table"))
        )
      )
    )
  )
}

# modules/mod_visionGeo_server.R
mod_geograph_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # -----------------------------------
    # [*] TRATAMENTO DE DADOS PARA O MAPA
    # -----------------------------------
    df.map_leaflet <- reactive({
      # Filtro dos Municípios
      GEOPA.filter <- GEOPA %>%
        filter(name_muni %in% citys) %>%
        select(name_muni, geom) %>%
        mutate(CITY = stringr::str_to_upper(name_muni)) %>%
        select(-name_muni)
      
      # Join com os Dados Geográficos e Cálculo da Estatística Analisada
      df.map <- VENDAS %>%
        left_join(GEOPA.filter, by = "CITY") %>%
        select(CITY, VALOR_TOTAL, geom) %>%
        group_by(CITY, geom) %>%
        summarise(ESTATISTICA = ifelse(input$map_choice == "TOTAL", sum(VALOR_TOTAL), mean(VALOR_TOTAL)))
      
      # Percentual (baseado em `ESTATISTICA`)
      df.map$PERCENTUAL <-  (df.map$ESTATISTICA / sum(df.map$ESTATISTICA)) * 100
      
      # Tranformando um objeto usável pelo Leaflet
      df.map <- st_as_sf(df.map)
      
      # Retornar Dados Tratados
      return(df.map)
    })
    
    # --------------------
    # [*] MAPA COM LEAFLET
    # --------------------
    output$city_map <- renderLeaflet({
      # Obtendo Dados para o Mapa
      df.map <- df.map_leaflet()
      
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
        #addProviderTiles(providers$Esri.NatGeoWorldMap)%>%
        addProviderTiles(providers$Esri.WorldStreetMap)%>%
        #addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          fillColor = ~pal(ESTATISTICA),
          weight = 1,                # Borda mais fina
          color = "black",           # Borda branca para contraste
          fillOpacity = 1,           # Leve transparência
          smoothFactor = 0.5,        # Suavização de bordas
          label = ~paste0(
            "<strong>", CITY, "</strong><br>",
            ifelse(input$map_choice == "TOTAL", "TOTAL DE VENDAS (R$): ", "MÉDIA DE VENDAS (R$): "), 
            "<strong>", format(ESTATISTICA, big.mark = ".", decimal.mark = ","), "</strong><br>",
            "PERCENTUAL DE VENDAS (%): ",
            "<strong>", format(PERCENTUAL, decimal.mark = ",", digits = 2), "%", "</strong>"
          ) %>% lapply(htmltools::HTML),
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#333",
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
        )  %>%
        setView(lng = -48.29567407905423, lat = -1.2271001218047826, zoom = 8.5) # Foco na região
    })
    
    # --------------------------------
    # [*] TRATAMENTO DE DADOS À TABELA
    # --------------------------------
    df.city_table <- reactive({
      # Tratamento dos Dados
      df.table <- VENDAS %>%
        group_by(CITY) %>%
        summarise(
          FREQ = n(),
          ESTATISTICA = ifelse("input$map_choice" == "TOTAL", sum(VALOR_TOTAL), mean(VALOR_TOTAL))
        ) %>%
        mutate(
          PERCENT = FREQ / sum(FREQ),
          PERCENT_EST = ESTATISTICA / sum(ESTATISTICA)
        ) %>%
        select(CITY, FREQ, PERCENT, ESTATISTICA, PERCENT_EST)
      
      # Retornar Dados Tratados
      return(df.table)
    })
    
    # -----------------
    # [*] TABELA COM DT
    # -----------------
    output$city_table <- renderDT({
      # Tabela com DT
      datatable(
        df.city_table(),
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

  })
}