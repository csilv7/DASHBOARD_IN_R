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
    # --------------------
    # [*] MAPA COM LEAFLET
    # --------------------
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
    
    # -----------------
    # [*] TABELA COM DT
    # -----------------
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
    # -----------------
    # [*] FIM DO SERVER
    # -----------------
  })
}