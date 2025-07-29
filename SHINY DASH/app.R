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
          tabPanel(
            strong("REFERENCIAL"), icon = icon("book"),
            column(width = 8,
              HTML(
                "
              <div text-align: justify>
                <p>O projeto visa democratizar o acesso à informação de vendas, transformando dados brutos em conhecimento acionável. Através de filtros dinâmicos e visualizações interativas, os usuários podem explorar a performance de vendas sob diversas perspectivas, incluindo granularidade geográfica, temporal e de produto.</p>
                <p>A arquitetura do dashboard foi concebida para ser escalável e de fácil manutenção, utilizando as melhores práticas de desenvolvimento em R com o framework Shiny, garantindo um ambiente ágil para futuras expansões e integrações de novas fontes de dados.</p>
              </div>
              "
              )
            )
          ),
          tabPanel(
            strong("MATERIAL E MÉTODOS"), icon = icon("chart-bar"),
            fluidRow(
              column(
                width = 6,
                HTML(
                  "
                  <div>
                    <h1><strong>Base de Dados</strong></h1>
                    <p>A análise é fundamentada em um conjunto de dados simulado de transações de vendas, denominado <code>DATA_GEN_V1.csv</code>. Este dataset compreende informações detalhadas sobre cada venda, incluindo data, localização (cidade), canal de venda, categoria e produto comercializado, valor total, quantidade, desconto aplicado, sexo do cliente e escolaridade estimada.</p>
                    <p>Para a seção de análise geográfica, é empregado um arquivo GeoJSON/RDS (<code>GEOPA.rds</code>) contendo geometrias dos municípios do estado do Pará, permitindo a visualização espacial dos dados de vendas.</p>
                  </div>
                  "
                )
              ),
              column(
                width = 6,
                HTML(
                  "
                  <div>
                    <h1><strong>Metodologia Analítica</strong></h1>
                    <p>A abordagem metodológica consiste em uma análise exploratória de dados (AED) com foco em métricas-chave de vendas:</p>
                    <ul>
                      <li><b>Visão Geral:</b> Apresentação de sumários quantitativos (total de vendas, quantidade vendida, desconto médio) e análises de distribuição (histogramas, boxplots) para o valor total de vendas, permitindo a identificação de outliers e a compreensão da dispersão dos dados.</li>
                      <li><b>Análise Qualitativa:</b> Exploração da frequência de vendas por categoria e produto, utilizando gráficos de pizza e barras para destacar os itens de maior representatividade.</li>
                      <li><b>Análise Geográfica:</b> Mapeamento das vendas por município, permitindo a visualização da performance regional através de mapas coropléticos interativos.</li>
                    </ul>
                    <p>A filtragem de dados é implementada de forma reativa, permitindo aos usuários ajustar o período, localidade, canal de venda, sexo, categoria e produto, proporcionando uma experiência de exploração customizável e imediata.</p>
                  </div>
                  "
                )
              )
            )
          ),
          tabPanel(
            strong("RECURSO COMPUTACIONAL"), icon = icon("desktop"),
            fluidRow(
              column(
                width = 6,
                HTML(
                  "
                  <div>
                   <h1>Recurso Computacional</h1>
                   <p>O presente Dashboard foi desenvolvido utilizando a linguagem de programação <b>R</b>, versão <code>4.5.1</code>, e o ambiente de desenvolvimento integrado <b>RStudio</b>, versão <code>2025.05.1</code>.</p>
                   <h4>Pacotes R Utilizados:</h4>
                   <ul>
                     <li><code>shiny</code>: Framework principal para construção de aplicações web interativas.</li>
                     <li><code>shinydashboard</code> e <code>bs4Dash</code>: Para a criação da estrutura de dashboard e elementos de UI modernos.</li>
                     <li><code>dplyr</code> e <code>tidyr</code>: Ferramentas essenciais para manipulação e transformação de dados.</li>
                     <li><code>tidyverse</code>: Coleção de pacotes para ciência de dados, incluindo <code>ggplot2</code> para visualizações estáticas.</li>
                     <li><code>plotly</code>: Para gráficos interativos e dinâmicos baseados em <code>ggplot2</code>.</li>
                     <li><code>echarts4r</code>: Para a geração de gráficos interativos com a biblioteca ECharts.</li>
                     <li><code>leaflet</code> e <code>sf</code>: Para manipulação de dados geoespaciais e criação de mapas interativos.</li>
                     <li><code>readr</code>: Para importação eficiente de dados CSV.</li>
                     <li><code>htmltools</code>: Para funcionalidades HTML adicionais em elementos Shiny.</li>
                   </ul>
                   <p>A performance da aplicação é otimizada através de funções reativas e boas práticas de programação, garantindo uma experiência fluida mesmo com conjuntos de dados de médio porte.</p>
                 </div>
                  "
                )
              ),
              column(
                width = 6,
                HTML(
                  "
                  
                  "
                )
              )
            )
          ),
          tabPanel(
            strong("RESPONSÁVEL TÉCNICO"), icon = icon("user"),
            HTML(
              "
              <p align='center'>
              <img src='PERFIL_ANALISTA.jpg' alt='Foto de Breno C R Silva' style='width: 150px; height: 150px; border-radius: 50%; bject-fit: cover; border: 3px solid #1772D1; transition: transform 0.3s; cursor: pointer;'/>
              </p> <p align='center'> <strong>Breno C R Silva</strong> </p>
              <p>🏛 Bacharelando em Estatística, Universidade Federal do Pará | 💻 Entusiasta em Ciência de Dados | 📈 Modelagem & Análises </p>
              <p>🔗 <a href='https://www.linkedin.com/in/brenosilva7' target='_blank'>LinkedIn</a> | <a href='https://github.com/csilv7' target='_blank'>GitHub</a></p>
              <p>Eu, Breno, sou um entusiasta de <b>Ciência de Dados e Visualização</b>, com experiência em análise estatística, modelagem preditiva e desenvolvimento de dashboards interativos. Atualmente, dedico-me a projetos que integram técnicas avançadas de estatística com ferramentas modernas de programação, sempre buscando aprimorar a capacidade de transformar dados brutos em insights acionáveis.</p>
              <p>Com um profundo conhecimento em estatística e boas práticas de desenvolvimento em R/Shiny, asseguro a integridade dos dados, a acurácia das análises e a robustez da aplicação, buscando a excelência na entrega de valor através de dashboards intuitivos e informativos.</p>
              <h4>Contato</h4>
              <p>✉ <b>E-mail:</b> breno.silva@icen.ufpa.br</p>
              <p>🌐 <b>Site:</b> <a href='https://www.seusite.com' target='_blank'>www.seusite.com</a></p>
              "
            )
          )
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