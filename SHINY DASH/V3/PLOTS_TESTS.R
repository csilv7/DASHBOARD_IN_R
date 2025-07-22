library(dplyr)
library(tidyr)
library(tidyverse)

library(ggplot2)
library(plotly)
library(echarts4r)

# ----------
#
# ----------

path <- "~/PROJETOS/R/DASHBOARD/CONJUNTO DE DADOS/DATA_GEN_V1.csv"
VENDAS.DF <- read.csv(path)

# ------------
# [] CATEGORIA
# ------------
p.category <- VENDAS.DF %>%
  group_by(CATEGORIA) %>%
  summarise(FREQ = n()) %>%
  arrange(-FREQ) %>%
  mutate(PERCENT = (FREQ / sum(FREQ)) * 100) %>%
  ggplot(aes(y = reorder(CATEGORIA, FREQ), x = FREQ)) +
  geom_bar(aes(fill = CATEGORIA), stat = "identity") +
  geom_text(aes(label = paste0(round(PERCENT, 2), "%")), # Formata para "XX.XX%"
            position = position_stack(vjust = 0.5),      # Centraliza verticalmente
            hjust = 1.1,                                 # Ajusta a posição horizontal (dentro da barra)
            size = 4,
            fontface = "bold",
            color = "black") + 
  labs(x = "FREQUÊNCIA", y = "CATEGORIA") +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

ggplotly(p.category)

# ----------

# VENDAS.DF %>%
#   group_by(CATEGORIA) %>%
#   summarise(FREQUENCIA = n()) %>%
#   mutate(
#     PERCENTUAL = FREQUENCIA / sum(FREQUENCIA),
#     POSITION = cumsum(PERCENTUAL) - 0.5 * PERCENTUAL
#   ) %>%
#   arrange(desc(FREQUENCIA)) %>%
#   ggplot(aes(x = 2, y = PERCENTUAL, fill = CATEGORIA)) +
#   geom_bar(
#     stat = "identity",
#     width = 1,
#     color = "white",
#     size = 0.5
#   ) +
#   coord_polar(theta = "y", start = 0) +
#   xlim(c(0.5, 2.5)) +
#   geom_text(
#     aes(label = paste0(round(PERCENTUAL * 100, 2), "%"), y = POSITION),
#     color = "black",
#     size = 4,
#     fontface = "bold"
#   ) +
#   labs(x = "", y = "") +
#   theme_void(base_size = 12) +
#   theme(
#     axis.title.x = element_text(face = "bold"),
#     axis.title.y = element_text(face = "bold")
#   )

# ----------

VENDAS.DF %>%
  group_by(CATEGORIA) %>%
  summarise(FREQUENCIA = n()) %>%
  mutate(
    PERCENTUAL = FREQUENCIA / sum(FREQUENCIA)
  ) %>%
  arrange(desc(FREQUENCIA)) %>%
  e_chart(CATEGORIA) %>%
  e_pie(
    FREQUENCIA,
    legend = T,
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

# ----------
# [] PRODUTO
# ----------
p.product <- VENDAS.DF %>%
  group_by(PRODUTO) %>%
  summarise(FREQUENCIA = n()) %>%
  mutate(
    PERCENTUAL = (FREQUENCIA / sum(FREQUENCIA)) * 100
  ) %>%
  arrange(desc(FREQUENCIA)) %>%
  ggplot(aes(x = FREQUENCIA, y = reorder(PRODUTO, FREQUENCIA))) +
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

# ----------

VENDAS.DF %>%
  group_by(PRODUTO) %>%
  summarise(FREQUENCIA = n()) %>%
  mutate(
    PERCENTUAL = FREQUENCIA / sum(FREQUENCIA)
  ) %>%
  arrange(desc(FREQUENCIA)) %>%
  e_chart(PRODUTO) %>%
  e_bar(
    FREQUENCIA,
    legend = T
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
  ) %>%
  e_flip_coords()


# --------------
# [] VALOR TOTAL
# --------------

p.histVT <- VENDAS.DF %>%
  ggplot(aes(x = VALOR_TOTAL)) + 
  geom_histogram(color = "white", fill = "darkblue", binwidth = 160) +
  labs(x = "VALOR TOTAL", y = "FREQUÊNCIA") +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

ggplotly(p.histVT)

p.BoxPlotVT <- VENDAS.DF %>%
  ggplot() + 
  geom_boxplot(aes(x = "", y = VALOR_TOTAL, group = 1)) +
  labs(y = "VALOR TOTAL") +
  theme_classic(base_size = 12) +
  theme(
    axis.title.y = element_text(face = "bold")
  )

ggplotly(p.BoxPlotVT)


# ------------------
# [] SÉRIE TEMPORAL
# ------------------
p.timesSeries <- VENDAS.DF %>%
  group_by(ANOS, MESES) %>%
  summarize(VT = sum(VALOR_TOTAL), .groups = "drop") %>%
  mutate(
    ANO_MES = lubridate::ym(paste(ANOS, MESES))
  ) %>%
  ggplot(aes(x = ANO_MES, y = VT, group = 1)) +
  geom_line(color = "blue") + geom_point(color = "red") +
  labs(x = "ANO-MÊS", y = "VALOR TOTAL") +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )
  
ggplotly(p.timesSeries)

p.tsBoxplot <- VENDAS.DF %>%
  group_by(ANOS) %>%
  ggplot(aes(x = ANOS, y = VALOR_TOTAL, color = ANOS)) +
  geom_boxplot() +
  labs(x = "ANO", y = "VALOR TOTAL") +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

ggplotly(p.tsBoxplot)