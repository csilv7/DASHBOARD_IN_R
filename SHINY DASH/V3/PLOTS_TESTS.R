library(dplyr)
library(ggplot2)
library(plotly)

path <- "/cloud/project/CONJUNTO DE DADOS/DATA_GEN_V1.csv"
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
# [] PRODUTO
# ----------
p.product <- VENDAS.DF %>%
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