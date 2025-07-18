# --------------------------
# [*] CONFIGURAÇÕES INICIAIS
# --------------------------

library(dplyr)
library(lubridate)

# Defidnidndo semente
set.seed(123456789)

# Tamanho Amostral
n <- 5000

# ------------------------
# [1] VARIÁVEIS PREDITORAS
# ------------------------

# ------------------------
# [1.1] PERFIL DEMOGRÁFICO
# ------------------------

# Perfil Demográfico - https://www.fapespa.pa.gov.br/sistemas/anuario2020/tabelas/demografia/tab-1.3-populacao-por-sexo-2016-a-2020.htm
city.profiles <- list(
  "BELÉM" = list(
    sex_ratio  = c(MASCULINO = 701185 / (701185 + 798456), FEMININO = 798456 / (701185 + 798456)), 
    age_groups = c("15-25" = 0.15, "26-35" = 0.30, "36-50" = 0.28, "50-65" = 0.20, "66+" = 0.07)
  ),
  "ANANINDEUA" = list(
    sex_ratio  = c(MASCULINO = 253904 / (253904 + 281643), FEMININO = 281643 / (253904 + 281643)), 
    age_groups = c("15-25" = 0.16, "26-35" = 0.32, "36-50" = 0.27, "50-65" = 0.19, "66+" = 0.06)
  ),
  "MARITUBA" = list(
    sex_ratio  = c(MASCULINO = 66712 / (66712 + 66973), FEMININO = 66973 / (66712 + 66973)), 
    age_groups = c("15-25" = 0.17, "26-35" = 0.33, "36-50" = 0.26, "50-65" = 0.18, "66+" = 0.06)
  ),
  "BENEVIDES" = list(
    sex_ratio  = c(MASCULINO = 31604 / (31604 + 32164), FEMININO = 32164 / (31604 + 32164)), 
    age_groups = c("15-25" = 0.20, "26-35" = 0.30, "36-50" = 0.25, "50-65" = 0.18, "66+" = 0.07)
  ),
  "SANTA BÁRBARA" = list(
    sex_ratio  = c(MASCULINO = 10874 / (10874 + 10575), FEMININO = 10575 / (10874 + 10575)), 
    age_groups = c("15-25" = 0.22, "26-35" = 0.28, "36-50" = 0.24, "50-65" = 0.17, "66+" = 0.09)
  )
)

# Gerar Pesos por Cidade - https://fapespa.pa.gov.br/sistemas/anuario2023/tabelas/demografia/tab-1.1-populacao-total-do-para-e-municipios-estimativas-populacionais-e-censo-deografico-2018-a-2022.htm
weights.city <- c(
  "BELÉM" = 1499641,
  "ANANINDEUA" = 535547,
  "MARITUBA" = 133685,
  "BENEVIDES" = 63768	,
  "SANTA BÁRBARA" = 21449
) %>% prop.table()

# Definindo semente
set.seed(123456789)

# Vetor de Cidades (Amostradas)
citys <- sample(x = names(weights.city), size = n, replace = T, prob = weights.city)

# Armazenando em um Data Frame
df <- data.frame(CITY = citys, SEX = NA)

# Simulando o "SEX" de acordo com o perfil demográfrico
df <- df %>%
  group_by(CITY) %>%
  mutate(
    SEX = sample(
      x = names(city.profiles[[unique(CITY)]]$sex_ratio),
      size = n(),
      replace = T,
      prob = city.profiles[[unique(CITY)]]$sex_ratio
    )
  )

# Criando vetor para armazenar idades
df$IDADE <- NA

for (city in unique(df$CITY)) {
  idx <- df$CITY == city
  
  # Obtem perfil etário
  range.probs <- city.profiles[[city]]$age_groups
  range.names <- names(range.probs)
  
  # Sorteia faixas para os clientes da cidade
  range.drawn <- sample(
    x = range.names,
    size = sum(idx),
    replace = TRUE,
    prob = range.probs
  )
  
  # Converte faixas em idade real
  ages.drawn <- sapply(range.drawn, function(range_) {
    limits <- as.numeric(unlist(strsplit(range_, "-|\\+")))
    if (length(limits) == 2) {
      sample(x = limits[1]:limits[2], size = 1)
    } else {
      sample(x = limits[1]:75, size = 1)  # Idade Máxima: 75
    }
  })
  
  # Atribui ao data frame
  df[idx, "IDADE"] <- ages.drawn
}

# -----------------------
# [1.2] PADRÕES DE VENDAS
# -----------------------

# CANAL DE VENDA por SEXO e CIDADE
df$CANAL_VENDA <- mapply(function(sex, city) {
  if (city %in% c("BENEVIDES", "SANTA BÁRBARA")) {
    probs <- c("ONLINE" = 0.15, "LOJA FÍSICA" = 0.25, "REVENDEDOR" = 0.60)
  } else if (sex == "MASCULINO") {
    probs <- c("ONLINE" = 0.60, "LOJA FÍSICA" = 0.25, "REVENDEDOR" = 0.15)
  } else {
    probs <- c("ONLINE" = 0.30, "LOJA FÍSICA" = 0.50, "REVENDEDOR" = 0.20)
  }
  sample(names(probs), size = 1, prob = probs)
}, df$SEX, df$CITY)

# CATEGORIA por SEXO e CIDADE
df$CATEGORIA <- mapply(function(sex, city) {
  base_probs <- rep(0.2, 5)
  names(base_probs) <- c("ELETRÔNICOS", "ROUPAS", "ALIMENTOS", "LIVROS", "BRINQUEDOS")
  
  # Ajustes por sexo
  if (sex == "MASCULINO") {
    base_probs["ELETRÔNICOS"] <- base_probs["ELETRÔNICOS"] + 0.1
    base_probs["ALIMENTOS"] <- base_probs["ALIMENTOS"] + 0.1
    base_probs["ROUPAS"] <- base_probs["ROUPAS"] - 0.05
    base_probs["LIVROS"] <- base_probs["LIVROS"] - 0.05
  } else {
    base_probs["ROUPAS"] <- base_probs["ROUPAS"] + 0.1
    base_probs["BRINQUEDOS"] <- base_probs["BRINQUEDOS"] + 0.1
    base_probs["ELETRÔNICOS"] <- base_probs["ELETRÔNICOS"] - 0.05
    base_probs["ALIMENTOS"] <- base_probs["ALIMENTOS"] - 0.05
  }
  
  # Ajustes por cidade
  if (city == "BELÉM") {
    base_probs["ELETRÔNICOS"] <- base_probs["ELETRÔNICOS"] + 0.05
    base_probs["LIVROS"] <- base_probs["LIVROS"] + 0.05
  } else if (city %in% c("ANANINDEUA", "MARITUBA")) {
    base_probs["ALIMENTOS"] <- base_probs["ALIMENTOS"] + 0.1
    base_probs["BRINQUEDOS"] <- base_probs["BRINQUEDOS"] + 0.1
  } else if (city %in% c("BENEVIDES", "SANTA BÁRBARA")) {
    base_probs["ROUPAS"] <- base_probs["ROUPAS"] + 0.1
  }
  
  # Normaliza
  base_probs <- base_probs / sum(base_probs)
  sample(names(base_probs), size = 1, prob = base_probs)
}, df$SEX, df$CITY)

# Dicionário de Produtos por Categoria
categories.and.products <- list(
  "ELETRÔNICOS" = c("SMARTPHONE PREMIUM", "FONE DE OUVIDO BLUETOOTH"),
  "ROUPAS" = c("CALÇA JEANS MASCULINA", "VESTIDO FLORAL FEMININO"), 
  "ALIMENTOS" = c("CESTA DE FRUTAS ORGÂNICAS", "CAFÉ GOURMET (250G)"), 
  "LIVROS" = c("BEST-SELLER DE FICÇÃO", "LIVRO TÉCNICO/ACADÊMICO"), 
  "BRINQUEDOS" = c("KIT DE CONSTRUÇÃO ROBÓTICA", "BONECA COLECIONÁVEL")
)

# PRODUTO por Categoria
df$PRODUTO <- mapply(function(cat) {
  sample(categories.and.products[[cat]], size = 1)
}, df$CATEGORIA)

# -------------------------------
# [1.3] CARACTERÍSTICAS ESTIMADAS
# -------------------------------

# Preço médio por produto
preco_base <- c(
  "SMARTPHONE PREMIUM" = 4500,
  "FONE DE OUVIDO BLUETOOTH" = 350,
  "CALÇA JEANS MASCULINA" = 180,
  "VESTIDO FLORAL FEMININO" = 220,
  "CESTA DE FRUTAS ORGÂNICAS" = 80,
  "CAFÉ GOURMET (250G)" = 45,
  "BEST-SELLER DE FICÇÃO" = 70,
  "LIVRO TÉCNICO/ACADÊMICO" = 150,
  "KIT DE CONSTRUÇÃO ROBÓTICA" = 280,
  "BONECA COLECIONÁVEL" = 120
)

# Log-normal com base no preço base
df$PRECO <- mapply(function(produto) {
  media <- log(preco_base[produto])
  round(rlnorm(1, meanlog = media, sdlog = 0.25), 2)
}, df$PRODUTO)

# Quantidade com base inversa do preço
df$QUANTIDADE <- mapply(function(preco) {
  if (preco < 50) {
    sample(1:5, 1, prob = c(0.3, 0.25, 0.2, 0.15, 0.1))
  } else if (preco < 200) {
    sample(1:3, 1, prob = c(0.5, 0.3, 0.2))
  } else {
    sample(1:2, 1, prob = c(0.8, 0.2))
  }
}, df$PRECO)

# Desconto proporcional ao canal
df$DESCONTO_APLICADO <- mapply(function(canal) {
  if (canal == "ONLINE") runif(1, 0.05, 0.25)
  else if (canal == "LOJA FÍSICA") runif(1, 0.01, 0.15)
  else runif(1, 0.02, 0.2)
}, df$CANAL_VENDA)

# RENDA_ESTIMADA (em R$ mil)
df$RENDA_ESTIMADA <- mapply(function(cidade, idade, canal) {
  base <- case_when(
    cidade == "BELÉM" ~ 3.0,
    cidade == "ANANINDEUA" ~ 2.2,
    cidade == "MARITUBA" ~ 2.0,
    cidade == "BENEVIDES" ~ 1.9,
    cidade == "SANTA BÁRBARA" ~ 1.7
  )
  idade_adj <- ifelse(idade < 26, -0.2, ifelse(idade <= 50, 0.1, -0.1))
  canal_adj <- ifelse(canal == "REVENDEDOR", 0.2, ifelse(canal == "ONLINE", 0.1, 0))
  renda <- base + idade_adj + canal_adj
  round(rnorm(1, mean = renda, sd = 0.3), 2)
}, df$CITY, df$IDADE, df$CANAL_VENDA)

# ESCOLARIDADE SCORE (de 0 a 1)
df$ESCOLARIDADE_SCORE <- mapply(function(idade, sexo, cidade) {
  base <- case_when(
    cidade == "BELÉM" ~ 0.7,
    cidade == "ANANINDEUA" ~ 0.575,
    cidade == "MARITUBA" ~ 0.5,
    cidade == "BENEVIDES" ~ 0.45,
    cidade == "SANTA BÁRBARA" ~ 0.15
  )
  idade_adj <- ifelse(idade < 20, 0.5, ifelse(idade < 35, 0.65, ifelse(idade < 65, 0.7, ifelse(idade < 80, 0.9))))
  sexo_adj <- ifelse(sexo == "FEMININO", 0.75, 0.45)
  esc <- mean(c(base, idade_adj, sexo_adj))
  min(max(round(rnorm(1, mean = esc, sd = 0.05), 2), 0), 1.0)
}, df$IDADE, df$SEX, df$CITY)

df[, "ESCOLARIDADE_ROTULADA"] <- cut(df[["ESCOLARIDADE_SCORE"]],
                                     breaks = c(0, 0.125, 0.25, 0.375, 0.5, 0.675, 0.75, 0.875, 1),
                                     labels = c("SEM ESCOLARIDADE", "ENS FUND INCOMP", "ENS FUND COMP",
                                                "ENS MÉDIO INCOMP", "ENS MÉDIO COMP", "ENS SUPER INCOMP", "ENS SUPER COMP",
                                                "PÓS-GRADUAÇÃO"),
                                     right = TRUE)

# # ------------------------------------
# # [1.4] TENDÊNCIA LINEAR (NORMALIZADA)
# # ------------------------------------
# 
# # Criação da tendência linear multiplicativa (padrão entre 0.8 a 1.2)
# df$TENDENCIA <- with(df, {
#   # Coeficientes fictícios
#   b0 <- 0.9
#   b1 <- 0.1 * scale(RENDA_ESTIMADA)
#   b2 <- 0.05 * scale(ESCOLARIDADE_SCORE)
#   b3 <- ifelse(SEX == "FEMALE", 0.045, -0.025)
#   b4 <- ifelse(CANAL_VENDA == "REVENDEDOR", 0.05, 0)
#   tendencia_raw <- b0 + b1 + b2 + b3 + b4
#   round(pmax(0.75, pmin(1.25, tendencia_raw)), 3)  # limites seguros
# })

# ---------------------------
# [2] GERANDO DADOS TEMPORAIS
# ---------------------------

# ----------------------
# [2.1] PADRÕES SAZONAIS
# ----------------------

# Gerar datas
dt <- seq(as.Date("2015-01-01"), as.Date("2025-12-31"), by = "day")

# Tamanho de dt
n.dt <- length(dt)

# Datas Extras
extra.dates <- sample(x = dt, size = n - n.dt, replace = T)
dates <- sample(c(dt, extra.dates)) # Concatena e Embaralha os valores do Vetor

# Sazonalidade semanal
weekly.seasonality <- c(1.1, 1.2, 1.0, 0.9, 0.8, 1.3, 1.5) # Sunday, Monday, Tuesday, Wednesday, Thursday, Friday and Saturday

# Sazonalidade mensal
monthly.seasonality <- c(1.0, 0.9, 1.15, 1.25, 1.35, 1.05, 1.1, 1, 1.16, 1.26, 1.36, 1.15)

# Sazonalidade Anual
annual.seasonality <- c(0.5, 1, 1.5, 1, 1.75, 2.5, 2, 2.5, 3.75, 4, 3.25, 4.25)

# Obter índices dos dias da semana
weekday.idx <- as.numeric(format(dates, "%w")) + 1
month.idx <- as.numeric(format(dates, "%m"))
year.idx <- as.numeric(format(dates, "%y")) - 14

# Aplicar fator sazonal: vetorizado
Zt.seasonal <- weekly.seasonality[weekday.idx] * monthly.seasonality[month.idx] * annual.seasonality[year.idx]

# Efeito Sazonal
df$SEASONAL_EFFECT <- Zt.seasonal

# Armazenando no Data Frame
df$DATA_VENDA <- dates

# Armazenar ANOS, MESES E
df$ANOS <- as.numeric(format(dates, "%y")) + 2000
df$MESES <- as.numeric(format(dates, "%m"))
df$DIA_SEMANA <- as.numeric(format(dates, "%w")) + 1

# ---------------------
# [3] VARIÁVEL RESPOSTA
# ---------------------

# Valor total ajustado
df$VALOR_TOTAL <- round(
  df$PRECO * df$QUANTIDADE * (1 - df$DESCONTO_APLICADO) * Zt.seasonal, 2
)

# Visualizar
print(df)

# Salvar Conjunto de Dados
write.csv(x = df, file = "/cloud/project/CONJUNTO DE DADOS/DATA_GEN_V1.csv", row.names = F) # em CSV
save(df, file = "/cloud/project/CONJUNTO DE DADOS/DATA_GEN_V1.RData") # em RData