# Pacotes utilizados
library(readr)
library(dplyr)
library(janitor)
library(tidyr)
library(maxstat)
library(data.table)
library(caret)
library(PropCIs)

# Carregando dados 
dados_aux <- fread("https://www.kaggle.com/api/v1/datasets/download/kawsarahmad/dengue-dataset-bangladesh")

# Pontos de corte para a idade
cutpoint <- maxstat.test(Outcome ~ Age, data = dados_aux)
cutpoint$estimate


dados <- dados_aux |>
  mutate(
    Age_less_16_years = case_when(Age < 16 ~ "Yes",
                                    Age >= 16 ~ "No"),
    NS1 = case_when(NS1 == 1 ~ "Positive",
                    NS1 == 0 ~ "Negative"),
    IgG = case_when(IgG == 1 ~ "Positive",
                    IgG == 0 ~ "Negative"),
    IgM = case_when(IgM == 1 ~ "Positive",
                    IgM == 0 ~ "Negative"),
    Outcome = case_when(Outcome == 1 ~ "Positive",
                    Outcome == 0 ~ "Negative")
  ) |>
  select(-c(Age, District))

# Número de variáveis faltantes
sum(is.na(dados))

# Transformando todas as variáveis em fator

dados <- dados |>
  mutate(
    Gender = factor(Gender),
    IgG = factor(IgG),
    NS1 = factor(NS1),
    IgM = factor(IgM),
    Area = factor(Area),
    AreaType = factor(AreaType, levels = c("Undeveloped", "Developed")),
    HouseType = factor(HouseType, levels = c("Tinshed", "Building", "Other")),
    Age_less_16_years = factor(Age_less_16_years),
    Outcome = factor(Outcome)
  )

#--------------- Análise de teste diagnósticos -----------------------
# A função confusionMatrix retorna:
# Tabela teste x resultado real
# Acurácia
# Kappa (análise de concordância)
# Teste de Mcnemar
# Sensibilidade
# Especificidade

# métricas para NS1
confusionMatrix(as.factor(dados$NS1), as.factor(dados$Outcome), positive = "Positive")
#métricas para IgG
confusionMatrix(as.factor(dados$IgG), as.factor(dados$Outcome), positive = "Positive")
#métricas para IgM
confusionMatrix(as.factor(dados$IgM), as.factor(dados$Outcome), positive = "Positive")

#--------------- Diferença de duas proporções -----------------------


comparar_proporcoes <- function(data, grupo_var, desfecho_var, positivo_label = "Positive", conf.level = 0.95) {
  # Extrair os dois níveis do grupo
  grupo_niveis <- unique(data[[grupo_var]])
  if (length(grupo_niveis) != 2) stop("A variável do grupo deve ter exatamente dois níveis.")
  
  g1 <- grupo_niveis[1]
  g2 <- grupo_niveis[2]
  
  # Totais
  n1 <- nrow(dplyr::filter(data, !!rlang::sym(grupo_var) == g1))
  n2 <- nrow(dplyr::filter(data, !!rlang::sym(grupo_var) == g2))
  
  # Positivos
  x1 <- nrow(dplyr::filter(data, !!rlang::sym(grupo_var) == g1, !!rlang::sym(desfecho_var) == positivo_label))
  x2 <- nrow(dplyr::filter(data, !!rlang::sym(grupo_var) == g2, !!rlang::sym(desfecho_var) == positivo_label))
  
  # Proporções
  p1 <- x1 / n1
  p2 <- x2 / n2
  diff <- p1 - p2
  
  # Erro padrão e IC
  z <- qnorm(1 - (1 - conf.level)/2)
  se <- sqrt( (p1 * (1 - p1)) / n1 + (p2 * (1 - p2)) / n2 )
  lower <- diff - z * se
  upper <- diff + z * se
  
  # Retorno
  result <- list(
    grupo_1 = g1,
    grupo_2 = g2,
    total_g1 = n1,
    total_g2 = n2,
    positivos_g1 = x1,
    positivos_g2 = x2,
    prop_g1 = p1,
    prop_g2 = p2,
    diff_prop = diff,
    ic_95 = c(lower, upper)
  )
  
  return(result)
}

# Para Gender
res_gender <- comparar_proporcoes(dados, grupo_var = "Gender", desfecho_var = "Outcome")
print(res_gender)

# Para AreaType
res_areatype <- comparar_proporcoes(dados, grupo_var = "AreaType", desfecho_var = "Outcome")
print(res_areatype)

# Para Age_less_16_years
res_age <- comparar_proporcoes(dados, grupo_var = "Age_less_16_years", desfecho_var = "Outcome")
print(res_age)

#--------------- Análises de Razão de Chances -----------------------

#### Gender

# Contagens
a <- nrow(filter(dados, Gender == "Male" & Outcome == "Positive"))
b <- nrow(filter(dados, Gender == "Male" & Outcome == "Negative"))
c <- nrow(filter(dados, Gender == "Female" & Outcome == "Positive"))
d <- nrow(filter(dados, Gender == "Female" & Outcome == "Negative"))

# OR, log(OR), SE
or_gender <- (a * d) / (b * c)
log_or_gender <- log(or_gender)
se_log_or_gender <- sqrt(1/a + 1/b + 1/c + 1/d)

# IC 95%
z <- qnorm(0.975)
ic_log_gender <- log_or_gender + c(-1, 1) * z * se_log_or_gender
ic_gender <- exp(ic_log_gender)

# Resultado
cat("🔹 Gender:\n")
cat("  OR =", round(or_gender, 3), "\n")
cat("  log(OR) =", round(log_or_gender, 3), "\n")
cat("  IC 95% OR = [", round(ic_gender[1], 3), ",", round(ic_gender[2], 3), "]\n\n")


#### AreaType

a <- nrow(filter(dados, AreaType == "Developed" & Outcome == "Positive"))
b <- nrow(filter(dados, AreaType == "Developed" & Outcome == "Negative"))
c <- nrow(filter(dados, AreaType == "Undeveloped" & Outcome == "Positive"))
d <- nrow(filter(dados, AreaType == "Undeveloped" & Outcome == "Negative"))

or_area <- (a * d) / (b * c)
log_or_area <- log(or_area)
se_log_or_area <- sqrt(1/a + 1/b + 1/c + 1/d)

ic_log_area <- log_or_area + c(-1, 1) * z * se_log_or_area
ic_area <- exp(ic_log_area)

cat("🔹 AreaType:\n")
cat("  OR =", round(or_area, 3), "\n")
cat("  log(OR) =", round(log_or_area, 3), "\n")
cat("  IC 95% OR = [", round(ic_area[1], 3), ",", round(ic_area[2], 3), "]\n\n")

#### Age_less_16_years 

a <- nrow(filter(dados, Age_less_16_years == "Yes" & Outcome == "Positive"))
b <- nrow(filter(dados, Age_less_16_years == "Yes" & Outcome == "Negative"))
c <- nrow(filter(dados, Age_less_16_years == "No" & Outcome == "Positive"))
d <- nrow(filter(dados, Age_less_16_years == "No" & Outcome == "Negative"))

or_age <- (a * d) / (b * c)
log_or_age <- log(or_age)
se_log_or_age <- sqrt(1/a + 1/b + 1/c + 1/d)

ic_log_age <- log_or_age + c(-1, 1) * z * se_log_or_age
ic_age <- exp(ic_log_age)

cat("🔹 Age < 16:\n")
cat("  OR =", round(or_age, 3), "\n")
cat("  log(OR) =", round(log_or_age, 3), "\n")
cat("  IC 95% OR = [", round(ic_age[1], 3), ",", round(ic_age[2], 3), "]\n\n")

# ------------- Modelo de Regressão Logística

# Modelo com várias variáveis categóricas
modelo_logistico <- glm(Outcome ~ ., 
                        data = dados, family = binomial)

# Resumo do modelo
summary(modelo_logistico)

# Odds ratios e ICs
exp(cbind(OR = coef(modelo_logistico), confint(modelo_logistico)))



