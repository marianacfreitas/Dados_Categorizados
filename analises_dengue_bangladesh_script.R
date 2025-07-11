# Pacotes utilizados
library(readr)
library(dplyr)
library(janitor)
library(tidyr)
library(maxstat)
library(data.table)

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
    Outcome = case_when(Outcome == 1 ~ "Presence of Dengue",
                    Outcome == 0 ~ "Absence of Dengue")
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

