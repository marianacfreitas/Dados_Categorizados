# Pacotes utilizados
library(readr)
library(dplyr)
library(janitor)
library(tidyr)
library(maxstat)

# Carregando dados 
dados_aux <- read.csv("employee_promotion.csv") |>
  select(-c(employee_id, region))

# Pontos de corte para algumas variáveis
cutpoint <- maxstat.test(is_promoted ~ age, data = dados_aux)
cutpoint$estimate
cutpoint <- maxstat.test(is_promoted ~ length_of_service, data = dados_aux)
cutpoint$estimate
cutpoint <- maxstat.test(is_promoted ~ avg_training_score, data = dados_aux)
cutpoint$estimate
cutpoint <- maxstat.test(is_promoted ~ no_of_trainings, data = dados_aux)
cutpoint$estimate

dados <- dados_aux |>
  mutate(
    age_less_44_years = case_when(age < 44 ~ "Yes",
                                    age >= 44 ~ "No"),
    length_of_service_less_10_years = case_when(length_of_service < 10 ~ "Yes",
                                                length_of_service >= 10 ~ "No"),
    avg_training_score_less_82 = case_when(avg_training_score < 82 ~ "Yes",
                                                 avg_training_score >= 82 ~ "No"),
    no_of_trainings_more_than_1 = case_when(no_of_trainings <= 1 ~ "No",
                                            no_of_trainings > 82 ~ "Yes"),
    is_promoted = case_when(is_promoted == 1 ~ "Yes",
                            is_promoted == 0 ~ "No")
  ) |>
  select(-c(age, length_of_service, avg_training_score, no_of_trainings))

# Imputando a moda nas colunas com NA ou em branco
for (col in names(dados)) {
  if (any(is.na(dados[[col]])) | any(dados[[col]] == "")) {
    moda <- names(which.max(table(dados[[col]])))
    dados[[col]][is.na(dados[[col]])] <- moda
  }
}

# Transformando todas as variáveis em fator

dados <- dados |>
  mutate(
    department = factor(department),
    education = factor(education, levels = c("Below Secondary", "Bachelor's", "Master's & above")),
    gender = factor(gender),
    recruitment_channel = factor(recruitment_channel, levels = c("sourcing", "referred", "other")),
    previous_year_rating = factor(previous_year_rating, levels = c("1", "2", "3", "4", "5")),
    awards_won = factor(awards_won., levels = c("0", "1")),
    is_promoted = factor(is_promoted, levels = c("Yes", "No")),
    age_less_44_years = factor(age_less_44_years, levels = c("Yes", "No")),
    length_of_service_less_10_years = factor(length_of_service_less_10_years, levels = c("Yes", "No")),
    avg_training_score_less_82 = factor(avg_training_score_less_82, levels = c("Yes", "No")),
    no_of_trainings_more_than_1 = factor(no_of_trainings_more_than_1, levels = c("Yes", "No")),
    
  )

