---
title: "Relatório de Dengue em Bangladesh"
author: "MarianaFreitas e Aline Pires"
date: " "
output: pdf_document
---




# Introdução

# Metodologia

Dados categorizados são conjuntos de dados cujas variáveis são categóricas, ou seja, representam característica, qualidade ou atributo. Essas variáveis categóricas podem ser nominais, quando as classes da variável não tem ordem natural (gênero, tipo sanguíneo..) ou ordinais, quando as classes apresentam ordem natural (nível de escolaridade, grau de dor,...). Uma importante técnica na análise de dados categorizados são as tabelas de contigência, ideais para organizar a frequência das interseções entre as variáveis categóricas, permitindo a observação de associação entre variáveis, cálculo de medidas de desempenho de testes diagnósticos e realização de testes de associação, simetria ou homogeneidade entre variáveis.

No contexto de testes diagnósticos - testes que identificam se um indivíduo apresenta ou não determinada doença ou condição - é possível calcular algumas medidas para aavaliar a performance dos testes, já que estão sujeitos a erros e, consequentemente, seus resultados apresentam grau de incerteza. Duas medidas muito utilizadas são a sensibilidade e especificidade. A sensibilidade é calculada como a razão de verdadeiros positivos (doentes cujo teste foi positivo) em relação à soma de falsos negativos (doentes cujo teste foi negativo) e verdadeiros positivos. Já a especificidade corresponde à razão de verdadeiros negativos (não doentes cujo teste foi negativo) em relação à soma de verdadeiros negativos e falsos positivos (não doentes cujo teste foi positivo). Assim, a sensibilidade pode ser interpretada como a probabilidade do teste ser positivo dados que o indivíduo está doente e a especificidade como a probabilidade do teste ser negativo dado que o indivíduo não está doente. Um bom teste apresenta sensibilidade e especificidade altas, sendo que podem variar de 0 a 1.

O tipo de estudo e delineamento amostral são importantes para a interpretação de resultados. Nesse caso, será justificado que se trata de um estudo transversal, mas não há informações suficientes para definir o delineamento amostral. Para esse tipo de estudo, cabe verificar se há diferenças nas proporções de doentes em diferentes classes de variáveis binárias. Para isso, é calculada a estimação pontual da diferença entre as porporções e em seguida é feito o intervalo de confiança. Se o intervalo de confiança incluir zero, não se pode afirmar que há uma diferença entre as proporções. Caso contrário, há diferença nas porporções a uma determinada nível de confiança - nesse trabalho foi utilizado 95%. Outra importante alternativa para avaliar associação em tabelas de contigência $2x2$ são as razão de chances, medida apropriada para o tipo de estudo transversal. A razão de chances se trata da razão entre a chance de uma classe apresentar a doença e a 
chance da outra classe apresentar a doença. Foi feita uma estimativa da razão de chance e em seguida foi complementada por inferência estatística, com cálculo do logaritmo da razão de chances e respectivos intervalos de confiança obtidos por aproximação normal. Se o intervalo de confiança incluir 1, não se pode afirmar que há diferença entre as chances, caso contrário há diferença a um determinado nível de confiança.

Além de inferência estatística para as proporções, também foram feitos testes para avaliar independência, associação, simetria e homogeneidade.  Primeiro forma feitos testes específicoa para variáveis ordinais. Para verificar a intensidade e direção da associação entre variáveis ordinais, foram aplicadas os testes Gama de Goodman e Kruskal, Tau de Kendall e Tau-b de Kendall - que consideram a ordenação das classes. A Gama de Goodman e Kruskal se baseia em pares concordantes e discordantes em tabelas de contingência, variando de -1 a 1, indicando associação perfeita negativa ou positiva, respectivamente, e desconsidera os pares empatados. As medidas Tau de Kendall e Tau-b de Kendall corrigem a Gama ao considerar empates nas margens. O Tau-b é útil para tabelas não quadradas, visto que ajusta a estatística levando em conta o número de empates nas linhas e colunas. Para testar tendência linear entre variáveis ordinais, podem ser usados os testes de Cochran-Armitage - em tabelas para verificar se a proporção de sucesso aumenta ou diminui linearmente com as categorias das variáveis ordinais - e o teste de Mantel - em tabelas $sxr$, avaliando a presença de uma tendência linear global entre variáveis ordinais. Aqui foi aplicado apenas o primeiro teste, já que há apenas duas variáveis ordinais.

Para analisar a associação entre duas variáveis categóricas controlando por uma terceira variável, foram construídas tabelas de contingência parciais, e calculadas razões de chances condicionais em cada classe, permitindo avaliar se a associação é condicionalmente homogênea entre as classes. A homogeneidade das razões de chances foi testada com o teste de Breslow-Day, que avalia se as ORs são estatisticamente iguais entre as classes. Quando a homogeneidade foi aceita, foi utilizado o teste de Mantel-Haenszel, que fornece uma razão de chances combinada ajustada, além de um teste de associação global. Essa técinica é importante para lidar com casos em que ocorre o Paradoxo de Simpson - quando a associação entre duas variáveis muda após o controle por uma terceira. 

Para tabelas de contingência com dimensões $r×s$, foram aplicados algins testes para avaliar associação e simetria. O teste qui-quadrado de Pearson foi utilizado para verificar a independência entre linhas e colunas. O teste de razão de verossimilhança, uma alternativa ao qui-quadrado, tem base no modelo de log-verossimilhança, sendo mais adequado em amostras pequenas ou quando os pressupostos de normalidade não são satisfeitos. O teste de homogeneidade foi aplicado em situações nas quais uma das variáveis representa grupos e a outra representa categorias de resposta, buscando verificar se a distribuição de respostas é homogênea entre os grupos. O teste de simetria foi utilizado em tabelas quadradas para avaliar se a frequência de observações na célula $(i,j)$ é igual a $(j, i)$, útil para dados pareados ou classificações duplas. O teste de homogeneidade marginal, também em tabelas quadradas, verificou se as distribuições marginais das linhas e colunas são idênticas, independentemente da simetria.

Por fim, foi abordada a etapa de modelagem em tabelas de contigência. Foi ajustado um modelo de regressão logística, permitindo estimar a probabilidade de doença como função das variáveis explicativas categóricas. Os coeficientes do modelo foram interpretados em termos do log das razões de chance. Pela base de dados desse trabalho se tratar de um problema com variável resposta binária, os modelos log-lineares não foram utilizados. Fio feita uma seleção do modelo de regressão logística mais adequado a partir do critério de Informação Akaike (AIC) e análise do ajuste do modelo.

Todas as análises foram feitas utilizando o software R, com pacotes específicos mencionados ao longo do relatório.

# Resultados

## Análise exploratória
+ Falar sobre os dados (variáveis, fonte, ...) Aline
+ falar que apenas age_less_16 e AreaType são ordinais Aline
+ Fazer análises que julgar interessantes Aline


## Avaliação de Testes Diagnósticos

O conjunto de dados apresenta três testes para detectar a dengue: denotados por NS1, IgG e IgM. Considerando apenas as informações sobre os resultados desses três testes e a presença ou não da doença nos indivíduos testados, são obtidas as tabelas 1, 2 e 3 para NS1, IgG e IgM, respectivamente.

Tabela 1: Distribuição dos desfechos segundo teste NS1.

| NS1/Outcome  | Negative | Positive |
|--------------|----------|----------|
| Negative     |   467    |    14    |
| Positive     |     0    |   519    |

Tabela 2: Distribuição dos desfechos segundo teste IgG.

| IgG/Outcome  | Negative | Positive |
|--------------|----------|----------|
| Negative     |   467    |    0     |
| Positive     |     0    |   533    |

Tabela 3: Distribuição dos desfechos segundo teste IgM.

| IgM/Outcome  | Negative | Positive |
|--------------|----------|----------|
| Negative     |   251    |    274   |
| Positive     |     216  |   259    |

Apenas observando as tabelas parciais é possível notar que o teste mais preciso parece ser o IgG, enquanto o de menor eficácia seria o IgM. No entanto, essa intuição pode ser formalizada utilizando as medidas de sensibilidade e especificidade apresentadas na tabela 4.

Tabela 4: Medidas de avaliação dos testes diagnósticos.

|Teste Diagnóstico | Sensibilidade | Especificidade | 
|------------------|---------------|----------------|
| NS1              | 0,974         | 1              |
|IgG               | 1             | 1              |
|IgM               | 0.486         | 0.538          |

Concluões sobre os testes:
+ O teste NS1 apresentou boa performance no geral, classificando corretamente todos os que não tinham dengue e também com alta sensibilidade - indicando que classificou grande parte dos indivíduos com a diença corretamente.
+ O teste IgG classificou corretamente todos os indivíduos.
+ O teste IgM teve performance bem ruim, com ambas as medidas baixas.


## Tabelas de Contigência

Para decidir que ferramentas serão usadas na análise de uma tabela de contigência, é importante antes de qualquer coisa entender qual tipo é o tipo de estudo. A partir das informações fornecidas pela fonte dos dados, é possível inferir que se trata de um estudo transversal, pois os dados são colhidos em um ponto específico no tempo após a ocorrência ou não ocorrãncia de dengue, não houve nenhum tipo de intervenção ou acompanhamento dos indivíduos. Esse tipo de estudo permite a verificar se há diferenças nas proporções de doentes nas classes de variáveis binárias, a partir da criação de tabelas $2x2$ considerando a variável explicativa binária de interesse e a variável resposta. Nas tabelas 5, 6 e 7 são mostradas as tabelas $2x2$ para as variáveis binárias `Gender`, `AreaType` e `Age_less_16_years`.


Tabela 5: Distribuição dos desfechos segundo gênero.

| Gender/Outcome    | Negative | Positive |
|-------------------|----------|----------|
| Female            |   243    |   281    |
| Male              |   224    |   252    |


Tabela 6: Distribuição dos desfechos segundo tipo de área.

| AreaType/Outcome | Negative | Positive |
|------------------|----------|----------|
| Developed        |   244    |   257    |
| Undeveloped      |   223    |   276    |


Tabela 7: Distribuição dos desfechos segundo classificação da idade.

| Age_less_16_years/Outcome | Negative | Positive |
|---------------------------|----------|----------|
| No                        |   406    |   452    |
| Yes                       |   61     |   81     |


Inicialmente não é possível concluir muito apenas observando as tabelas, então é útil calcular as estimações pontuais e intervalos de confiança para as diferenças de proporção de doentes entre as classes de cada variável. Os resultados estão apresentados na tabela 8.


Tabela 8: Resultados para diferenças de proporções.

| Variável          | Diferença na proporção de doentes | Estimação pontual | IC (95%)          |
|-------------------|-----------------------------------|-------------------|-------------------|
| Gender            | Female - Male                     | 0.006848          | [-0.055,  0.069]  |
| AreaType          | Undeveloped - Developed           | 0.040132          | [-0.021,  0.102]  |
| Age_less_16_years | No - Yes                          | -0.04361          | [-0.132,  0.044]  |

Todos os intervalos de confiança, a um nível de 95% contém o valor 0. Dessa forma, assumimos que não há diferença nas proporções de doentes entre as classes das variáveis `Gender`, `AreaType` e `Age_less_16_years`. As estimações pontuais também foram bem próximas de zero, o que reafirma que as diferenças são fruto da aleatoriedade, não das classes.

Uma medida importante para verificar se há associação entre a ocorrência de doença e as variáveis binárias mencionadas é a razão de chance. suas estimativas pontuais e intervalos de confiança calculados a partir da exponencial dos logs das razões de chance constam na tabela 9.


Tabela 9: Resultados para razões de chances.

| Variável          | Razão das chances de apresentar dengue | Estimação pontual | IC (95%)          |
|-------------------|----------------------------------------|-------------------|-------------------|
| Gender            | Male/Female                            | 0.973             | [ 0.759 , 1.248 ] |
| AreaType          | Developed/Undeveloped                  | 0.851             | [ 0.664 , 1.091 ] |
| Age_less_16_years | Yes/No                                 | 1.19              | [ 0.834 , 1.707 ] |

Visto que todos os intervalos de confiança para a razão de chance contém 1, pode-se afirmar que não há diferença causada pelas classes entre as chances de ter dengue. As estimações pontuais também estão bem próximas de 1, reforçando essa ideia.

## Inferência para Tabelas de Contigência
+ Teste Gama de Goodman e Kruskal (usar variáveis idade e areatype) Aline
+ Teste Tau de Kendall e Tau-b de Kendall (usar variáveis idade e areatype) Aline
+ Cochran-Armitage para tendência linear (usar variáveis idade e areatype) Aline

## Associação em Tabelas de Contigência
+ Teste de Breslow Day Aline
+ Se homogeneidade aceita: teste de Mantel-Haenszel - falar se ocorre ou não o paradoxo de simpason Aline
+ Teste qui-quadrado Aline
+ Teste de razão de verossimilhança Aline
+ Teste de homogeneidade Aline
+ Teste de simetria (testar simetria entre IgG, IgM, e NS1?  ou de outras variáveis?) Aline
+ Teste de homogeneidade marginal Aline


## Regressão Logística
+ Testar opções de modelos de regressão logística Mariana
+ Selecionar baseado no AIC Mariana
+ Interpretação do modelo Mariana

# Conclusão

# Apêndice

Códigos utilizados em R.


``` r
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

write.csv(dados, "dados_tratados.csv")

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
```

