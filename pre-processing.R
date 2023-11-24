# Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)

# Get the Data
dados <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

# Remove columns
dados <- subset(dados, select = - c(owner, farm_name, lot_number, mill, 
                                    ico_number, company, producer, owner_1))

# Detail columns
detail <- dados  %>%
  lapply(type_sum) %>%
  as_tibble() %>%
  pivot_longer(cols = 1:ncol(dados),
               names_to = "Coluna",
               values_to = "Tipo") %>%
  inner_join(
    dados %>%
      summarise(across(everything(), ~sum(is.na(.)))) %>%
      pivot_longer(cols = 1:ncol(dados),
                   names_to = "Coluna",
                   values_to = "Total NA")
  )

# Handling nulls

#altitude_mean_meters é a média das colunas altitude_low_meters e altitude_high_meters.
# Boxplot para verificar outliers
boxplot(dados$altitude_mean_meters,
        names = c('Mean'), 
        col = c('blue'),  
        main = 'Boxplot', 
        ylab = 'Altitude (meters)')     

# Calcula estatísticas desconsiderando NAs
estatisticas <- summary(na.omit(dados$altitude_mean_meters))
print(estatisticas)

#decisao: manter apenas altitude_mean_meters e substituir nulos pela mediana
dados <- subset(dados, select =  -c(altitude_low_meters, altitude_high_meters))
dados$altitude_mean_meters[is.na(dados$altitude_mean_meters)] <- estatisticas["Median"]

#A coluna altitude exibe uma faixa que contem o valor da altitude_mean_meters
dados[c('altitude', 'altitude_mean_meters')] %>% unique() %>% head(10) %>% print()

#decisao: remover a coluna 'altitude'
dados <- subset(dados, select = -altitude)

#A maioria dos dados são da cor 'Green', e as outras são variacoes de verde
dados$color %>% table() %>% print()

#decisao: remover a coluna 'color'
dados <- subset(dados, select = -color)

#quakers possui apenas 1 valor nulo
unique(dados$quakers) %>% print()

#decisao: substituir pelo valor medio arredondado para inteiro
dados$quakers[is.na(dados$quakers)] <- mean(na.omit(dados$quakers)) %>% round()

#country_of_origin tem apenas 1 valor nulo, mas vamos fazer uma análise por país
#decisao: remover observacao
dados <- subset(dados, !is.na(country_of_origin))

#a informacao da coluna region é mais granular que a coluna pais, abaixo podemos
#observar que há muito ruído como 'test', 'mmm'
dados[c('country_of_origin', 'region')] %>% subset(country_of_origin == "Brazil") %>% unique() %>% print()

#decisao: remover coluna region
dados <- subset(dados, select = -region)

#a coluna harvest_year precisa de tratamento
dados$harvest_year_aux <- dados$harvest_year
unique(dados$harvest_year)

# Remove qualquer caractere que esteja antes de '-' ou '/'
dados$harvest_year <- gsub(".+[/\\-]", "", dados$harvest_year)
# Remove todos os caracteres que não formam uma sequência de 4 números
dados$harvest_year <- gsub("[^0-9]{4}", "", dados$harvest_year)
# Mantém apenas os 4 caracteres que começam com '20'
dados$harvest_year <- gsub(".*(20\\d{2}).*", "\\1", dados$harvest_year)
# Substitui por NA as observações que contêm caracteres não numéricos
dados$harvest_year <- gsub("[^0-9]", "", dados$harvest_year)
dados$harvest_year[dados$harvest_year == ""] <- NA
# Substitui por NA as observações que são iguais a "10" ou "09"
dados$harvest_year <- ifelse(dados$harvest_year %in% c("10", "09"), NA, dados$harvest_year)

#compara dados anteriores com dados tratados
dados[c('harvest_year', 'harvest_year_aux')] %>% unique() %>% print(n = 50)
dados <- subset(dados, select = -harvest_year_aux)

#decisao: substitui valores nulos pela media arredondada para inteiro
dados$harvest_year <- as.numeric(dados$harvest_year)
dados$harvest_year[is.na(dados$harvest_year)] <- mean(na.omit(dados$harvest_year)) %>% round()

#Para coluna 'variety' e 'processing_method' temos algumas observacoes marcadas como 'Other'
table(dados$variety) %>% print()
table(dados$processing_method) %>% print()

#decisao: substituir NA por 'Other'
dados$variety[is.na(dados$variety)] <- "Other"
dados$processing_method[is.na(dados$processing_method)] <- "Other"
