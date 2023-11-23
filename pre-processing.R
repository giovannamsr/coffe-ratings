# Load libraries

library(tidyverse)

# Get the Data

dados <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

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
