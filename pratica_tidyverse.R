
# Pacotes tidyverse - Prática -----------------------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 17/03/2022 -------------------------------------------------------------------------------------------------------------------------

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

dados <- starwars
View(dados)

dados %>%
  select(mass, hair_color) %>%
  filter(hair_color == "brown")

dados %>%
  select(height, hair_color) %>%
  filter(hair_color == "brown")

dados_1 <- dados %>%
  select(height, mass) %>%
  summarise_at(c("height", "mass"), list(max, min), na.rm = TRUE) 
dados_1

dados_2 <- dados %>%
  select(height, mass) 
dados_2

ggplot(dados_2, aes(x = height, y = mass)) +
  geom_point()

dados_2 <- dados %>%
  select(height, mass) %>%
  filter(mass != 1358) # Elimina o valor máximo (outlier que impede ver a tendência)
dados_2

ggplot(dados_2, aes(x = height, y = mass)) +
  geom_point()

dados_3 <- dados %>%
  select(height, mass, eye_color) %>%
  group_by(eye_color) %>%
  summarise_all(list(mean), na.rm = TRUE)
dados_3

