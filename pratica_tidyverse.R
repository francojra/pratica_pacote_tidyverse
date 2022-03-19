
# Pacotes tidyverse - Prática -----------------------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 17/03/2022 -------------------------------------------------------------------------------------------------------------------------

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

dados <- starwars
View(dados)

# Manipulação de dados 1 -------------------------------------------------------------------------------------------------------------------

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

# Gráfico 1 --------------------------------------------------------------------------------------------------------------------------------

ggplot(dados_2, aes(x = height, y = mass)) +
  geom_point()

# Manipulação de dados 2 -------------------------------------------------------------------------------------------------------------------

dados_2 <- dados %>%
  select(height, mass) %>%
  filter(mass != 1358) # Elimina o valor máximo (outlier que impede ver a tendência)
dados_2

# Gráfico 2 --------------------------------------------------------------------------------------------------------------------------------

ggplot(dados_2, aes(x = height, y = mass)) +
  geom_point()

# Manipulação de dados 3 -------------------------------------------------------------------------------------------------------------------

dados_3 <- dados %>%
  select(height, mass, species) %>%
  filter(species %in% c("Besalisk", "Dug", "Gungan", "Ewok", "Droid")) %>%
  group_by(species) %>%
  summarise_all(list(mean), na.rm = TRUE)
dados_3

# Gráficos 3 -------------------------------------------------------------------------------------------------------------------------------

plot_altura <- ggplot(dados_3, aes(x = species, y = height)) +
  geom_col(aes(fill = species)) +
  scale_x_discrete(limits = c("Gungan", "Besalisk", "Droid", "Dug", "Ewok")) +
  scale_fill_discrete(name = "Espécies") +
  labs(x = "Espécies", y = "Altura (cm)")
plot_altura

plot_massa <- ggplot(dados_3, aes(x = species, y = mass)) +
  geom_col(aes(fill = species)) +
  scale_x_discrete(limits = c("Besalisk", "Gungan", "Droid", "Dug", "Ewok")) +
  scale_fill_discrete(name = "Espécies") +
  labs(x = "Espécies", y = "Massa (kg)")
plot_massa

# Manipulação de dados 4 -------------------------------------------------------------------------------------------------------------------

dados_4 <- dados %>%
  select(gender, mass, height) %>%
  filter(gender %in% c("feminine", "masculine")) %>%
  group_by(gender) %>%
  summarise_all(list(mean), na.rm = TRUE)
dados_4  

# Gráficos 4 -------------------------------------------------------------------------------------------------------------------------------

massa_genero <- ggplot(dados_4, aes(x = gender, y = mass)) +
  geom_col(aes(fill = gender))
massa_genero

altura_genero <- ggplot(dados_4, aes(x = gender, y = height)) +
  geom_col(aes(fill = gender))
altura_genero
