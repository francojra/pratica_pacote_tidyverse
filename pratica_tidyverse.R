
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

# Manipulação de dados 5 -------------------------------------------------------------------------------------------------------------------

dados_5 <- dados %>%
  select(gender, mass, height) %>%
  filter(gender %in% c("feminine", "masculine")) %>%
  filter(mass != 1358)
dados_5

# Gráficos 5 -------------------------------------------------------------------------------------------------------------------------------

massa_genero_box <- ggplot(dados_5, aes(x = gender, y = mass)) +
  geom_violin(col = "blue") +
  geom_boxplot(aes(fill = gender), width = 0.2) +
  geom_jitter()
massa_genero_box

altura_genero_box <- ggplot(dados_5, aes(x = gender, y = height)) +
  geom_violin(col = "blue") +
  geom_boxplot(aes(fill = gender), width = 0.2) +
  geom_jitter()
altura_genero_box

# Manipulação de dados 6 -------------------------------------------------------------------------------------------------------------------

dados_6 <- dados %>%
  select(species, birth_year) %>%
  filter(species %in% c("Human", "Droid", "Wookiee", "Ewok", "Cerean")) %>%
  group_by(species) %>%
  summarise(mean(birth_year, na.rm = TRUE))
dados_6
  
dados_6 <- dados %>%
  select(species, birth_year) %>%
  filter(species %in% c("Human", "Droid", "Wookiee", "Ewok", "Cerean")) %>%
  group_by(species) %>%
  summarise_all(list(mean), na.rm = TRUE)
dados_6

# Gráfico 6 -------------------------------------------------------------------------------------------------------------------------------

ggplot(dados_6, aes(x = species, y = birth_year)) +
  geom_col(aes(fill = species)) +
  scale_x_discrete(limits = c("Wookiee", "Cerean", "Human", "Droid", "Ewok"))

# Manipulação de dados 7 -------------------------------------------------------------------------------------------------------------------

dados_7 <- dados %>%
  select(height, mass, birth_year, skin_color) %>%
  filter(skin_color %in% c("green", "red", "blue", "brown", "dark")) %>%
  group_by(skin_color) %>%
  summarise_at(c("height", "mass", "birth_year"), list(mean), na.rm = TRUE)
dados_7

# Gráficos 7 -------------------------------------------------------------------------------------------------------------------------------

altura_pele <- ggplot(dados_7, aes(x = skin_color, y = height)) +
  geom_col(fill = "#cab2d6") +
  scale_x_discrete(limits = c("green", "brown", "red", "dark", "blue")) +
  theme_light()
altura_pele

massa_pele <- ggplot(dados_7, aes(x = skin_color, y = mass)) +
  geom_col(fill = "#cab2d6") +
  scale_x_discrete(limits = c("blue", "dark", "green", "red", "brown")) +
  theme_light()
massa_pele

idade_pele <- ggplot(dados_7, aes(x = skin_color, y = birth_year)) +
  geom_col(fill = "#cab2d6") +
  scale_x_discrete(limits = c("brown", "blue", "dark", "red", "green")) +
  theme_light()
idade_pele
