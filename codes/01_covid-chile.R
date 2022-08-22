# Link --------------------------------------------------------------------


# https://www.youtube.com/watch?v=75jDwQBgm9Q&ab_channel=DerekCorcoran


# Packages ----------------------------------------------------------------

library(tidyverse)
library(gganimate)
library(ggrepel)
library(lubridate)



# Productos ---------------------------------------------------------------

# D19 - Casos activos por fecha de inicio de síntomas y comuna: Descripción

Activos <- readr::read_csv(
  file = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto19/CasosActivosPorComuna.csv"
  ) %>% 
  tidyr::pivot_longer(
    cols = starts_with("20"),
    names_to = "Fecha",
    values_to = "Infectados"
    ) %>% 
  mutate(
    Fecha = lubridate::ymd(Fecha)
    ) %>% 
  dplyr::filter(
    Comuna != "Total"
    ) %>% 
  mutate(
    Infectados_por_100.000 = (Infectados/Poblacion)*100000
    ) %>% 
  dplyr::filter(
    # Filtro de regiones
    Region %in% c("Valparaiso", "Antofagasta", "Biobio"),
    # Filtro de fechas
    Fecha %within% lubridate::interval(start = ymd("2020-01-01"), end = ymd("2020-12-31"))
    )

theme_set(theme_light())

ggplot(data = Activos, aes(x = Fecha, y = Infectados_por_100.000)) +
  geom_line(aes(color = Comuna)) +
  facet_wrap( ~ Region) +
  geom_hline(yintercept = 40,
             lty = 2,
             color = "dark red") +
  geom_point() +
  gganimate::transition_reveal(along = Activos$Fecha) +
  ggrepel::geom_text_repel(aes(label = Comuna)) +
  theme(legend.position = "none")

