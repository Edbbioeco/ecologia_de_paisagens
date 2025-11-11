# Pacotes ----

library(tidyverse)

library(parzer)

# Dados ----

## Importando ----

coord <- readr::read_csv("G:/Meu Drive/UFPE/projeto mestrado/Ecologia de Paisagens/pt_cam_trap2.csv")

## Visualizando ----

coord

# Tratando ----

## Convertendo ----

coord_2 <- coord |>
  dplyr::mutate(Latitude = Latitude |> parzer::parse_lat(),
                Longitude = Longitude |> parzer::parse_lon())

coord_2

## Exportando ----

coord_2 |>
  readr::write_csv("G:/Meu Drive/UFPE/projeto mestrado/Ecologia de Paisagens/pt_cam_trap2_convertido.csv")
