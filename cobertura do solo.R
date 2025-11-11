# Pacotes ----

library(sf)

library(tidyverse)

library(terra)

library(tidyterra)

library(flextable)

# Dados ----

## Shapefiles ----

### Brasil ----

#### Importando ----

br <- sf::st_read("brasil.shp")

#### Visualizando ----

br

br |>
  ggplot() +
  geom_sf()

### Regiões ----

#### Importando ----

regioes <- sf::st_read("regioes.shp")

#### Visualizando ----

regioes

regioes |>
  ggplot() +
  geom_sf()

### Pernambuco ----

#### Importando ----

pe <- sf::st_read("municipios.shp") |>
  dplyr::filter(abbrv_s == "PE")

#### Visualizando ----

pe

pe |>
  ggplot() +
  geom_sf()

### APA Aldeia Beberibe ----

#### Importando ----

apa <- sf::st_read("apa_aldeiabeberibe.shp")

#### Visualizando ----

apa

apa |>
  ggplot() +
  geom_sf()

## Raster de cobertura do solo ----

### Importando ----

cobertura <- terra::rast("brasil_coverage_2023.tif")

### Visualizando ----

cobertura |> plot()

### Recortando apenas para Pernambuco ----

#### Recortando ----

cobertura_recortado <- cobertura |>
  terra::crop(apa)|>
  terra::mask(apa)

#### Visualizando ----

cobertura_recortado

ggplot() +
  tidyterra::geom_spatraster(data = cobertura_recortado) +
  scale_fill_viridis_c(option = "turbo",
                       na.value = NA)

## Coordenadas ----

### Importando ----

coords <- readr::read_csv("pt_cam_trap2_convertido.csv")

### Visualizando ----

coords

### Transformando em shapefile ----

coords_shp <- coords |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = cobertura |> terra::crs())

coords_shp

ggplot() +
  geom_sf(data = apa) +
  geom_sf(data = coords_shp)

## Criando os buffers ----

### Criando ----

buffers <- coords_shp |>
  sf::st_buffer(dist = 1000)

### Visualizando ----

buffers

ggplot() +
  tidyterra::geom_spatraster(data = cobertura_recortado) +
  geom_sf(data = apa, color = "red", fill = "transparent", linewidth = 1) +
  geom_sf(data = buffers, color = "red", fill = "transparent", linewidth = 1) +
  scale_fill_viridis_c(option = "turbo",
                       na.value = NA)


# Análise ----

## Cortando para os buffers ----

### Cortando ----

cobertura_buffers <- cobertura_recortado |>
  terra::mask(buffers) |>
  terra::crop(buffers)

### Visualizando ----

cobertura_buffers %>% terra::classify(cbind(before = 0,
                                            after = 0))

ggplot() +
  geom_sf(data = apa, color = "red", fill = "transparent", linewidth = 1) +
  tidyterra::geom_spatraster(data = cobertura_buffers) +
  geom_sf(data = buffers, color = "red", fill = "transparent", linewidth = 1) +
  scale_fill_viridis_c(option = "turbo",
                       na.value = NA)

## Calculando ----

### Calculando as porcentagens ----

calculo_raster <- function(x){

  recortando <- cobertura_recortado |>
    terra::mask(buffers |> dplyr::filter(id == x)) |>
    terra::crop(buffers |> dplyr::filter(id == x))

  total <- recortando |>
    as.data.frame(xy = TRUE) |>
    nrow()

  recortando_df <- recortando |>
    as.data.frame(xy = TRUE) |>
    dplyr::group_by(brasil_coverage_2023) |>
    dplyr::summarise(`% de ocupação` = (n() / total) * 100) |>
    dplyr::rename("Categoria" = brasil_coverage_2023) |>
    dplyr::mutate(id = x,
                  `% de ocupação` = `% de ocupação` |> round(2)) |>
    dplyr::relocate(id, .before = Categoria)

  assign(paste0("porcentagem_id0", x), recortando_df, envir = globalenv())

}


purrr::walk(1:8, calculo_raster)

### Unindo as dataframes ----

porcentagens_unidos <- dplyr::bind_rows(porcentagem_id01,
                                        porcentagem_id02,
                                        porcentagem_id03,
                                        porcentagem_id04,
                                        porcentagem_id05,
                                        porcentagem_id06,
                                        porcentagem_id07,
                                        porcentagem_id08) |>
  dplyr::mutate(Categoria = Categoria |> as.numeric())

porcentagens_unidos

### Transformando em wide ----

porcentagens_unidos_trat <- porcentagens_unidos |>
  tidyr::pivot_wider(names_from = id,
                     values_from = `% de ocupação`,
                     values_fill = 0)

porcentagens_unidos_trat

## Tabela ----

### Criando ----

porcentagens_unidos_trat_flex <- porcentagens_unidos_trat |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all")

porcentagens_unidos_trat_flex

### Salvando ----

porcentagens_unidos_trat_flex |>
  flextable::save_as_docx(path = "tabela_categorias_uso_de_solo.docx")
