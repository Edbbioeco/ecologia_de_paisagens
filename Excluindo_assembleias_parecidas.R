# Pacotes ----

library(geobr)

library(tidyverse)

library(sf)

library(readxl)

library(ggsflabel)

library(fields)

library(dbscan)

# Dados ----

## Grade ----

### Importando ----

grade_cep <- sf::st_read("grade_cep.shp")

### Visualizando ----

grade_cep

grade_cep %>%
  ggplot() +
  geom_sf(color = "black", fill = "green4")

## Registros ----

### Importando ----

matriz <- readxl::read_xlsx("matriz.xlsx")

### Transformando em shapefile ----

matriz_shp <- matriz %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = grade_cep %>% sf::st_crs())

### Visualizando ----

matriz

matriz_shp

ggplot() +
  geom_sf(data = grade_cep, color = "black", fill = "green4") +
  geom_sf(data = matriz_shp)

### Excluindo as assembleias com menos de 10 espécies ----

assembleias <- matriz %>%
  tidyr::pivot_longer(cols = 5:176,
                      names_to = "Species",
                      values_to = "Presence") %>%
  dplyr::filter(Presence == 1) %>%
  dplyr::summarise(`Número de Espécies` = Species %>% dplyr::n_distinct(),
                   .by = Assemblage) %>%
  dplyr::filter(`Número de Espécies` <= 10) %>%
  dplyr::pull(Assemblage)

assembleias

matriz_shp_trat <- matriz_shp %>%
  dplyr::filter(!Assemblage %in% assembleias)

ggplot() +
  geom_sf(data = caa, color = "black", fill = "gold") +
  ggsflabel::geom_sf_label_repel(data = matriz_shp_trat, aes(label = Assemblage))

matriz_trat <- matriz %>%
  dplyr::filter(!Assemblage %in% assembleias)

matriz_trat

# Clusterizando por distância < 10km ----

## Matriz de distância geográfica ----

dist_matrix <- matriz_trat %>%
  dplyr::select(Longitude:Latitude) %>%
  as.data.frame %>%
  fields::rdist.earth(miles = FALSE) %>%
  as.dist()

dist_matrix

## Calculando os clusters ----

dbscan_result <- dbscan::dbscan(dist_matrix,
                                eps = 10,
                                minPts = 2)

dbscan_result

## Criar um data frame de identificação dos clusters ----

clusters <- tibble::tibble(Assemblage = matriz_trat$Assemblage,
                           Cluster = dplyr::if_else(dbscan_result$cluster == 0,
                                                    "Sem Cluster",
                                                    paste0("Cluster ", dbscan_result$cluster))
)

clusters

## Adicionando a coluna de Cluster à matriz original ----

matriz_trat_2 <- matriz_trat %>%
  dplyr::left_join(clusters,
                   by = "Assemblage") %>%
  dplyr::relocate(Cluster, .after = Source)

matriz_trat_2

## Checando cada Cluster ----

### Criando uma função ----

checando_Clusters <- function(x){

  message(stringr::str_glue("# Avaliação para o {x}"))

  message("## Matriz")

  matriz_trat_2 %>%
    dplyr::filter(Cluster == x) %>%
    dplyr::select(1, 5:177) %>%
    dplyr::select(dplyr::where(~ any(. != 0))) %>%
    as.data.frame() %>%
    print()

  message("## Número de espécies")

  matriz_trat_2 %>%
    dplyr::filter(Cluster == x) %>%
    dplyr::select(1, 5:177) %>%
    dplyr::select(dplyr::where(~ any(. != 0))) %>%
    dplyr::rename("rowname" = Assemblage) %>%
    tibble::column_to_rownames() %>%
    as.data.frame() %>%
    dplyr::select_if(is.numeric) %>%
    vegan::specnumber() %>%
    print()

  message("## Dissimilaridade")

  matriz_trat_2 %>%
    dplyr::filter(Cluster == x) %>%
    dplyr::select(1, 5:177) %>%
    dplyr::select(dplyr::where(~ any(. != 0))) %>%
    dplyr::rename("rowname" = Assemblage) %>%
    tibble::column_to_rownames() %>%
    as.data.frame() %>%
    dplyr::select_if(is.numeric) %>%
    vegan::vegdist(method = "jaccard") %>%
    print()

}

### Checando ----

purrr::walk(matriz_trat_2 %>%
              dplyr::filter(Cluster != "Sem Cluster") %>%
              dplyr::pull(Cluster) %>%
              unique(),
            checando_Clusters)

### Avaliação ----

# Cluster 1: manter a Assembleia Assemblage 15969 e remover Assemblage 15613
# Cluster 2: manter a Assembleia Assemblage 20456 e remover Assemblage 20271
# Cluster 3: manter a Assemblage 21243 e remover a Assemblage 21432
# Cluster 4: manter a Assemblage 25520 e remover as Assemblage 25181 e Assemblage 25691
# Cluster 5: manter a Assemblage 25528 e remover a Assemblage 25697
# Cluster 6: manter a Assemblage 26796 e remover a Assemblage 26797
# Cluster 7: manter a Assemblage 30487 e remover a Assemblage 30628
# Cluster 8: manter a Assemblage 31572 e remover a Assemblage 31433
# Cluster 9: manter a Assemblage 34593 e remover a Assemblage 34676
# Cluster 10: manter a Assemblage 35151 e remover as Assemblage 35074 e Assemblage 35152
# Cluster 11: manter a Assemblage 35226 e remover a Assemblage 35227
# Cluster 12: manter a Assemblage 36500 e remover a Assemblage 36546
# Cluster 13: manter a Assemblage 5037 e remover a Assemblage 5036
# Cluster 14: manter a Assemblage 5328 e remover a Assemblage 5329
# Cluster 15: manter a Assemblage 6348 e remover as Assemblage 6452 e Assemblage 6454
# cluster 16: manter a Assemblage 9361 e remover a ssemblage 9362

### Removendo as assembleias ----

matriz_final <- matriz_trat_2 %>%
  dplyr::filter(!Assemblage %in% paste0("Assemblage ", c(15613,
                                                         20271,
                                                         21432,
                                                         25181,
                                                         5691,
                                                         25697,
                                                         26797,
                                                         30628,
                                                         31433,
                                                         34676,
                                                         35074,
                                                         35152,
                                                         35227,
                                                         36546,
                                                         5036,
                                                         5329,
                                                         6452,
                                                         6454,
                                                         9362)))

matriz_final

### Lista de espécies -----

matriz_final %>%
  dplyr::select(6:177) %>%
  names()

### Exportando ----

matriz_final %>%
  openxlsx::write.xlsx("registros_finais.xlsx")


insertDashes <- function() {
  rstudioapi::insertText("----")
}
rstudioapi::inse
