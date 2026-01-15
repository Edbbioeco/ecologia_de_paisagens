# Pacotes ----

library(readxl)

library(tidyverse)

library(parzer)

library(sf)

library(geodata)

library(terra)

library(tidyterra)

library(betapart)

library(reshape2)

library(hillR)

library(landscapemetrics)

library(FactoMineR)

library(sjPlot)

library(DHARMa)

library(flextable)

library(rsq)

library(vegan)

# Dados ----

## Dados das espécies ----

### Importando ----

dados_sps <- readxl::read_xlsx("anfibios_inventários.xlsx")

### Visualizando ----

dados_sps

dados_sps %>% dplyr::glimpse()

### Tratando ----

dados_sps_trat <- dados_sps %>%
  dplyr::select(-Família) %>%
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Presença,
                     values_fill = 0)

dados_sps_trat

fragmentos <- dados_sps_trat$Área

dados_sps_trat <- dados_sps_trat %>%
  dplyr::select_if(is.numeric)

rownames(dados_sps_trat) <- fragmentos

dados_sps_trat

## Coordenadas geográficas ----

### Importando ----

coords <- readxl::read_xlsx("anfibios_inventários.xlsx",
                               sheet = 2)

### Visualizando ----

coords

coords %>% dplyr::glimpse()

### Tratando ----

coords_trat <- coords %>%
  dplyr::mutate(Longitude = Longitude %>% parzer::parse_lon(),
                Latitude = Latitude %>% parzer::parse_lat())

coords_trat

## Shapefiles ----

#### Visualizando ----

### Centro de Endemismo Pernambuco ----

#### Importando ----

estados <- sf::st_read("estados.shp") %>%
  dplyr::filter(abbrv_s %in% c("AL", "PE", "PB", "RN"))

mata_atlantica <- sf::st_read("biomas.shp") %>%
  dplyr::filter(name_biome == "Mata Atlântica")

#### Visualizando ----

estados

estados %>%
  ggplot() +
  geom_sf()

mata_atlantica

mata_atlantica %>%
  ggplot() +
  geom_sf()

#### Tratando ----

cep <- estados %>%
  sf::st_intersection(mata_atlantica) %>%
  sf::st_buffer(0) %>%
  sf::st_union() %>%
  sf::st_sf()

cep

cep %>%
  ggplot() +
  geom_sf()

## Rasters ----

### Raster de cobertura do solo ----

#### Importando ----

cobertura <- terra::rast("brasil_coverage_2023.tif")

#### Visualizando ----

cobertura %>% plot()

#### Tratando ----

cep <- cep %>%
  sf::st_transform(crs = cobertura %>% sf::st_crs())

cobertura_cortado <- cobertura %>%
  terra::crop(cep) %>%
  terra::mask(cep)

cobertura_cortado

cobertura_cortado %>% plot()

### Variáveis bioclimáticas ----

#### Importando ----

bioclim <- geodata::worldclim_country(country = "Brazil",
                                      var = "bio",
                                      res = 0.5,
                                      path = here::here())

#### Visualizando

bioclim %>% plot()

#### Recortando ----

cep <- cep %>%
  sf::st_transform(crs = bioclim %>% sf::st_crs())

bioclim_cortado <- bioclim[[c(9, 16)]] %>%
  terra::crop(cep) %>%
  terra::mask(cep)

bioclim_cortado

bioclim_cortado %>% plot()

### Imagens de satélite ----

### NDVI ----

#### Importando ----

ndvi <- terra::rast("ndvi.tif")

#### Visualizando ----

ndvi

ggplot() +
  tidyterra::geom_spatraster(data = ndvi) +
  geom_sf(data = cep, color = "black", fill = "transparent") +
  scale_fill_viridis_c(option = "turbo",
                       na.value = "transparent")

# Análises de diversidade ----

## Riqueza ----

riqueza <- dados_sps_trat %>%
  vegan::specnumber()

riqueza

## Composição ----

### Calculando ----

comp <- dados_sps_trat %>%
 betapart::beta.pair(index.family = "jaccard")

comp

### Gráfico ----

#### Sorensen ----

sorensen <- comp$beta.jac %>%
  as.matrix()

sorensen[upper.tri(sorensen)] <- NA

sorensen_vetor <- sorensen %>%
  reshape2::melt() %>%
  tidyr::drop_na() %>%
  dplyr::mutate(tipo = "Jaccard",
                Igual = dplyr::case_when(Var1 == Var2 ~ "Sim",
                                         .default = "Não")) %>%
  dplyr::filter(Igual != "Sim") %>%
  dplyr::select(-Igual)

sorensen_vetor

#### Substituição ----

tur <- comp$beta.jtu %>%
  as.matrix()

tur[upper.tri(tur)] <- NA

tur_vetor <- tur %>%
  reshape2::melt() %>%
  tidyr::drop_na() %>%
  dplyr::mutate(tipo = "Substituição",
                Igual = dplyr::case_when(Var1 == Var2 ~ "Sim",
                                         .default = "Não")) %>%
  dplyr::filter(Igual != "Sim") %>%
  dplyr::select(-Igual)

tur_vetor

#### Aninhamento ----

sne <- comp$beta.jne %>%
  as.matrix()

sne[upper.tri(sne)] <- NA

sne_vetor <- sne %>%
  reshape2::melt() %>%
  tidyr::drop_na() %>%
  dplyr::mutate(tipo = "Aninhamento",
                Igual = dplyr::case_when(Var1 == Var2 ~ "Sim",
                                         .default = "Não")) %>%
  dplyr::filter(Igual != "Sim") %>%
  dplyr::select(-Igual)

sne_vetor

#### Graficando ----

div_beta_df <- dplyr::bind_rows(sorensen_vetor,
                                tur_vetor,
                                sne_vetor) %>%
  dplyr::mutate(tipo = tipo %>% forcats::fct_relevel(c("Jaccard",
                                                      "Substituição",
                                                      "Aninhamento"))) %>%
dplyr::rename("Índice de Diversidade Beta" = value)

div_beta_df

div_beta_df %>%
  ggplot(aes(Var1, Var2, fill = `Índice de Diversidade Beta`)) +
  geom_tile(color = "black", linewidth = 0.75) +
  coord_equal() +
  facet_wrap(~tipo) +
  scale_fill_viridis_c(option = "turbo", limits = c(0, 1)) +
  labs(x = NULL,
       y =  NULL) +
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5,
                                barwidth = 20,
                                frame.colour = "black",
                                ticks.colour ="black",
                                ticks.linewidth = 0.5)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.text.x = element_text(color = "black", size = 12, angle = 60, hjust = 1),
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 12),
        strip.text = element_text(color = "black", size = 15),
        legend.position = "top")

ggsave(filename = "diversidade_beta_anuros.png", height = 10, width = 12)

# Análise de métrica de paisagem ----

## Criando os buffers ----

buffers <- coords_trat %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = cobertura %>% terra::crs()) %>%
  sf::st_buffer(4000)

buffers

buffers %>%
  ggplot() +
  geom_sf()

## Categorias de uso do solo ----

### Recortando para os buffers ----

calculo_raster <- function(nomes_1, nomes_2){

  recortando <- cobertura_cortado %>%
    terra::mask(buffers %>% dplyr::filter(Área == nomes_1)) %>%
    terra::crop(buffers %>% dplyr::filter(Área == nomes_1))

  recortando_df <- recortando %>%
    as.data.frame(xy = TRUE) %>%
    dplyr::rename("Categoria" = brasil_coverage_2023) %>%
    dplyr::mutate(Área = nomes_2,
                  Categoria = Categoria %>% as.character()) %>%
    dplyr::relocate(Área, .before = Categoria)

  assign(paste0("cobertura_buffer_id", nomes_2), recortando_df, envir = globalenv())

}

purrr::walk2(buffers$Área, fragmentos, calculo_raster)

### Unificando os data frames ----

objetos <- ls(pattern = "^cobertura_buffer_id")

objetos <- mget(objetos)

unificado <- objetos %>%
  dplyr::bind_rows()

unificado

### Número de categorias ----

numero_categorias <- unificado %>%
  dplyr::summarise(`Número de categorias` = Categoria %>% dplyr::n_distinct(),
                   .by = Área)

numero_categorias

### Diversidade de categorias ----

#### Criando um novo dataframe ----

abundancia_categorias <- unificado %>%
  dplyr::group_by(Área, Categoria) %>%
  dplyr::summarise(Abundância = n()) %>%
  tidyr::pivot_wider(names_from = Categoria,
                     values_from = Abundância,
                     values_fill = 0) %>%
  dplyr::ungroup() %>%
  dplyr::select_if(is.numeric)

rownames(abundancia_categorias) <- fragmentos

abundancia_categorias

#### Calculando a diversidade ----

diversidade <- abundancia_categorias %>%
  hillR::hill_taxa(q = 1)

diversidade

## Variáveis Bioclimáticas ----

### Temperatura do Quarto mais seco ----

bio_medias_temp_secos <- numeric(0)

media_bio_temp_secos <- function(nomes_1){

  recortando <- bioclim_cortado[[1]] %>%
    terra::mask(buffers %>% dplyr::filter(Área == nomes_1)) %>%
    terra::crop(buffers %>% dplyr::filter(Área == nomes_1))

  resultado_bio_temp_seco <- recortando %>%
    as.data.frame(xy = TRUE) %>%
    dplyr::summarise(média = wc2.1_30s_bio_9 %>% mean(na.rm = TRUE)) %>%
    dplyr::pull(média)

  bio_medias_temp_secos <- c(resultado_bio_temp_seco)

  assign("bio_medias_temp_secos",
         c(get("bio_medias_temp_secos", envir = globalenv()), resultado_bio_temp_seco),
         envir = globalenv())

}

purrr::walk(buffers$Área, media_bio_temp_secos)

bio_medias_temp_secos

### Precipitação do quarto mais umido ----

bio_medias_prec_umido <- numeric(0)

media_bio_prec_umido <- function(nomes_1){

  recortando <- bioclim_cortado[[2]] %>%
    terra::mask(buffers %>% dplyr::filter(Área == nomes_1)) %>%
    terra::crop(buffers %>% dplyr::filter(Área == nomes_1))

  resultado_bio_prec_umido <- recortando %>%
    as.data.frame(xy = TRUE) %>%
    dplyr::summarise(média = wc2.1_30s_bio_16 %>% mean(na.rm = TRUE)) %>%
    dplyr::pull(média)

  bio_medias_prec_umido <- c(resultado_bio_prec_umido)

  assign("bio_medias_prec_umido",
         c(get("bio_medias_prec_umido", envir = globalenv()), resultado_bio_prec_umido),
         envir = globalenv())

}

purrr::walk(buffers$Área, media_bio_prec_umido)

bio_medias_prec_umido

## NDVI ----

### NDVI médio ----

ndvi_medias <- numeric(0)

media_ndvi <- function(nomes_1){

  recortando <- ndvi %>%
    terra::mask(buffers %>% dplyr::filter(Área == nomes_1)) %>%
    terra::crop(buffers %>% dplyr::filter(Área == nomes_1))

  resultado_ndvi_medio <- recortando %>%
    as.data.frame(xy = TRUE) %>%
    dplyr::summarise(média = ndvi %>% mean(na.rm = TRUE)) %>%
    dplyr::pull(média)

  ndvi_media <- c(resultado_ndvi_medio)

  assign("ndvi_medias",
         c(get("ndvi_medias", envir = globalenv()), resultado_ndvi_medio),
         envir = globalenv())

}

purrr::walk(buffers$Área, media_ndvi)

ndvi_medias

## Área de classes ----

### Criando uma lista com os rasters nos buffers ----

gerar_buffers <- function(nomes_1, nomes_2){

  recortando <- cobertura_cortado %>%
    terra::mask(buffers %>% dplyr::filter(Área == nomes_1)) %>%
    terra::crop(buffers %>% dplyr::filter(Área == nomes_1))

  assign(paste0("buffer_", nomes_2), recortando, envir = globalenv())

}

purrr::walk2(buffers$Área, fragmentos, gerar_buffers)

lista_obj_buffers <- ls(pattern = "^buffer_")

buffers_lista <- mget(lista_obj_buffers)

names(buffers_lista) <- fragmentos

buffers_lista

### Área total ----

area_total <- buffers_lista %>%
  landscapemetrics::lsm_p_area() %>%
  dplyr::group_by(layer) %>%
  dplyr::summarise(Área = value %>% sum() * 10000)

area_total

### Floresta ----

area_floresta <- buffers_lista %>%
  landscapemetrics::lsm_p_area() %>%
  dplyr::filter(class %in% c(1, 3:6, 49, 10:12, 32, 29, 50)) %>%
  dplyr::rename("Local" = layer) %>%
  dplyr::group_by(Local) %>%
  dplyr::summarise(`Área total` = value %>% sum() * 10000) %>%
  dplyr::mutate(Local = fragmentos,
                `Área relativa` = `Área total` / area_total$Área)

area_floresta

 ### Corpos d'água ----

area_corpos_dagua <- buffers_lista %>%
  landscapemetrics::lsm_p_area() %>%
  dplyr::filter(class %in% c(26, 31, 33)) %>%
  dplyr::rename("Local" = layer) %>%
  dplyr::group_by(Local) %>%
  dplyr::summarise(`Área total` = value %>% sum() * 10000) %>%
  dplyr::mutate(Local = fragmentos,
                `Área relativa` = `Área total` / area_total$Área)

area_corpos_dagua

# Tabela de Espécies registradas ----

## Criando a tabela ----

dados_sps_registradas <- dados_sps %>%
  dplyr::distinct(Espécie, Família) %>%
  dplyr::arrange(Família) %>%
  dplyr::relocate(Família, .before = Espécie) %>%
  dplyr:: mutate(Família = dplyr::if_else(row_number() == 1,
                                          Família,
                                          dplyr::if_else(Família %>% dplyr::lag() != Família,
                                                         Família,
                                                         NA_character_)))

dados_sps_registradas

## Flextable ----

### Criando o flextable ----

dados_sps_registradas_flex <- dados_sps_registradas %>%
  flextable::flextable() %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::width(width = 1.3) %>%
  flextable::font(fontname = "Arial", part = "all") %>%
  flextable::fontsize(size = 11)

dados_sps_registradas_flex

### Salvando o flextable ----

dados_sps_registradas_flex %>%
  flextable::save_as_docx(path = "tabela_especies_registradas.docx")

# Quantidade de espécies por família ----

## Criando uma tabela ----

dados_sps_familias <- dados_sps %>%
  dplyr::summarise(`Número de espécies` = n(), .by = c(Espécie, Família)) %>%
  dplyr::summarise(`Número de espécies` = n(), .by = Família) %>%
  dplyr::arrange(`Número de espécies` %>% dplyr::desc())

dados_sps_familias

## Flextable ----

### Criando o flextable ----

dados_sps_familias_flex <- dados_sps_familias %>%
  flextable::flextable() %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::width(width = 1.3) %>%
  flextable::font(fontname = "Arial", part = "all") %>%
  flextable::fontsize(size = 11)

dados_sps_familias_flex

### Salvando o flextable ----

dados_sps_familias_flex %>%
  flextable::save_as_docx(path = "tabela_familias_quantidade.docx")

## Espécies mais e menos abundantes ----

### Mais abundante ----

dados_sps %>%
  dplyr::summarise(`Número de espécies` = n(), .by = Espécie) %>%
  dplyr::arrange(`Número de espécies` %>% dplyr::desc())

### Menos abundante ----

dados_sps %>%
  dplyr::summarise(`Número de espécies` = n(), .by = Espécie) %>%
  dplyr::arrange(`Número de espécies`)

# Modelos lineares ----

## Riqueza ----

### Criando um data frame ----

df_riqueza <- tibble::tibble(Área = fragmentos,
                             Riqueza = riqueza,
                             `Número de categorias` = numero_categorias$`Número de categorias`,
                             `Diversidade de categorias` = diversidade,
                             `NDVI médio` = ndvi_medias,
                             `Área de vetação nativa` =  area_floresta$`Área total`,
                             `Área de corpos d'água` = area_corpos_dagua$`Área total`,
                             `Temp. média quart. seco` = bio_medias_temp_secos,
                             `Prec. média quart. umido` = bio_medias_prec_umido) %>%
  as.data.frame()

df_riqueza

### testando multicolinearidade ----

cor_matriz <- df_riqueza[3:9] %>%
  cor(method = "spearman") %>%
  as.matrix()

cor_matriz[upper.tri(cor_matriz)] <- NA

cor_matriz %>%
  reshape2::melt() %>%
  tidyr::drop_na() %>%
  dplyr::mutate(tipo = "Jaccard",
                Igual = dplyr::case_when(Var1 == Var2 ~ "Sim",
                                         .default = "Não")) %>%
  dplyr::filter(Igual != "Sim") %>%
  dplyr::select(-Igual) %>%
  dplyr::mutate(value = value %>% round(2)) %>%
  ggplot(aes(Var1, Var2, fill = value, label = value)) +
  geom_tile(color = "black", linewidth = 0.75) +
  coord_equal() +
  geom_text(size = 5, color = "black") +
  scale_fill_viridis_c(option = "turbo",
                       breaks = seq(-1, 1, 0.2),
                       limits = c(-1, 1)) +
  labs(x = NULL,
       y =  NULL,
       fill = "Índice de Correlação de Spearman") +
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5,
                                barheight = 2,
                                barwidth = 30,
                                frame.colour = "black",
                                ticks.colour ="black",
                                ticks.linewidth = 0.5)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15, angle = 60, hjust = 1),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        strip.text = element_text(color = "black", size = 15),
        legend.position = "top")

ggsave("correlacao_riqueza.png", height = 10, width = 12)

### Reduzindo multicolinearidade ----

pca <- df_riqueza[c(3:4, 7)] %>%
  vegan::decostand(method = "standardize") %>%
  FactoMineR::PCA(graph = TRUE)

pca$eig

pca$var

pc1 <- pca$ind$coord[, 1]

pc1

df_riqueza_pc1 <- df_riqueza %>%
  dplyr::mutate(PC1 = pc1)

df_riqueza_pc1

### Criando os modelos GLM ----

modelo_riqueza_glm <- glm(Riqueza ~ PC1 + `NDVI médio` + `Área de vetação nativa` + `Temp. média quart. seco` + `Prec. média quart. umido`,
                          data = df_riqueza_pc1,
                          family = poisson(link = "log"))

modelo_riqueza_glm %>% DHARMa::simulateResiduals(plot = TRUE)

### Avaliando o modelo ----

modelo_riqueza_glm %>%
  summary()

r2 <- modelo_riqueza_glm %>%
  rsq::rsq(adj = TRUE)

r2

r2_parcial <- modelo_riqueza_glm %>%
  rsq::rsq.partial(adj = TRUE)

r2_parcial

### Tabela com as estatísticas ----

#### Criando a tabela ----

sumariao_glm <- modelo_riqueza_glm %>%
  summary()

sumariao_glm$df.residual

tabela_glm <- tibble::tibble(Preditor = c("PC1",
                                          "NDVI médio",
                                          "% de cobertura de vetação nativa",
                                          "Temp. média quart. seco",
                                          "Prec. média quart. umido"
                                          )) %>%
  dplyr::bind_cols(sumariao_glm$coefficients[-1, ]) %>%
  dplyr::mutate(Estimate = Estimate %>% round(3),
                `Std. Error` = `Std. Error` %>% round(3),
                `Graus de liberdade` = "5, 7",
                `z value` = `z value` %>% round(3),
                `Pr(>|z|)` = `Pr(>|z|)` %>% round(3)) %>%
  tidyr::unite(col = "Coeficiente estimado ± erro padrão",
               2:3,
               sep = " ± ") %>%
  dplyr::rename("z" = `z value`,
                "p" = `Pr(>|z|)`) %>%
  dplyr::mutate(p = dplyr::case_when(p < 0.01 ~ "p < 0.01",
                                     .default = p %>% as.character())) %>%
  dplyr::relocate(`Graus de liberdade`, .before = p)

tabela_glm

#### Criando o flextable ----

tabela_glm_flex <- tabela_glm %>%
  flextable::flextable() %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::width(width = 1.3)

tabela_glm_flex

#### Salvando a tabela ----

tabela_glm_flex %>%
  flextable::save_as_docx(path = "tabela_estatisticas_riqueza.docx")

### Gráfico ----
df_riqueza_pc1[c(5:6, 8:10)]

df_riqueza_pc1 %>%
  tidyr::pivot_longer(names_to = "Variável",
                      values_to = "Valores preditores",
                      cols = c(5:6, 8:10)) %>%
  dplyr::mutate(Variável = Variável %>% forcats::fct_relevel(df_riqueza_pc1[5:10] %>% names())) %>%
  ggplot(aes(`Valores preditores`, Riqueza,
             fill = Variável,
             color = Variável)) +
  geom_point(shape = 21, size = 3.5, color = "black", show.legend = FALSE) +
  geom_smooth(data = . %>% dplyr::filter(Variável %in% c("Temp. média quart. seco")),
              method = "lm",
              se = FALSE,
              show.legend = FALSE) +
  scale_fill_manual(values = c("gold", "#49be12", "orange", "cyan4", "orangered")) +
  scale_color_manual(values = c("orange4")) +
  facet_wrap(~ Variável, scales = "free_x") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        strip.text = element_text(color = "black", size = 15))

ggsave(filename = "modelo_riqueza.png", height = 10, width = 12)

## Composição ----

### Calculando as dissimilaridades ----

#### Composição ----

distancia_comp <- sorensen_vetor %>%
  dplyr::pull(value)

distancia_comp

#### Número de categorias ----

n_cat_matriz <- df_riqueza[3] %>%
  vegan::vegdist(method = "bray") %>%
  as.matrix()

n_cat_matriz[upper.tri(n_cat_matriz)] <- NA

n_cat_dist <- n_cat_matriz %>%
  reshape2::melt() %>%
  tidyr::drop_na() %>%
  dplyr::mutate(Igual = dplyr::case_when(Var1 == Var2 ~ "Sim",
                                         .default = "Não")) %>%
  dplyr::filter(Igual != "Sim") %>%
  dplyr::select(-Igual) %>%
  dplyr::pull(value)

n_cat_dist

#### Diversidade de categorias ----

div_cat_matriz <- df_riqueza[4] %>%
  vegan::vegdist(method = "euclidean") %>%
  as.matrix()

div_cat_matriz[upper.tri(div_cat_matriz)] <- NA

div_cat_dist <- div_cat_matriz %>%
  reshape2::melt() %>%
  tidyr::drop_na() %>%
  dplyr::mutate(Igual = dplyr::case_when(Var1 == Var2 ~ "Sim",
                                         .default = "Não")) %>%
  dplyr::filter(Igual != "Sim") %>%
  dplyr::select(-Igual) %>%
  dplyr::pull(value)

div_cat_dist

#### NDVI médio ----

ndvi_medio_matriz <- df_riqueza[5] %>%
  vegan::vegdist(method = "euclidean") %>%
  as.matrix()

ndvi_medio_matriz[upper.tri(ndvi_medio_matriz)] <- NA

ndvi_medio_dist <- ndvi_medio_matriz %>%
  reshape2::melt() %>%
  tidyr::drop_na() %>%
  dplyr::mutate(Igual = dplyr::case_when(Var1 == Var2 ~ "Sim",
                                         .default = "Não")) %>%
  dplyr::filter(Igual != "Sim") %>%
  dplyr::select(-Igual) %>%
  dplyr::pull(value)

ndvi_medio_dist

#### Área de floresta ----

area_floresta_matriz <- df_riqueza[6] %>%
  vegan::vegdist(method = "euclidean") %>%
  as.matrix()

area_floresta_matriz[upper.tri(area_floresta_matriz)] <- NA

area_floresta_dist <- area_floresta_matriz %>%
  reshape2::melt() %>%
  tidyr::drop_na() %>%
  dplyr::mutate(Igual = dplyr::case_when(Var1 == Var2 ~ "Sim",
                                         .default = "Não")) %>%
  dplyr::filter(Igual != "Sim") %>%
  dplyr::select(-Igual) %>%
  dplyr::pull(value)

area_floresta_dist

#### Área de corpos d'água ----

area_agua_matriz <- df_riqueza[7] %>%
  vegan::vegdist(method = "euclidean") %>%
  as.matrix()

area_agua_matriz[upper.tri(area_agua_matriz)] <- NA

area_agua_dist <- area_agua_matriz %>%
  reshape2::melt() %>%
  tidyr::drop_na() %>%
  dplyr::mutate(Igual = dplyr::case_when(Var1 == Var2 ~ "Sim",
                                         .default = "Não")) %>%
  dplyr::filter(Igual != "Sim") %>%
  dplyr::select(-Igual) %>%
  dplyr::pull(value)

area_agua_dist

#### Temperatura média do quarto mais seco ----

temp_seco_matriz <- df_riqueza[8] %>%
  vegan::vegdist(method = "euclidean") %>%
  as.matrix()

temp_seco_matriz[upper.tri(temp_seco_matriz)] <- NA

temp_seco_dist <- temp_seco_matriz %>%
  reshape2::melt() %>%
  tidyr::drop_na() %>%
  dplyr::mutate(Igual = dplyr::case_when(Var1 == Var2 ~ "Sim",
                                         .default = "Não")) %>%
  dplyr::filter(Igual != "Sim") %>%
  dplyr::select(-Igual) %>%
  dplyr::pull(value)

temp_seco_dist

#### Precipitação média do quarto mais umido ----

prec_umido_matriz <- df_riqueza[9] %>%
  vegan::vegdist(method = "euclidean") %>%
  as.matrix()

prec_umido_matriz[upper.tri(prec_umido_matriz)] <- NA

prec_umido_dist <- prec_umido_matriz %>%
  reshape2::melt() %>%
  tidyr::drop_na() %>%
  dplyr::mutate(Igual = dplyr::case_when(Var1 == Var2 ~ "Sim",
                                         .default = "Não")) %>%
  dplyr::filter(Igual != "Sim") %>%
  dplyr::select(-Igual) %>%
  dplyr::pull(value)

prec_umido_dist

### Criando um novo dataframe ----

df_comp <- tibble::tibble(`Dissimilaridade de Composição` = distancia_comp,
                          `Dissimilaridade do número de categorias` = n_cat_dist,
                          `Dissimilaridade da diversidade de categorias` = div_cat_dist,
                          `Dissimilaridade do NDVI médios` = ndvi_medio_dist,
                          `Dissimilaridade da área de floresta` = area_floresta_dist,
                          `Dissimilaridade da área de corpos d'água` = area_agua_dist,
                          `Dissimilaridade de Temp. seco` = temp_seco_dist,
                          `Dissimilaridade de Prec. úmido` = prec_umido_dist)

df_comp %>% dplyr::glimpse()

### Testando multicolineaidade ----

cor_matriz_comp <- df_comp[2:8] %>%
  cor(method = "spearman") %>%
  as.matrix()

cor_matriz_comp[upper.tri(cor_matriz_comp)] <- NA

cor_matriz_comp %>%
  reshape2::melt() %>%
  tidyr::drop_na() %>%
  dplyr::mutate(Igual = dplyr::case_when(Var1 == Var2 ~ "Sim",
                                         .default = "Não")) %>%
  dplyr::filter(Igual != "Sim") %>%
  dplyr::select(-Igual) %>%
  dplyr::mutate(value = value %>% round(2)) %>%
  ggplot(aes(Var1, Var2, fill = value, label = value)) +
  geom_tile(color = "black", linewidth = 0.75) +
  coord_equal() +
  geom_text(size = 5) +
  scale_fill_viridis_c(option = "turbo",
                       breaks = seq(-1, 1, 0.2),
                       limits = c(-1, 1)) +
  labs(x = NULL,
       y =  NULL,
       fill = "Índice de Correlação de Spearman") +
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5,
                                barheight = 2,
                                barwidth = 30,
                                frame.colour = "black",
                                ticks.colour ="black",
                                ticks.linewidth = 0.5)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15, angle = 60, hjust = 1),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        strip.text = element_text(color = "black", size = 15),
        legend.position = "top")

ggsave("correlacao_composicao.png", height = 10, width = 12)

### Criando o modelo GLM ----

modelo_comp <- glmmTMB::glmmTMB(`Dissimilaridade de Composição` ~ `Dissimilaridade do número de categorias` +
                                  `Dissimilaridade da diversidade de categorias` +
                                  `Dissimilaridade do NDVI médios` +
                                  `Dissimilaridade da área de floresta` +
                                  `Dissimilaridade da área de corpos d'água` +
                                  `Dissimilaridade de Temp. seco` +
                                  `Dissimilaridade de Prec. úmido`,
                   family = glmmTMB::beta_family(),
                    data = df_comp)

### Avaliando os pressupostos do modelo ----

modelo_comp %>% DHARMa::simulateResiduals(plot = TRUE)

### Avaliando o modelo ----

modelo_comp %>%
  summary()

### Tabela com as estatísticas ----

#### Criando a tabela ----

sumariao_glm_comp <- modelo_comp %>%
  summary()

sumariao_glm_comp$coefficients$cond

tabela_glm_comp <- tibble::tibble(Preditor = c("Dissimilaridade do número de categorias",
                                               "Dissimilaridade da diversidade de categorias",
                                               "Dissimilaridade do NDVI médios",
                                               "Dissimilaridade da área de floresta",
                                               "Dissimilaridade da área de corpos d'água",
                                               "Dissimilaridade de Temp. seco",
                                               "Dissimilaridade de Prec. úmido")) %>%
  dplyr::bind_cols(sumariao_glm_comp$coefficients$cond[-1, ]) %>%
  dplyr::mutate(Estimate = Estimate %>% round(5),
                `Std. Error` = `Std. Error` %>% round(4),
                `Graus de liberdade` = "7, 47",
                `z value` = `z value` %>% round(2),
                `Pr(>|z|)` = `Pr(>|z|)` %>% round(4)) %>%
  tidyr::unite(col = "Coeficiente estimado ± erro padrão",
               2:3,
               sep = " ± ") %>%
  dplyr::rename("z" = `z value`,
                "p" = `Pr(>|z|)`) %>%
  dplyr::relocate(`Graus de liberdade`, .before = p)

tabela_glm_comp

#### Criando o flextable ----

tabela_glm_comp_flex <- tabela_glm_comp %>%
  flextable::flextable() %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::width(width = 1.3) %>%
  flextable::font(fontname = "Arial", part = "all") %>%
  flextable::fontsize(size = 11)

tabela_glm_comp_flex

#### Salvando a tabela ----

tabela_glm_comp_flex %>%
  flextable::save_as_docx(path = "tabela_estatisticas_composicao.docx")

### Gráfico ----

df_comp %>%
  tidyr::pivot_longer(names_to = "Variável",
                      values_to = "Dissimilaridade da paisagem",
                      cols = c(2:8)) %>%
  dplyr::mutate(Variável = Variável %>% forcats::fct_relevel(df_comp[2:7] %>% names())) %>%
  ggplot(aes(`Dissimilaridade da paisagem`, `Dissimilaridade de Composição`,
             fill = Variável,
             color = Variável)) +
  geom_point(shape = 21, size = 3.5, color = "black", show.legend = FALSE) +
  geom_smooth(data = . %>% dplyr::filter(Variável == "Dissimilaridade do NDVI médios"),
              method = "lm",
              se = FALSE,
              show.legend = FALSE) +
  scale_fill_manual(values = c("orange",
                               "gold",
                               "#a7bc45",
                               "#49be12",
                               "lightblue4",
                               "orangered",
                               "blue")) +
  scale_color_manual(values = "green4") +
  facet_wrap(~ Variável,
             ncol = 2,
             scales = "free_x") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        strip.text = element_text(color = "black", size = 15))

ggsave(filename = "modelo_comp.png", height = 12, width = 10)


coords_trat_sf <- coords_trat %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = ndvi %>% terra::crs())

ndvi_valores <- ndvi %>%
  terra::extract(coords_trat_sf)

ndvi_valores %>%
  dplyr::mutate(riqueza) %>%
  ggplot(aes(ndvi, riqueza)) +
  geom_point()


ndvi_valores %>%
  dplyr::mutate(riqueza) -> df

glm(riqueza ~ ndvi, data = df, family = poisson(link = "log")) %>%
  summary()

bioclim_valores <- bioclim_cortado %>%
  terra::extract(coords_trat_sf)

bioclim_valores %>%
  dplyr::mutate(riqueza) %>%
  tidyr::pivot_longer(cols = c(2:3),
                      values_to = "valores",
                      names_to = "variavel") %>%
  ggplot(aes(valores, riqueza)) +
  geom_point() +
  facet_wrap(~ variavel, scales = "free_x")


bioclim_valores %>%
  dplyr::mutate(riqueza) -> df

glm(riqueza ~ wc2.1_30s_bio_9 + wc2.1_30s_bio_16, data = df, family = poisson(link = "log")) %>%
  summary()
