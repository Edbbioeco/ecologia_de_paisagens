# Pacotes ----

library(readxl)

library(tidyverse)

library(reshape2)

library(nlme)

library(performance)

# Dados ----

## Riqueza ----

### Importando ----

riqueza <- readxl::read_xlsx("valores_riqueza.xlsx")

### Visualizando ----

riqueza %>% dplyr::glimpse()

riqueza

## Composição ----

### Importando ----

matriz_trat <- readxl::read_xlsx("matriz_trat.xlsx")

### Visualizando ----

matriz_trat %>% dplyr::glimpse()

matriz_trat

# Modelos linear Riqueza ----

## Multicolinearidade ----

riqueza_cor <- riqueza %>%
  dplyr::select(-c(1, 9)) %>%
  cor(method = "spearman")

riqueza_cor[upper.tri(riqueza_cor)] <- NA

riqueza_cor %>%
  reshape2::melt() %>%
  tidyr::drop_na() %>%
  dplyr::mutate(igual = dplyr::case_when(Var1 == Var2 ~ "sim",
                                         .default = "Não"),
                value = value %>% round(2)) %>%
  dplyr::filter(igual == "Não") %>%
  dplyr::select(-igual) %>%
  ggplot(aes(Var1, Var2, fill = value, label = value)) +
  geom_tile(color = "black", linewidth = 1) +
  geom_text() +
  coord_equal() +
  scale_fill_gradientn(colours = c(viridis::viridis(n = 10) %>% rev(), viridis::viridis(n = 10)),
                       limits = c(-1, 1),
                       breaks = seq(-1, 1, 0.2)) +
  labs(x = NULL,
       y = NULL) +
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5,
                                barwidth = 25,
                                barheight = 1.5,
                                frame.colour = "black",
                                ticks.colour = "black",
                                ticks.linewidth = 0.5)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "bottom")

## Modelo ----

### Tratando os nomes das variáveis ----

riqueza_trat <- riqueza

nomes_colunas_riqueza <- riqueza %>% names() %>% stringr::str_replace_all(" ", "_") %>% stringr::str_remove_all("'|%_de_")

colnames(riqueza_trat) <- nomes_colunas_riqueza

riqueza_trat %>% names()

### Criando os modelos ----

modelo_glm_poisson <- glm(Riqueza ~ Altitude_média +
                            Precipitação_média +
                            Aplitude_de_Precipitação +
                            corpos_dagua +
                            vegetação_nativa +
                            Diversidade_da_paisagem,
                          data = riqueza_trat,
                          family = poisson(link = "log"))
## Avaliando o modelo ----

modelo_glm_poisson %>% DHARMa::simulateResiduals(plot = TRUE)

modelo_glm_poisson %>% anova()

## Gráfico ----

riqueza %>%
  tidyr::pivot_longer(cols = c(`Altitude média`,
                               `Precipitação média`:`Diversidade da paisagem`),
                      names_to = "Preditor",
                      values_to = "Valor Preditor") %>%
  dplyr::mutate(Preditor = Preditor %>% forcats::fct_relevel(names(riqueza[, c(2, 4:8)]))) %>%
  ggplot(aes(`Valor Preditor`, Riqueza, color = Preditor, fill = Preditor)) +
  geom_point(size = 2.5, shape = 21, color = "black") +
  geom_smooth(data = riqueza %>%
                tidyr::pivot_longer(cols = c(`Altitude média`,
                                             `Precipitação média`:`Diversidade da paisagem`),
                                    names_to = "Preditor",
                                    values_to = "Valor Preditor") %>%
                dplyr::filter(Preditor %in% c("Altitude média",
                                              "Precipitação média",
                                              "% de vegetação nativa",
                                              "Diversidade da paisagem")) %>%
                dplyr::mutate(Preditor = Preditor %>% forcats::fct_relevel(names(riqueza[, c(2, 4:8)]))),
              method = "lm") +
  facet_wrap(~Preditor, scales = "free_x") +
  scale_fill_manual(values = c("gold", "cyan3", "lightblue", "skyblue4", "green2", "violet")) +
  scale_color_manual(values = c("gold4", "darkblue", "darkgreen", "purple4")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        strip.text = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "none")

ggsave(filename = "modelo_riqueza.png", height = 10, width = 12)

## Tabela ----

## Sumário ----

sumario <- modelo_glm_poisson %>% summary()

sumario <- sumario$coefficients %>% as.data.frame()

sumario

## Tabela das estátísticas ----

### Criando ----

riqueza_tabela <- tibble::tibble(Preditor = names(riqueza[, c(2, 4:8)]),
               `Coeficiente estimado ± Erro Padrão` = paste0(sumario$Estimate[-1] %>% round(3),
                                                             " ± ",
                                                             sumario$`Std. Error`[-1] %>% round(3)),
               `Graus de Liberdade` = paste0("1, ", 55:60 %>% rev()),
               Z = sumario$`z value`[-1] %>% round(3),
               p = sumario$`Pr(>|z|)`[-1] %>% round(3)) %>%
  dplyr::mutate(p = dplyr::case_when(p < 0.01 ~ "< 0.01",
                                     .default = p %>% as.character()) ) %>%
  flextable::flextable() %>%
  flextable::width(width = 1.5) %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::bold(part = "header")

riqueza_tabela

### Exportando ----

riqueza_tabela %>%
  flextable::save_as_docx(path = "tabela_riqueza.docx")

# Modelos linear Composição ----

## Distância na composição ----

dist <- matriz_trat %>%
  dplyr::select(5:112) %>%
  vegan::vegdist(method = "jaccard")

## db-RDA ---

### Criando o modelo ----

dbrda <- vegan::capscale(dist ~ Altitude_média +
                  Precipitação_média +
                  Aplitude_de_Precipitação +
                  corpos_dagua +
                  vegetação_nativa +
                  Diversidade_da_paisagem,
                data = riqueza_trat,
                permutations = 1000)

### Avaliando ----

dbrda %>%
  vegan::anova.cca(by = "term")

RsquareAdj(dbrda)

### Gráfico ----

variaveis <- dbrda$CCA$biplot %>%
  as.data.frame() %>%
  tibble::rownames_to_column() %>%
  dplyr::mutate(rowname = rowname %>% stringr::str_replace_all("_", " "))

variaveis

coords <- dbrda$Ybar %>%
  as.data.frame() %>%
  dplyr::mutate(Assemblage = matriz_trat$Assemblage)

coords

ggord::ggord(dbrda, ptslab = TRUE, size = 1, addsize = 3, repel = TRUE)

ggplot() +
  geom_label(data = coords, aes(Dim1, Dim2, label = Assemblage), size = 2.5, color = "black", fill = "gold") +
  geom_text(data = variaveis, aes(CAP1, CAP2, label = rowname, fontface = "bold"), color = "black", size = 5)+
  geom_segment(data = variaveis, aes(x = 0, y = 0, xend = CAP1, yend = CAP2), color = "black") +
  scale_x_continuous(limits = c(-0.725, 0.86)) +
  labs(x = "CAP 1 (39.75%)",
       y = "CAP2 (19.1%)") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        strip.text = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "none")

ggsave(filename = "modelo_composicao.png", height = 10, width = 12)

## Tabela de Estatísticas ----

### Criando ----

sumario_rda <- dbrda %>%
  vegan::anova.cca(by = "term")

composicao_tabela <- sumario_rda %>%
  as.data.frame() %>%
  tibble::rownames_to_column() %>%
  dplyr::rename("Preditor" = rowname,
                "Graus de liberdade" = Df,
                "p" = `Pr(>F)`) %>%
  dplyr::select(-3) %>%
  dplyr::filter(Preditor != "Residual") %>%
  dplyr::mutate(`Graus de liberdade` = paste0("1, ", 55:60 %>% rev()),
                Preditor = Preditor %>% stringr::str_replace_all("_", " "),
                `F` = `F` %>% round(3),
                p = dplyr::case_when(p < 0.01 ~ "< 0.01",
                                     .default = p %>% as.character())) %>%
  flextable::flextable() %>%
  flextable::width(width = 1.5) %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::bold(part = "header")

composicao_tabela

### Exportando ----

composicao_tabela %>%
  flextable::save_as_docx(path = "tabela_composicao.docx")

