# Pacotes ----

library(gert)

# listando arquivos ----

gert::git_status() |>
  as.data.frame() |>
  dplyr::filter(file |> stringr::str_detect(".R$"))

# Adicionado arquivos ----

gert::git_add(list.files(pattern = "^settando_")) |>
  as.data.frame()

# Commitando ----

gert::git_commit("Arquivo .Rproj")

# Pushando ----

gert::git_push(remote = "origin", force = TRUE)

# Pullando ----

gert::git_pull()

# Resetando ----

gert::git_reset_soft("HEAD~1")

gert::git_reset_hard("HEAD~1")
