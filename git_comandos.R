# Pacotes ----

library(gert)

# listando arquivos ----

gert::git_status() |>
  as.data.frame() |>
  dplyr::filter(file |> stringr::str_detect(".docx$"))

# Adicionado arquivos ----

gert::git_add(list.files(pattern = "git_comandos.R")) |>
  as.data.frame()

# Commitando ----

gert::git_commit("Script para comandos de Git")

# Pushando ----

gert::git_push(remote = "origin", force = TRUE)

# Pullando ----

gert::git_pull()

# Resetando ----

gert::git_reset_mixed()

gert::git_reset_soft("HEAD~1")

gert::git_reset_hard("HEAD~1")
