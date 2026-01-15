# Pacotes ----

library(usethis)

# Iniciando ----

usethis::use_git()

# configurando suário e e-mail ----

usethis::use_git_config(user.name = "Edbbioeco",
                        user.email = "edsonbbiologia@gmail.com")

# Settando repositório ----

usethis::proj_get()

usethis::use_git_remote(name = "origin",
                        url = "https://github.com/Edbbioeco/ecologia_de_paisagens.git",
                        overwrite = TRUE)

# Cobfigurar o principal branch para main ----

usethis::git_default_branch_configure(name = "main")

# Renomear o branch do master para main ----

usethis::git_default_branch_rename(from = "master", to = "main")
