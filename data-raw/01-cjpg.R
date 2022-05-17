# atualização dos dados: 2013-2021 ----------------------------------------

# download
quebras_anos <- seq(as.Date("2013-01-01"), as.Date("2022-01-01"), "1 year")
q1 <- quebras_anos[-length(quebras_anos)]
q2 <- quebras_anos[-1] - 1

purrr::walk2(q1, q2, ~{
  lex::tjsp_cjpg_download(
    busca = 'dissolução E sociedade',
    dir = stringr::str_glue("data-raw/atualizacao/cjpg/{lubridate::year(.x)}"),
    data_ini = .x, data_fim = .y
  )
})

# parse
arqs <- fs::dir_ls(
  "data-raw/atualizacao/cjpg",
  recurse = TRUE,
  type = "file",
  regexp = "search",
  invert = TRUE
)

progressr::with_progress({
  aux_cjpg <- lex::pvec(arqs, lex::tjsp_cjpg_parse)
})

readr::write_rds(aux_cjpg, "data-raw/atualizacao/aux_cjpg.rds")

da_cjpg <- aux_cjpg |>
  purrr::map_dfr("result") |>
  dplyr::mutate(data_de_disponibilizacao = lubridate::dmy(data_de_disponibilizacao)) |>
  dplyr::filter(foro == "Foro Central Cível")

readr::write_rds(da_cjpg, "data-raw/atualizacao/da_cjpg.rds")

da_cjpg_sem_txt <- da_cjpg |>
  dplyr::select(-resumo)

writexl::write_xlsx(da_cjpg_sem_txt, "data-raw/atualizacao/da_cjpg_sem_txt.xlsx")


# visualizacao simples ----------------------------------------------------


da_cjpg_sem_txt |>
  dplyr::mutate(ano = as.character(lubridate::year(data_de_disponibilizacao))) |>
  dplyr::count(ano) |>
  ggplot2::ggplot(ggplot2::aes(ano, n)) +
  ggplot2::geom_col(fill = viridis::viridis(1, 1, .2, .8)) +
  ggplot2::theme_minimal(16) +
  ggplot2::scale_y_continuous(breaks = 0:10*100, limits = c(0, 600)) +
  ggplot2::labs(
    x = "Ano", y = "Decisões",
    title = "Sentenças Envolvendo Dissolução de Sociedades",
    subtitle = "Foro Central Cível - TJSP",
    caption = glue::glue("Consulta: 'dissolução E sociedade'\nTotal: {nrow(da_cjpg_sem_txt)} casos")
  )
