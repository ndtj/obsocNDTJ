
# read --------------------------------------------------------------------


da_cjpg <- readr::read_rds("data-raw/da_cjpg.rds")
my_email <- "julio.trecenti@gmail.com"
googlesheets4::gs4_auth(my_email)
googledrive::drive_auth(my_email)


# prepare -----------------------------------------------------------------

set.seed(137)
da_cjpg_sample <- da_cjpg |>
  dplyr::select(id = n_processo, vara) |>
  dplyr::distinct(id, .keep_all = TRUE) |>
  dplyr::slice_sample(n = 20)

emails <- "1fcT8ILFCA2n7uAtyWzCsnzthL-WKjTwFbaFGC-dlPPk" |>
  googlesheets4::read_sheet() |>
  with(email) |>
  tibble::enframe("pessoa_id", "email") |>
  dplyr::mutate(pessoa_id = as.character(pessoa_id))

planilhas <- c(
  socios = "https://docs.google.com/spreadsheets/d/14ycAiGlIRhmcwwtCSLM_GT0OhVKzzsfnyc0VP8HOV4I/edit#gid=0",
  partes = "https://docs.google.com/spreadsheets/d/14LKszYzlDqe6N-O0PvH7ymC5AYbzACPP4ZywZ5c6Y18/edit#gid=0",
  balanco = "https://docs.google.com/spreadsheets/d/1XPkhSz1pDW8_hp-Gcef7rgVj8G_DYpmje5Bp7i3tOLk/edit#gid=0"
) |>
  tibble::enframe("planilha_id", "link")

aux_cjpg_emails <- da_cjpg_sample |>
  dplyr::mutate(email = list(emails), planilha = list(planilhas)) |>
  tidyr::unnest(email) |>
  tidyr::unnest(planilha)

aux_split <- aux_cjpg_emails |>
  dplyr::group_split(id, pessoa_id, planilha_id)


# criar planilhas auxiliares ----------------------------------------------

purrr::walk(aux_split, criar_tabela_classificacao)

# links_pre_teste ---------------------------------------------------------

# Passo 8: montar planilha de classificação ------------------------------------


aux_planilhas_gsheets <- "https://drive.google.com/drive/u/2/folders/1e_tQmBq94gPx6lXv2jO7ieF49QTI20cK" |>
  googledrive::as_id() |>
  googledrive::drive_ls() |>
  dplyr::transmute(
    id = name,
    link = purrr::map_chr(drive_resource, "webViewLink")
  ) |>
  tidyr::separate(id, c("id", "planilha", "pessoa_id"), sep = "_") |>
  tidyr::pivot_wider(
    names_from = planilha,
    values_from = link,
    names_prefix = "planilha_"
  )

fmt <- function(.x, col) {
  googlesheets4::gs4_formula(glue::glue('=HYPERLINK("{.x}","{col}")'))
}

links_classificacao <- aux_cjpg_emails |>
  dplyr::distinct(id, vara, pessoa_id, email) |>
  dplyr::inner_join(aux_planilhas_gsheets, c("id", "pessoa_id")) |>
  dplyr::group_by(id, pessoa_id) |>
  dplyr::mutate(link = link_classificacao(dplyr::cur_data_all())) |>
  dplyr::ungroup() |>
  dplyr::transmute(
    id,
    email = email,
    link_classificacao = link,
    link_socios = planilha_socios,
    link_partes = planilha_partes,
    link_balanco = planilha_balanco
  ) |>
  dplyr::slice_sample(prop = 1) |>
  dplyr::mutate(dplyr::across(
    dplyr::contains("link"),
    ~fmt(.x, dplyr::cur_column())
  )) |>
  dplyr::mutate(status = "", .after = email) |>
  dplyr::arrange(email)

googlesheets4::write_sheet(
  links_classificacao,
  "https://docs.google.com/spreadsheets/d/1927irVSx8-nWvprGNqDp00Hf3R9OKY9DXU29Akd4Mho/edit#gid=0",
  sheet = "Sheet1"
)

