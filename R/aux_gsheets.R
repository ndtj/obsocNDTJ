criar_tabela_classificacao <- function(da) {
  nm <- paste(da$id, da$planilha_id, da$pessoa_id, sep = "_")
  usethis::ui_info("{da$id[1]} --------------------------------")
  # criar
  folder <- googledrive::as_id(
    "https://drive.google.com/drive/u/2/folders/1e_tQmBq94gPx6lXv2jO7ieF49QTI20cK"
  )

  ss_new <- googledrive::drive_create(
    name = nm,
    path = folder,
    type = "spreadsheet"
  )
  # copiar
  googlesheets4::sheet_copy(
    from_ss = da$link,
    from_sheet = "Sheet1",
    to_ss = ss_new,
    to_sheet = "classificacao",
    .before = 1
  )
  googlesheets4::sheet_delete(ss_new, "Sheet1")
  ss_new$id
}

link_classificacao <- function(da) {
  q <- c(
    entry.1520516273 = da$email,
    entry.16924311 = da$id,
    entry.2143679306 = da$vara
  )
  u_base <- "https://docs.google.com/forms/d/e/1FAIpQLSc6zINZHjKZWiwYTC7qTo1WCzw7UKAqUqwbx2HaiCFBt3mlKQ/viewform?usp=pp_url&"
  parms <- URLencode(paste(paste(names(q), q, sep = "="), collapse = "&"))
  paste(u_base, parms, sep = "&")
}
