library(magrittr)
library(rvest)

wikipedias_list <- "https://en.wikipedia.org/wiki/List_of_Wikipedias"
wikipedias_list <- read_html(wikipedias_list)

wikipedias <- wikipedias_list %>%
  xml_find_all(".//table[contains(@class, 'wikitable')]") %>%
  .[[3]] %>%
  html_table() %>%
  set_colnames(c(
    "language", "language_local", "wiki_id",
    "n_articles", "n_pages", "n_edits",
    "n_admins", "n_users", "n_active_users",
    "n_images", "depth"
  )) %>%
  dplyr::mutate(wiki_id = paste0(wiki_id, ".wikipedia")) %>%
  dplyr::select(-c(language_local, depth, n_images))

wikipedias %>%
  dplyr::select(
    wiki_id, language, n_articles, n_admins,
    n_editors = n_users, n_active_editors = n_active_users
  ) %>%
  readr::write_csv("meta.csv")
