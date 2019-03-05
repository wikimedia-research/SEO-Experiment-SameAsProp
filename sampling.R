library(glue)
library(magrittr)

snapshot <- "2019-01"

# Excluded from test:
# Indonesian: idwiki
# Portuguese: ptwiki
# Punjabi: pawiki, pnbwiki
# Dutch: nlwiki, nds_nlwiki
# Korean: kowiki
# Bhojpuri: bhwiki
# Cherokee: chrwiki
# Kazakh: kkwiki
# Catalan: cawiki
# French: frwiki
# Yoruba: yowiki
# Kalmyk: xalwiki
excluded_codes <- c(
  "id", "pt", "pa", "pnb", "nl",
  "nds_nl", "ko", "bh", "chr",
  "kk", "ca", "fr", "yo", "xal"
)

language_codes <- readr::read_csv("meta.csv") %>%
  dplyr::filter(n_articles >= 100) %>%
  dplyr::pull(wiki_id) %>%
  gsub(".wikipedia", "", .) %>%
  setdiff(excluded_codes)

wikis <- tibble::tibble(
  wiki_id = paste0(language_codes, ".wikipedia"),
  wiki_db = paste0(gsub("-", "_", language_codes, fixed = TRUE), "wiki")
)

recreate_table <- "
USE bearloga;
DROP TABLE IF EXISTS sameas_pages;
CREATE TABLE sameas_pages (
  wiki_id STRING COMMENT 'e.g. en.wikipedia',
  page_id BIGINT COMMENT 'page ID',
  page_random FLOAT COMMENT 'random number 0-1',
  test_group STRING COMMENT 'treatment or control'
);
"

message("Recreating 'bearloga.sameas_pages' table")
system(glue('hive -e "{recreate_table}"'))

# SET hive.exec.dynamic.partition.mode=nonstrict;
query <- "
INSERT INTO bearloga.sameas_pages
  SELECT
    '${wiki_id}' AS wiki_id,
    page_id,
    page_random,
    IF(page_random >= 0.5, 'treatment', 'control') AS test_group
  FROM wmf_raw.mediawiki_page
  WHERE snapshot = '${snapshot}'
    AND wiki_db = '${wiki_db}'
    AND NOT page_is_redirect
    AND page_namespace = 0
"

load_pages <- function(wiki_id, wiki_db) {
  message(glue("Loading pages from {wiki_db} as {wiki_id}"))
  query <- glue(query, .open = "${")
  system(glue('nice ionice hive -e "{query}"'))
  return(invisible(NULL))
}

# iterate over the (wiki_id, wiki_db) pairs to populate the sameas_pages table:
purrr::pwalk(wikis, load_pages)
