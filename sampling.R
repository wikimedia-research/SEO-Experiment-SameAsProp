library(glue)

wikis <- dplyr::data_frame(
  wiki_id = c("en.wikipedia", "es.wikipedia", "pl.wikipedia", "bg.wikipedia"),
  wiki_db = c("enwiki", "eswiki", "plwiki", "bgwiki")
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
  WHERE snapshot = '2018-12'
    AND wiki_db = '${wiki_db}'
    AND NOT page_is_redirect
    AND page_namespace = 0
"

load_pages <- function(wiki_id, wiki_db) {
  message(glue("Loading pages from {wiki_db} as {wiki_id}"))
  query <- glue(query, .open = "${")
  system(glue('hive -e "{query}"'))
  return(invisible(NULL))
}

# iterate over the (wiki_id, wiki_db) pairs to populate the sameas_pages table:
purrr::pmap(wikis, load_pages)
