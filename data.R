# nice ionice spark2R --master yarn --executor-memory 2G --executor-cores 1 --driver-memory 4G

library(magrittr)
library(zeallot)
library(glue)

file_name <- "pageviews_by_group.csv"

excluded_codes <- c(
  "id", "pt", "pa", "pnb", "nl",
  "nds_nl", "ko", "bh", "chr",
  "kk", "ca", "fr", "yo", "xal"
)

wikis <- readr::read_csv("meta.csv") %>%
  dplyr::filter(n_articles >= 100) %>%
  dplyr::pull(wiki_id) %>%
  setdiff(excluded_codes)

query <- "
WITH pageviews_daily AS (
  SELECT
    CONCAT(year, '-', LPAD(month, 2, '0'), '-', LPAD(day, 2, '0')) AS `date`,
    page_id, SUM(view_count) AS view_count
  FROM wmf.pageview_hourly
  WHERE project = '${wiki}'
    AND year = ${year} AND month = ${month}
    AND referer_class = 'external (search engine)'
    AND agent_type = 'user'
    AND namespace_id = 0
  GROUP BY CONCAT(year, '-', LPAD(month, 2, '0'), '-', LPAD(day, 2, '0')), page_id
), page_assignments AS (
  SELECT page_id, test_group
  FROM bearloga.sameas_pages
  WHERE wiki_id = '${wiki}'
), pageview_assigments AS (
  SELECT `date`, test_group, page_assignments.page_id, view_count
  FROM page_assignments
  LEFT JOIN pageviews_daily ON page_assignments.page_id = pageviews_daily.page_id
  WHERE view_count IS NOT NULL AND `date` IS NOT NULL
), ranked_pageviews AS (
  SELECT `date`, test_group, view_count, ROW_NUMBER() OVER (PARTITION BY `date` ORDER BY view_count DESC) AS `rank`
  FROM pageview_assigments
)
SELECT
  `date`, test_group, IF(`rank` <= 100, 'top 100', 'less popular') AS page_popularity,
  COUNT(1) AS n_pages, SUM(view_count) AS total_views
FROM ranked_pageviews
GROUP BY `date`, test_group, IF(`rank` <= 100, 'top 100', 'less popular')
"

fetch_data <- function(date, wiki) {
  message("Fetching data from ", format(date, "%Y-%m"), " for ", wiki)
  c(year, month, day) %<-% wmf::extract_ymd(date)
  query <- as.character(glue(query, .open = "${"))
  result <- collect(sql(query))
  result %<>%
    dplyr::mutate(wiki = wiki) %>%
    dplyr::select(date, wiki, test_group, page_popularity, n_pages, total_views)
  # save in-progress data:
  readr::write_csv(result, file_name, append = file.exists(file_name))
  clearCache()
  clearJobGroup()
  return(invisible(NULL))
}

from_date <- as.Date("2018-10-01")
# to_date <- from_date
to_date <- as.Date("2019-02-28")
dates <- seq(from_date, to_date, by = "month")
combinations <- list(date = dates, wiki = wikis) %>%
  expand.grid(stringsAsFactors = FALSE) %>%
  dplyr::arrange(date, wiki)
# combinations %<>%
  # dplyr::filter(wiki %in% c("en.wikipedia", "es.wikipedia", "fr.wikipedia", "ru.wikipedia"))
purrr::walk2(combinations$date, combinations$wiki, fetch_data)
# ^ all remotely on stat1007
q(save = "no")
