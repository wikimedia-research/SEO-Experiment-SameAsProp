# spark2R --master yarn --executor-memory 2G --executor-cores 1 --driver-memory 4G

library(magrittr)
library(zeallot)
library(glue)

file_name <- "pageviews_by_group.csv"
file_name_tmp <- "pageviews_by_group_online.csv"

query <- "
WITH pageviews_in_test AS (
  SELECT
    sap.wiki_id,
    sap.page_id,
    sap.test_group,
    pvh.hour,
    pvh.view_count
  FROM bearloga.sameas_pages sap
  LEFT JOIN wmf.pageview_hourly pvh ON (
    sap.page_id = pvh.page_id
    AND sap.wiki_id = pvh.project
    AND pvh.year = ${year}
    AND pvh.month = ${month}
    AND pvh.day = ${day}
    AND pvh.referer_class = 'external (search engine)'
    AND pvh.agent_type = 'user'
  )
), day_pageviews AS (
  SELECT wiki_id, test_group, page_id, SUM(view_count) AS view_count
  FROM pageviews_in_test
  GROUP BY wiki_id, test_group, page_id
), distributed_pageviews AS (
  SELECT
    wiki_id, page_id, view_count,
    ROW_NUMBER() OVER (PARTITION BY wiki_id ORDER BY view_count DESC) AS rank
  FROM day_pageviews
), top_100 AS (
  SELECT wiki_id, page_id, 'yes' AS popular
  FROM distributed_pageviews
  WHERE rank <= 100
), separated_pageviews AS (
  SELECT
    dpv.wiki_id,
    dpv.test_group,
    dpv.page_id,
    top_100.popular,
    dpv.view_count
  FROM day_pageviews dpv
  LEFT JOIN top_100 ON (
    dpv.wiki_id = top_100.wiki_id
    AND dpv.page_id = top_100.page_id
  )
)
SELECT
  wiki_id,
  test_group,
  NVL(popular, 'no') AS popular,
  COUNT(1) AS n_pages,
  SUM(view_count) AS view_total,
  PERCENTILE(view_count, 0.5) AS view_median
FROM separated_pageviews
GROUP BY wiki_id, test_group, NVL(popular, 'no')
"

date_components <- function(date) {
  year <- lubridate::year(date)
  month <- lubridate::month(date)
  day <- lubridate::mday(date)
  return(c(year, month, day))
}

fetch_data <- function(date) {
  formatted_date <- format(date, "%Y-%m-%d")
  message("Fetching data from ", formatted_date)
  c(year, month, day) %<-% date_components(date)
  query <- as.character(glue(query, .open = "${"))
  result <- collect(sql(query))
  result %<>%
    dplyr::mutate(date = date, popular == 'yes') %>%
    dplyr::arrange(wiki_id, popular, test_group) %>%
    dplyr::select(date, wiki_id, popular, test_group, n_pages, view_total, view_median)
  # save in-progress data:
  readr::write_csv(result, file_name_tmp, append = file.exists(file_name_tmp))
  clearCache()
  clearJobGroup()
  return(result)
}

from_date <- as.Date("2018-11-01")
# to_date <- from_date
to_date <- as.Date("2018-12-07")
dates <- seq(from_date, to_date, by = "day")
results <- purrr::map_dfr(dates, fetch_data)
readr::write_csv(results, file_name)
# ^ all remotely on stat1007
