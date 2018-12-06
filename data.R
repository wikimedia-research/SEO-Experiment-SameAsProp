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
  )
)
SELECT
  wiki_id,
  test_group,
  COUNT(1) AS n_pages,
  SUM(view_count) AS view_count
FROM pageviews_in_test
GROUP BY wiki_id, test_group
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
    dplyr::mutate(date = date) %>%
    dplyr::arrange(wiki_id, test_group) %>%
    dplyr::select(date, wiki_id, test_group, n_pages, view_count)
  # save in-progress data:
  readr::write_csv(result, file_name_tmp, append = file.exists(file_name_tmp))
  return(result)
}

from_date <- as.Date("2018-11-14")
to_date <- as.Date("2018-12-05")
dates <- seq(from_date, to_date, by = "day")
results <- purrr::map_dfr(dates, fetch_data)
readr::write_csv(results, file_name)
# ^ all remotely on stat1007

# Locally:
if (!dir.exists("data")) dir.create("data")
system(glue("scp stat7:/home/bearloga/{file_name} data/{file_name}"))
