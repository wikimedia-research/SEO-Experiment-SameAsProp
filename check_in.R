# Preliminary assessment / check-in
library(magrittr)
library(zeallot)
library(glue)
library(ggplot2)
library(patchwork) # devtools::install_github("thomasp85/patchwork")

file_name <- "pageviews_by_group.csv"
if (!dir.exists("data")) dir.create("data")
system(glue("scp stat7:/home/bearloga/{file_name} data/{file_name}"))

# from https://meta.wikimedia.org/wiki/List_of_Wikipedias:
meta_data <- readr::read_csv("meta.csv") %>%
  dplyr::filter(n_articles >= 100) %>%
  dplyr::mutate(
    articles = factor(
      as.numeric(cut(n_articles, c(1e2, 1e3, 1e4, 1e5, 5e5, 1e6, 2e6, 6e7))),
      (1:7),
      paste(c("100-1K", "1K-10K", "10K-100K", "100K-500K", "500K-1M", "1M-2M", "2M-6M"), "articles")
    )
  )
table(meta_data$articles)

pageviews <- readr::read_csv(file.path("data", file_name)) %>%
  dplyr::left_join(meta_data, by = c("wiki" = "wiki_id")) %>%
  dplyr::mutate(
    test_group = factor(test_group, c("control", "treatment")),
    view_average = total_views / n_pages,
    page_popularity = factor(page_popularity, c("top 100", "less popular"))
  ) %>%
  dplyr::group_by(wiki) %>%
  dplyr::mutate(low_traffic_wiki = all(page_popularity == "top 100")) %>%
  # all pages will be in the top 100 pages if the wiki has fewer than 100 visited pages
  dplyr::ungroup()

annotations <- dplyr::data_frame(
  date = as.Date(c("2018-11-14", "2018-11-20",
                   "2018-12-24", "2019-01-01")),
  text = c("initial rollout @ 1%", "100% sampling",
           "Christmas Eve", "New Year's Day")
)

avg_daily_traffic <- pageviews %>%
  dplyr::filter(page_popularity == "less popular" | low_traffic_wiki) %>%
  dplyr::mutate(total_views = ifelse(is.na(total_views), 0, total_views)) %>%
  tidyr::complete(tidyr::nesting(articles, language, test_group), date, fill = list(total_views = 0, n_pages = 0)) %>%
  dplyr::mutate(
    period = ifelse(date <= "2018-11-20", "pre", "post"),
    period = factor(period, c("pre", "post"), c("before deployment", "after deployment"))
  ) %>%
  dplyr::group_by(articles, language, test_group, period) %>%
  dplyr::summarize(
    views = mean(total_views),
    logViews = mean(log(total_views + 1)),
    pages = max(n_pages)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.nan(views)) %>%
  dplyr::mutate(g = paste0(test_group, language))

avg_daily_traffic %>%
  dplyr::filter(pages > 0) %>%
  ggplot(aes(x = period, y = views, color = test_group, group = g)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ articles, scales = "free_y", ncol = 2) +
  wmf::theme_facet()
