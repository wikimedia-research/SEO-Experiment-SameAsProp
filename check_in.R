# Preliminary assessment / check-in
library(magrittr)
library(zeallot)
library(glue)
library(ggplot2)
library(patchwork) # devtools::install_github("thomasp85/patchwork")

file_name <- "pageviews_by_group.csv"
file_name_tmp <- "pageviews_by_group_online.csv"
if (!dir.exists("data")) dir.create("data")
system(glue("scp stat7:/home/bearloga/{file_name} data/{file_name}"))

pageviews <- readr::read_csv("data/pageviews_by_group.csv") %>%
  # dplyr::filter(date <= "2019-01-15") %>%
  dplyr::mutate(
    test_group = factor(test_group, c("control", "treatment")),
    view_average = view_total / n_pages,
    popular = factor(popular, c("yes", "no"), c("top 100 pages", "less popular pages"))
  ) %>%
  dplyr::arrange(wiki_id, test_group, popular, date) %>%
  dplyr::group_by(wiki_id, test_group, popular) %>%
  dplyr::mutate(
    roll_total = c(rep(NA, 3), RcppRoll::roll_mean(view_total, 7), rep(NA, 3)),
    roll_average = c(rep(NA, 3), RcppRoll::roll_mean(view_average, 7), rep(NA, 3))
  ) %>%
  dplyr::ungroup()

annotations <- dplyr::data_frame(
  date = as.Date(c("2018-11-14", "2018-11-20",
                   "2018-12-24", "2019-01-01")),
  text = c("initial rollout @ 1%", "100% sampling",
           "Christmas Eve", "New Year's Day")
)

p1 <- ggplot(pageviews, aes(x = date)) +
  geom_vline(aes(xintercept = date), data = annotations) +
  geom_text(aes(y = 1e5, label = text), data = annotations,
            angle = 90, nudge_x = -1, hjust = "left", vjust = "bottom",
            alpha = 0.5) +
  geom_line(aes(y = view_total, color = test_group), alpha = 0.6) +
  geom_line(aes(y = roll_total, color = test_group), size = 1.1) +
  # geom_smooth(aes(y = view_total, color = test_group), se = FALSE) +
  scale_y_log10(labels = polloi::compress) +
  scale_color_brewer(palette = "Set1") +
  facet_grid(popular ~ wiki_id, scales = "free_y") +
  wmf::theme_facet(14, "Source Sans Pro") +
  labs(
    title = "Total daily pageviews by group in sameAs A/B test",
    x = "Date", y = "Search engine-referred user pageviews", color = "Test group"
  )
p1
ggsave("plot1.png", p1, width = 16, height = 8, dpi = 100)

p2 <- ggplot(pageviews, aes(x = date)) +
  geom_vline(aes(xintercept = date), data = annotations) +
  geom_text(aes(y = 1e0, label = text), data = annotations,
            angle = 90, nudge_x = -1, hjust = "left", vjust = "bottom",
            alpha = 0.5) +
  geom_line(aes(y = view_average, color = test_group), alpha = 0.6) +
  geom_line(aes(y = roll_average, color = test_group), size = 1.1) +
  scale_y_log10(labels = polloi::compress) +
  scale_color_brewer(palette = "Set1") +
  facet_grid(popular ~ wiki_id, scales = "free_y") +
  wmf::theme_facet(14, "Source Sans Pro") +
  labs(
    title = "Average daily pageviews by group in sameAs A/B test",
    x = "Date", y = "Search engine-referred user pageviews", color = "Test group"
  )
p2
ggsave("plot2.png", p1, width = 16, height = 8, dpi = 100)

p3 <- ggplot(pageviews, aes(x = date)) +
  geom_vline(aes(xintercept = date), data = annotations) +
  geom_text(aes(y = 10, label = text), data = annotations,
            angle = 90, nudge_x = -1, vjust = "bottom", hjust = "left",
            alpha = 0.5) +
  geom_line(aes(y = view_median, color = test_group), linetype = "solid") +
  geom_line(aes(y = view_median, color = test_group), linetype = "dashed") +
  scale_y_log10(labels = polloi::compress) +
  scale_color_brewer(palette = "Set1") +
  facet_grid(popular ~ wiki_id) +
  wmf::theme_facet(14, "Source Sans Pro") +
  labs(
    title = "Median daily pageviews by group in sameAs A/B test",
    x = "Date", y = "Search engine-referred user pageviews", color = "Test group"
  )
p3
ggsave("plot3.png", p2, width = 16, height = 8, dpi = 100)
