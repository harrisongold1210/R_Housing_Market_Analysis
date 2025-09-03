title: "R_Bootcamp_Indv"
output: html_document
date: "2025-09-01"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
pkgs <- c("tidyverse", "lubridate", "scales", "janitor")
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install)) install.packages(to_install, quiet = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

use_vroom <- requireNamespace("vroom", quietly = TRUE)

data_path <- "RDC_Inventory_Hotness_Metrics_Zip_History.csv"  # put file in your working dir

if (use_vroom) {
  dat_raw <- vroom::vroom(data_path, show_col_types = FALSE)
} else {
  dat_raw <- readr::read_csv(data_path, show_col_types = FALSE, progress = TRUE)
}

required_cols <- c(
  "month_date_yyyymm", "zip_name", "postal_code",
  "hotness_rank", "hotness_score",
  "median_days_on_market", "median_listing_price"
)
missing_cols <- setdiff(required_cols, names(dat_raw))
if (length(missing_cols)) {
  stop("Missing expected columns: ", paste(missing_cols, collapse = ", "))
}

dat <- dat_raw %>%
  clean_names() %>%                                   # snake_case
  mutate(
    date = ymd(paste0(month_date_yyyymm, "01")),
    year = year(date)
  ) %>%
  separate(zip_name, into = c("city", "state"), sep = ",\\s*", fill = "right", remove = FALSE) %>%
  mutate(state = toupper(state)) %>%
  filter(!is.na(state), str_detect(state, "^[A-Z]{2}$")) %>% 
  mutate(
    hotness_rank = suppressWarnings(as.numeric(hotness_rank)),
    hotness_score = suppressWarnings(as.numeric(hotness_score)),
    median_days_on_market = suppressWarnings(as.numeric(median_days_on_market)),
    median_listing_price   = suppressWarnings(as.numeric(median_listing_price))
  )

state_month <- dat %>%
  group_by(state, date, year) %>%
  summarise(
    dom_median   = median(median_days_on_market, na.rm = TRUE),
    price_median = median(median_listing_price,   na.rm = TRUE),
    hotness_rank_median = median(hotness_rank,    na.rm = TRUE),
    hotness_score_median = median(hotness_score,  na.rm = TRUE),
    .groups = "drop"
  )

state_year <- state_month %>%
  group_by(state, year) %>%
  summarise(
    dom_annual   = median(dom_median, na.rm = TRUE),
    price_annual = median(price_median, na.rm = TRUE),
    hotness_rank_annual = median(hotness_rank_median, na.rm = TRUE),
    hotness_score_annual = median(hotness_score_median, na.rm = TRUE),
    .groups = "drop"
  )

state_profile_summary <- state_year %>%
  arrange(state, year)

print(head(state_profile_summary, 12))
state_profile_wide <- state_year %>%
  arrange(year) %>%
  pivot_wider(
    names_from = year,
    values_from = c(dom_annual, price_annual, hotness_rank_annual, hotness_score_annual),
    names_sep = "_"
  )
print(head(state_profile_wide, 10))

focus_states <- c("GA", "FL", "AL")
deep_period  <- interval(ymd("2022-01-01"), ymd("2023-12-31"))

deep <- state_month %>%
  filter(state %in% focus_states, date %within% deep_period)

# Create output folder for plots
if (!dir.exists("figs")) dir.create("figs")

# ---- Plot: Median Days on Market (monthly, 2022–2023) ----
p_dom <- deep %>%
  ggplot(aes(x = date, y = dom_median, color = state)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.2) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  labs(
    title = "Median Days on Market (Monthly) — GA vs. FL vs. AL (2022–2023)",
    x = NULL, y = "Median Days on Market"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")
print(p_dom)
ggsave("figs/deep_dive_dom_2022_2023.png", p_dom, width = 10, height = 6, dpi = 300)

p_price <- deep %>%
  ggplot(aes(x = date, y = price_median, color = state)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.2) +
  scale_y_continuous(labels = dollar_format(accuracy = 1)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  labs(
    title = "Median Listing Price (Monthly) — GA vs. FL vs. AL (2022–2023)",
    x = NULL, y = "Median Listing Price"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")
print(p_price)
ggsave("figs/deep_dive_price_2022_2023.png", p_price, width = 10, height = 6, dpi = 300)

deep_stats <- deep %>%
  group_by(state) %>%
  summarise(
    dom_med_2022_23   = median(dom_median,   na.rm = TRUE),
    price_med_2022_23 = median(price_median, na.rm = TRUE),
    hotrank_med_2022_23 = median(hotness_rank_median, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    rank_hotter = rank(hotrank_med_2022_23, ties.method = "min"),  # 1 = hottest (lowest rank)
    rank_faster = rank(dom_med_2022_23,     ties.method = "min"),  # 1 = fastest (lowest DOM)
    rank_cheaper= rank(price_med_2022_23,   ties.method = "min")   # 1 = cheapest (lowest price)
  )

hotter_state  <- deep_stats %>% arrange(hotrank_med_2022_23) %>% slice(1) %>% pull(state)
faster_state  <- deep_stats %>% arrange(dom_med_2022_23)     %>% slice(1) %>% pull(state)
cheaper_state <- deep_stats %>% arrange(price_med_2022_23)   %>% slice(1) %>% pull(state)

cat("\n--- GA/FL/AL Narrative (2022–2023) ---\n")
cat(sprintf("Hotter (lower hotness_rank): %s\n", hotter_state))
cat(sprintf("Moving faster (lower days on market): %s\n", faster_state))
cat(sprintf("Cheaper (lower listing price): %s\n", cheaper_state))
cat("\nDetailed stats:\n")
print(deep_stats %>% arrange(state))


top5_2023 <- state_month %>%
  filter(year == 2023) %>%
  group_by(state) %>%
  summarise(hotrank_2023 = median(hotness_rank_median, na.rm = TRUE), .groups = "drop") %>%
  arrange(hotrank_2023) %>%
  slice_head(n = 5) %>%
  pull(state)

hot_trends <- state_month %>%
  filter(state %in% top5_2023) %>%
  arrange(state, date)
```


```
Florida is ranked the hottest compared to Georgia and Alabama. With that said, Alabamahas the cheapest listing prices while Georgia has the average least days on market per house.

Connecticut had the best hotness scale as of 2023 at a score of ~1200. This is significantly better than the GA, FL, and AL markets. Could this be because of the smaller area of land in CT? Has there been recent development in the current years? Are houses getting cheaper there?

Ovevrall, the correlation with hotness and days on market (DOM) show how the longer a house sits on the market, they tend to get colder in their hotness ranking. Hotter markets seem to have higher supply and demand, which in turn shows they have less DOM per house.

```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
p_hot <- hot_trends %>%
  ggplot(aes(x = date, y = hotness_rank_median, color = state)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.2) +
  scale_y_reverse(labels = comma) +  # reverse so "hotter" (lower rank) is higher on the chart
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  labs(
    title = "Hotness Rank Trends — Top 5 States (by 2023 median rank)",
    x = NULL, y = "Median Hotness Rank (state-month, lower = hotter)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")
print(p_hot)
ggsave("figs/hotness_rank_trends_top5.png", p_hot, width = 10, height = 6, dpi = 300)
```
