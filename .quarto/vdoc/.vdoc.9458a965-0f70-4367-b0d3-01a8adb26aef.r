#
#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
#
#
#
billboard |>
  select(artist, track, date.entered, wk1, wk2, wk3, wk4)
#
#
#
billboard |>
  summarize(
    min_date = min(date.entered),
    max_date = max(date.entered)
  )
#
#
#
billboard |>
  ggplot(aes(x = wk1)) +
  geom_histogram(bins = 30, fill = "steelblue", na.rm = TRUE) +
  labs(
    title = "Distribution of Week 1 Rankings",
    subtitle = paste("n =", sum(!is.na(billboard$wk1))),
    x = "Week 1 Ranking",
    y = "Count"
  ) +
  theme_minimal()
#
#
#
billboard |>
  ggplot(aes(x = wk6)) +
  geom_histogram(bins = 30, fill = "coral", na.rm = TRUE) +
  labs(
    title = "Distribution of Week 6 Rankings",
    subtitle = paste("n =", sum(!is.na(billboard$wk6))),
    x = "Week 6 Ranking",
    y = "Count"
  ) +
  theme_minimal()
#
#
#
weeks <- c("wk1", "wk4", "wk10", "wk20", "wk40", "wk76")

missing_present <- tibble(
  week = weeks,
  present = sapply(weeks, function(w) sum(!is.na(billboard[[w]]))),
  missing = sapply(weeks, function(w) sum(is.na(billboard[[w]])))
)

missing_present
#
#
#
# Get the week columns
week_cols <- grep("^wk", names(billboard), value = TRUE)

re_entered_count <- billboard |>
  mutate(
    re_entered = apply(
      select(billboard, all_of(week_cols)),
      1,
      function(row) {
        # Find first NA
        first_na_idx <- which(is.na(row))[1]
        
        if (is.na(first_na_idx)) {
          # No NAs found, so didn't leave chart
          FALSE
        } else if (first_na_idx == length(row)) {
          # Last week has NA, but no weeks after
          FALSE
        } else {
          # Check if there are any non-NA values after first NA
          any(!is.na(row[(first_na_idx + 1):length(row)]))
        }
      }
    )
  ) |>
  count(re_entered) |>
  rename(
    `Re-entered Chart` = re_entered,
    `Number of Songs` = n
  )

re_entered_count
#
#
#
#
