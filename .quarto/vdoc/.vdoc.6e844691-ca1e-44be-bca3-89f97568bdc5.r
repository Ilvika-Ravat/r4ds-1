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
# Compare wk1 and wk6 rankings
rank_comparison <- billboard |>
  mutate(
    rank_change = wk6 - wk1,
    comparison = case_when(
      is.na(wk6) ~ "Missing by wk6",
      rank_change < 0 ~ "Improved",
      rank_change == 0 ~ "Stayed the same",
      rank_change > 0 ~ "Got worse",
      TRUE ~ "Other"
    )
  )

# Count by comparison category
comparison_counts <- rank_comparison |>
  count(comparison) |>
  rename(
    Category = comparison,
    Count = n
  )

print("Rank Changes from Week 1 to Week 6:")
print(comparison_counts)

# Calculate median rank change for songs with both weeks
median_change <- rank_comparison |>
  filter(!is.na(wk1) & !is.na(wk6)) |>
  summarize(
    `Median Rank Change` = median(rank_change, na.rm = TRUE),
    `Mean Rank Change` = mean(rank_change, na.rm = TRUE),
    `Songs with both weeks` = n()
  )

print("Rank Change Statistics:")
print(median_change)
#
#
#
#| cache: true
# Reshape billboard to long format
billboard_long <- billboard |>
  unite("artist_track", artist, track, sep = " - ", remove = FALSE) |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    names_prefix = "wk",
    names_transform = list(week = as.integer)
  )

# Plot ranking over time for each song
billboard_long |>
  ggplot(aes(x = week, y = rank, group = artist_track, color = artist_track)) +
  geom_line(alpha = 0.3, show.legend = FALSE) +
  scale_y_reverse() +
  labs(
    title = "Billboard Song Rankings Over Time",
    x = "Week",
    y = "Rank (lower is better)",
    caption = "Each line represents one song's trajectory on the Billboard chart"
  ) +
  theme_minimal()
#
#
#
# Create song-level summary
song_summary <- billboard_long |>
  group_by(artist, track) |>
  summarize(
    first_rank = first(rank, na_rm = TRUE),
    best_rank = min(rank, na.rm = TRUE),
    weeks_to_best = min(week[rank == best_rank], na.rm = TRUE),
    total_weeks = sum(!is.na(rank)),
    .groups = "drop"
  ) |>
  mutate(artist_track = paste(artist, "-", track))

head(song_summary, 10)

# Find notable songs
# Fastest to reach #1
fastest_to_one <- song_summary |>
  filter(best_rank == 1) |>
  slice_min(weeks_to_best, n = 1)

# Slowest to reach #1
slowest_to_one <- song_summary |>
  filter(best_rank == 1) |>
  slice_max(weeks_to_best, n = 1)

# Top-10 song with longest chart run
longest_top10 <- song_summary |>
  filter(best_rank <= 10) |>
  slice_max(total_weeks, n = 1)

# Compile notable songs
notable_songs <- bind_rows(
  fastest_to_one |> mutate(category = "Fastest to #1"),
  slowest_to_one |> mutate(category = "Slowest to #1"),
  longest_top10 |> mutate(category = "Longest Top-10 Run")
) |>
  select(category, artist, track, first_rank, best_rank, weeks_to_best, total_weeks)

cat("\nNotable Top-10 Songs:\n")
print(notable_songs)
#
#
#
# Get the notable song names
fastest_name <- paste(fastest_to_one$artist, "-", fastest_to_one$track)
slowest_name <- paste(slowest_to_one$artist, "-", slowest_to_one$track)
longest_name <- paste(longest_top10$artist, "-", longest_top10$track)

# Filter to only top-10 songs and add classification
top10_songs <- billboard_long |>
  left_join(
    song_summary |> select(artist, track, best_rank),
    by = c("artist", "track")
  ) |>
  filter(best_rank <= 10) |>
  mutate(
    song_category = case_when(
      artist_track == fastest_name ~ "Fastest to #1",
      artist_track == slowest_name ~ "Slowest to #1",
      artist_track == longest_name ~ "Longest Top-10 Run",
      TRUE ~ "Other Top-10"
    )
  )

# Plot top-10 songs with notable ones highlighted
top10_songs |>
  ggplot(aes(x = week, y = rank, group = artist_track, color = song_category, 
             size = song_category, alpha = song_category)) +
  geom_line() +
  scale_y_reverse() +
  scale_color_manual(
    values = c(
      "Other Top-10" = "gray80",
      "Fastest to #1" = "#e41a1c",
      "Slowest to #1" = "#377eb8",
      "Longest Top-10 Run" = "#4daf4a"
    ),
    name = "Song Category"
  ) +
  scale_size_manual(
    values = c(
      "Other Top-10" = 0.5,
      "Fastest to #1" = 1.2,
      "Slowest to #1" = 1.2,
      "Longest Top-10 Run" = 1.2
    ),
    guide = "none"
  ) +
  scale_alpha_manual(
    values = c(
      "Other Top-10" = 0.3,
      "Fastest to #1" = 1,
      "Slowest to #1" = 1,
      "Longest Top-10 Run" = 1
    ),
    guide = "none"
  ) +
  labs(
    title = "Top-10 Billboard Songs with Notable Artists Highlighted",
    x = "Week",
    y = "Rank (lower is better)",
    caption = "Gray lines show all top-10 songs; colored lines show three notable songs"
  ) +
  theme_minimal() +
  theme(legend.position = "right")
#
#
#
#
#
#
#
#| message: false
library(readr)
music <- read_csv("data/music.csv")
music
#
#
#
#| message: false
artist_cols <- music |>
  select(starts_with("artist."))

artist_cols |>
  glimpse()

artist_cols |>
  slice_head(n = 5)
#
#
#
#| message: false
music |>
  summarize(
    min_artist_familiarity = min(artist.familiarity, na.rm = TRUE),
    q1_artist_familiarity = quantile(artist.familiarity, 0.25, na.rm = TRUE),
    median_artist_familiarity = median(artist.familiarity, na.rm = TRUE),
    q3_artist_familiarity = quantile(artist.familiarity, 0.75, na.rm = TRUE),
    max_artist_familiarity = max(artist.familiarity, na.rm = TRUE),

    min_artist_hotttnesss = min(artist.hotttnesss, na.rm = TRUE),
    q1_artist_hotttnesss = quantile(artist.hotttnesss, 0.25, na.rm = TRUE),
    median_artist_hotttnesss = median(artist.hotttnesss, na.rm = TRUE),
    q3_artist_hotttnesss = quantile(artist.hotttnesss, 0.75, na.rm = TRUE),
    max_artist_hotttnesss = max(artist.hotttnesss, na.rm = TRUE),

    min_song_year = min(song.year, na.rm = TRUE),
    q1_song_year = quantile(song.year, 0.25, na.rm = TRUE),
    median_song_year = median(song.year, na.rm = TRUE),
    q3_song_year = quantile(song.year, 0.75, na.rm = TRUE),
    max_song_year = max(song.year, na.rm = TRUE),

    min_song_tempo = min(song.tempo, na.rm = TRUE),
    q1_song_tempo = quantile(song.tempo, 0.25, na.rm = TRUE),
    median_song_tempo = median(song.tempo, na.rm = TRUE),
    q3_song_tempo = quantile(song.tempo, 0.75, na.rm = TRUE),
    max_song_tempo = max(song.tempo, na.rm = TRUE)
  )
#
#
#
#| message: false
song_year_data <- music |>
  filter(song.year != 0)

song_year_count <- nrow(song_year_data)

song_year_data |>
  ggplot(aes(x = song.year)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Song Year",
    subtitle = paste("n =", song_year_count, "songs"),
    x = "Song Year",
    y = "Count"
  ) +
  theme_minimal()
```
#
#| message: false
music |>
  select(
    artist.name,
    artist.location,
    artist.latitude,
    artist.longitude,
    artist.terms,
    artist.familiarity,
    artist.hotttnesss,
    song.title,
    song.year
  ) |>
  slice_head(n = 10)
#
#
#
