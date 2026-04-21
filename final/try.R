library(tidyverse)
library(lubridate)
library(sf)
library(tigris)
library(here)
library(scales)

eviction <- read.csv(here("data/philadelphia_weekly_2020_2021.csv"))

summary(eviction)
names(eviction)

evic_2023 <- eviction %>%
  filter(week >= 158)

evic_2023 <- evic_2023 %>%
  mutate(
    week_date = as.Date(week_date),
    GEOID = as.character(GEOID)
  )

## time change
weekly_trend <- evic_2023 %>%
  group_by(week_date) %>%
  summarise(
    filings_2020 = sum(filings_2020, na.rm = TRUE),
    filings_avg = sum(filings_avg, na.rm = TRUE),
    filings_baseline = sum(filings_avg_prepandemic_baseline, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(weekly_trend, aes(x = week_date)) +
  geom_line(aes(y = filings_2020, color = "2020 filings"), linewidth = 1) +
  geom_line(aes(y = filings_avg, color = "historical avg"), linetype = "dashed") +
  geom_line(aes(y = filings_baseline, color = "baseline"), linetype = "dotted") +
  labs(
    title = "Weekly Eviction Filings, week >= 158",
    x = "Week",
    y = "Filings",
    color = NULL
  ) +
  theme_minimal()

## racial change

weekly_by_race <- evic_2023 %>%
  group_by(week_date, racial_majority) %>%
  summarise(
    filings_2020 = sum(filings_2020, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(weekly_by_race, aes(x = week_date, y = filings_2020, color = racial_majority)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Weekly Filings by Racial Majority, week >= 158",
    x = "Week",
    y = "Filings",
    color = "Racial majority"
  ) +
  theme_minimal()

## spacial summary
tract_summary <- evic_2023 %>%
  group_by(GEOID) %>%
  summarise(
    total_filings = sum(filings_2020, na.rm = TRUE),
    avg_weekly_filings = mean(filings_2020, na.rm = TRUE),
    racial_majority = first(racial_majority)
    .groups = "drop"
  )

## map
phl_tracts <- tracts(
  state = "PA",
  county = "Philadelphia",
  year = 2020,
  class = "sf"
) %>%
  mutate(GEOID = as.character(GEOID))
map_df <- phl_tracts %>%
  left_join(tract_summary, by = "GEOID")

ggplot(map_df) +
  geom_sf(aes(fill = total_filings), color = NA) +
  scale_fill_viridis_c(na.value = "grey90", labels = comma) +
  labs(
    title = "Total Eviction Filings by Tract, week >= 158",
    fill = "Total filings"
  ) +
  theme_minimal()

ggplot(tract_summary, aes(x = total_filings)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of Total Eviction Filings Across Tracts",
    x = "Total filings per tract",
    y = "Number of tracts"
  ) +
  theme_minimal()
