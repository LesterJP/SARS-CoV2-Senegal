# Gregory S. Orf, Ph.D.
# Virus Discovery Group, Abbott Laboratories

# load libraries ----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(readxl)
library(ggtext)
library(viridisLite)
library(zoo)
library(ggrepel)

# import data -------------------------------------------------------------

setwd("Data/")

case_data <-
  read_csv(
    "owid-covid-data.csv"
  ) %>%
  subset(
    location == "Senegal"
  ) %>%
  mutate(
    date = as.Date(date, "%m/%d/%Y")
  ) %>%
  mutate(
    Month = as.Date(cut(date, breaks = "1 month", start.on.monday = FALSE))
  ) %>%
  select(
    Month,
    new_cases,
    new_deaths,
    new_vaccinations,
    reproduction_rate
  ) %>%
  replace(
    is.na(.),
    0
  ) %>%
  group_by(
    Month
  ) %>%
  summarise(
    Cases = sum(new_cases)
  ) %>%
  ungroup() %>%
  filter(
    Month >= as.Date("2020-03-01") & Month <= as.Date("2021-01-31")
  ) %>%
  mutate(
    Wave = ifelse(Month <= as.Date("2020-10-31"), "Wave 1", "Wave 2")
  )

sequencing_data <-
  read_excel(
    "Senegal_pango.xlsx"
  ) %>%
  mutate(
    Collection_date.x = as.Date(Collection_date.x, "%Y-%m-%d", tz = "UTC")
  ) %>%
  mutate(
    Month = as.Date(cut(Collection_date.x, breaks = "1 month", start.on.monday = FALSE))
  ) %>%
  group_by(
    Month
  ) %>%
  summarise(
    Sequences = n()
  ) %>%
  ungroup() %>%
  filter(
    Month >= as.Date("2020-03-01") & Month <= as.Date("2021-01-31")
  )

correlation_table <-
  case_data %>%
  full_join(
    sequencing_data,
    by = c("Month")
  ) %>%
  relocate(
    Wave,
    .after = Month
  ) %>%
  mutate_if(
    is.numeric,
    coalesce,
    0
  ) %>%
  mutate(
    Month = as.yearmon(Month, "%b %Y")
  )

correlation <-
  correlation_table %>%
  cor.test(
    data = .,
    x = .$Cases,
    y = .$Sequences,
    method = "pearson"
    # ,exact = FALSE
  )

# correlation

# plot --------------------------------------------------------------------

Fig_S2D <-
  ggplot() +
  theme_classic() +
  geom_point(
    data = correlation_table,
    mapping = aes(
      x = Cases,
      y = Sequences,
      color = Wave
    ),
    size = 4
  ) +
  geom_smooth(
    correlation_table,
    mapping = aes(
      x = Cases,
      y = Sequences
    ),
    formula = y ~ x,
    method = lm,
    se = FALSE,
    fullrange = TRUE,
    color = "#009E73",
    size = 1.5
  ) +
  scale_colour_manual(
    values = c(
      "Wave 1" = "#0072B2",
      "Wave 2" = "#D55E00"
    )
  ) +
  theme(
    axis.text.x = element_text(size = 14, face = "bold", color = "black"),
    axis.text.y = element_text(size = 14, face = "bold", color = "black"),
    axis.title.y = element_text(size = 18, face = "bold", color = "black"),
    axis.title.x = element_text(size = 18, face = "bold", color = "black"),
    legend.text = element_text(size = 14, face = "bold", color = "black"),
    legend.title = element_text(size = 18, face = "bold", color = "black", hjust = 0.5),
    legend.background = element_rect(fill = "transparent", color = "black", size = 1),
    legend.position = c(0.2, 0.8),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA)
  ) +
  annotate(
    geom = "text",
    x = 6000,
    y = 30,
    label = paste(
      "r =",
      round(correlation$estimate, 3),
      "\np-value =",
      round(correlation$p.value, 3)
    ),
    color = "#009E73",
    size = 8,
    fontface = "bold"
  ) +
  ylab("New Monthly Genomes") +
  xlab("New Monthly Cases")

Fig_S2D

# save image --------------------------------------------------------------

ggsave(
  file = "Fig_S2D.svg",
  plot = Fig_S2D,
  device = "svg",
  scale = 3.1415,
  width = 800,
  height = 600,
  units = c("px"),
  limitsize = FALSE,
  bg = "transparent"
)
