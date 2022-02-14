# Gregory S. Orf, Ph.D.
# Virus Discovery Group, Abbott Laboratories

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(readxl)
library(ggtext)
library(viridisLite)

# Load data ---------------------------------------------------------------

setwd("Data/")

case_data <-
  read_csv("Fig_1_owid-covid-data.csv") %>%
  filter(location == "Senegal") %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  mutate(Week = as.Date(cut(date, breaks = "1 week", start.on.monday = FALSE))) %>%
  select(Week, new_cases, new_deaths, new_vaccinations, reproduction_rate) %>%
  replace(is.na(.), 0) %>%
  group_by(Week) %>%
  summarise(
    Cases = sum(new_cases)
  ) %>%
  ungroup() %>%
  filter(Week <= as.Date("2021-02-10"))

sequencing_data <-
  read_excel("Fig_1_Senegal_pango.xlsx") %>%
  mutate(
    Collection_date.x = as.Date(Collection_date.x, "%Y-%m-%d", tz = "UTC")
  ) %>%
  mutate(
    Week = as.Date(cut(Collection_date.x, breaks = "1 week", start.on.monday = FALSE))
  ) %>%
  group_by(Week) %>%
  summarise(
    Sequences = n()
  ) %>%
  ungroup()

data_for_plotting <-
  case_data %>%
  full_join(
    sequencing_data,
    by = c("Week")
  ) %>%
  mutate_if(
    is.numeric,
    coalesce,
    0
  )

wave1_stat <-
  round(
    sum(
      data_for_plotting %>%
        filter(
          Week < as.Date("2020-10-31")
        ) %>% pull(Sequences)
    ) / (
      sum(
        data_for_plotting %>%
          filter(
            Week < as.Date("2020-10-31")
          ) %>% pull(Cases)
      ) / 1000
    ),
    2
  )

wave2_stat <-
  round(
    sum(
      data_for_plotting %>%
        filter(
          Week > as.Date("2020-10-31")
        ) %>% pull(Sequences)
    ) / (
      sum(
        data_for_plotting %>%
          filter(
            Week > as.Date("2020-10-31")
          ) %>% pull(Cases)
      ) / 1000
    ),
    2
  )

# plot --------------------------------------------------------------------

scale_coeff <- 50

Fig_S2 <-
  ggplot(
    data_for_plotting,
    aes(x = Week)
  ) +
  geom_bar(
    aes(
      y = Sequences * scale_coeff
    ),
    stat = "identity",
    fill = "grey",
    color = "black",
    alpha = 0.75,
    size = 0.75
  ) +
  geom_point(
    aes(
      y = Cases
    ),
    color = "darkmagenta",
    alpha = 1.0,
    size = 3
  ) +
  geom_line(
    aes(
      y = Cases
    ),
    color = "darkmagenta",
    alpha = 1.0,
    size = 0.75
  ) +
  theme_classic() +
  scale_y_continuous(
    name = "New positive cases/week",
    expand = expansion(mult = c(0, 0.1)),
    sec.axis = sec_axis(
      ~ . / scale_coeff,
      name = "Sequenced cases/week"
    )
  ) +
  scale_x_date(
    date_labels = "%b-%Y",
    breaks = seq.Date(ymd("2020-03-02"), ymd("2021-02-10"), by = "month"),
    limits = c(ymd("2020-03-02"), ymd("2021-02-10"))
  ) +
  xlab(
    "Date"
  ) +
  theme(
    axis.text.x = element_text(size = 16, angle = 90, face = "bold", color = "black", vjust = 2, hjust = 1),
    axis.text.y = element_text(size = 16, face = "bold", color = "darkmagenta"),
    axis.text.y.right = element_text(size = 16, face = "bold", color = "black"),
    axis.title.y = element_text(size = 18, face = "bold", vjust = 2, color = "darkmagenta"),
    axis.title.y.right = element_text(size = 18, face = "bold", vjust = 2, color = "black"),
    axis.title.x = element_text(size = 18, face = "bold", vjust = -1),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA)
  ) +
  annotate(
    geom = "text",
    x = as.Date("2020-06-01"),
    y = 2000,
    label = paste("Mar 2020-Oct 2020\n", wave1_stat, "genomes/1000 cases")
  ) +
  annotate(
    geom = "text",
    x = as.Date("2020-11-01"),
    y = 2000,
    label = paste("Nov 2020-Feb 2021\n", wave2_stat, "genomes/1000 cases")
  )
  
Fig_S2

# save figure -------------------------------------------------------------

ggsave(
  file = "Fig_S2.svg",
  plot = Fig_S2,
  device = "svg",
  scale = 3.15,
  width = 800,
  height = 480,
  units = c("px"),
  limitsize = FALSE,
  bg = "transparent"
)
