# Gregory S. Orf, Ph.D.
# Abbott Virus Discovery Group

# load libraries ----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(scales)
library(cowplot)

# load data ---------------------------------------------------------------

setwd("Data/")

B1416_history <-
  read_csv(
    "B1416.total_tree_travelHistory.csv",
    show_col_types = FALSE
  )

B1416_interesting_locations <-
  c(
    "Senegal",
    "Gambia",
    "France"
  )

B1416_important_interactions <-
  c(
    "Senegal to Gambia",
    "Senegal to France",
    "Senegal to Others",
    "Gambia to Senegal",
    "Gambia to France",
    "Gambia to Others",
    "Others to Senegal",
    "Others to Others"
  )

B11420_history <-
  read_csv(
    "B11420.total_tree_travelHistory.csv",
    show_col_types = FALSE
  )

B11420_interesting_locations <-
  c(
    "Senegal",
    "Italy",
    "Switzerland"
  )

B11420_important_interactions <-
  c(
    "Italy to Switzerland",
    "Italy to Senegal",
    "Switzerland to Italy",
    "Switzerland to Senegal",
    "Senegal to Italy",
    "Senegal to Switzerland",
    "Senegal to Others",
    "Others to Others"
  )

# prepare data ------------------------------------------------------------

B1416_total_tree_travel_history <-
  B1416_history %>%
  mutate(date = date_decimal(time, tz = "UTC")) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d", tz = "UTC")) %>%
  select(treeId, date, startLocation, endLocation) %>%
  mutate(startLocation = ifelse(startLocation %in% B1416_interesting_locations, startLocation, "Others")) %>%
  mutate(endLocation = ifelse(endLocation %in% B1416_interesting_locations, endLocation, "Others")) %>%
  mutate(date = as.Date(cut(date, breaks = "1 week", start.on.monday = FALSE))) %>%
  select(date, startLocation, endLocation) %>%
  group_by(date, startLocation, endLocation) %>%
  summarise(value = n()) %>%
  ungroup() %>%
  arrange(date) %>%
  mutate(event = paste0(startLocation, " to ", endLocation)) %>%
  select(event, date, value) %>%
  complete(., event, date, fill = list(value = 0)) %>%
  filter(event %in% B1416_important_interactions) %>%
  filter(date >= as.Date("2020-01-01")) %>%
  arrange(date) %>%
  group_by(event) %>%
  mutate(running_total = cumsum(value)) %>%
  ungroup() %>%
  mutate(event = factor(event, levels = B1416_important_interactions))

B11420_total_tree_travel_history <-
  B11420_history %>%
  mutate(date = date_decimal(time, tz = "UTC")) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d", tz = "UTC")) %>%
  select(treeId, date, startLocation, endLocation) %>%
  mutate(startLocation = ifelse(startLocation %in% B11420_interesting_locations, startLocation, "Others")) %>%
  mutate(endLocation = ifelse(endLocation %in% B11420_interesting_locations, endLocation, "Others")) %>%
  mutate(date = as.Date(cut(date, breaks = "1 week", start.on.monday = FALSE))) %>%
  select(date, startLocation, endLocation) %>%
  group_by(date, startLocation, endLocation) %>%
  summarise(value = n()) %>%
  ungroup() %>%
  arrange(date) %>%
  mutate(event = paste0(startLocation, " to ", endLocation)) %>%
  select(event, date, value) %>%
  complete(., event, date, fill = list(value = 0)) %>%
  filter(event %in% B11420_important_interactions) %>%
  filter(date >= as.Date("2020-01-01")) %>%
  arrange(date) %>%
  group_by(event) %>%
  mutate(running_total = cumsum(value)) %>%
  ungroup() %>%
  mutate(event = factor(event, levels = B11420_important_interactions))

# create plots with insets ------------------------------------------------

B1416_main_plot <-
  ggplot(
    data = B1416_total_tree_travel_history,
    aes(x = date)
  ) +
  geom_point(
    aes(
      y = running_total,
      color = event
    ),
    size = 2
  ) +
  geom_step(
    aes(
      y = running_total,
      color = event
    ),
    size = 1
  ) +
  theme_classic() +
  ylab("Cumulative Markov Jumps") +
  xlab("Date") +
  scale_x_date(
    date_labels = "%b-%Y",
    breaks = seq.Date(ymd("2020-01-01"), ymd("2021-04-01"), by = "month"),
    limits = c(ymd("2020-01-01"), ymd("2021-04-30"))
  ) +
  theme(
    axis.text.x = element_text(size = 16, angle = 90, face = "bold", color = "black", vjust = 1.75, hjust = 1.0),
    axis.title.x = element_text(size = 18, face = "bold", color = "black", vjust = -1),
    axis.text.y = element_text(size = 16, face = "bold", color = "black"),
    axis.title.y = element_text(size = 18, face = "bold", color = "black"),
    legend.position = c(0.3, 0.8),
    legend.text = element_text(size = 14, face = "bold"),
    legend.title = element_blank()
  ) +
  annotate(
    "rect",
    xmin = ymd("2020-01-01"),
    xmax = ymd("2020-03-25"),
    ymin = -5000,
    ymax = 10000,
    fill = "transparent",
    color = "darkgrey",
    size = 0.75,
    linetype = "dashed"
  )

B1416_inset_plot <-
  B1416_main_plot +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 12, angle = 90, face = "bold", color = "black", vjust = 1.75, hjust = 1.0),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA)
  ) +
  scale_y_continuous(limits = c(0, 2000)) +
  scale_x_date(
    date_labels = "%b-%Y",
    breaks = seq.Date(ymd("2020-01-01"), ymd("2020-03-30"), by = "month"),
    labels = seq.Date(ymd("2020-01-01"), ymd("2020-02-30"), by = "month"),
    limits = c(ymd("2020-01-01"), ymd("2020-03-25"))
  )

B1416_plot_with_inset <-
  ggdraw() +
  draw_plot(B1416_main_plot) +
  draw_plot(B1416_inset_plot, x = 0.15, y = 0.275, width = 0.2, height = 0.4)

B11420_main_plot <-
  ggplot(
    data = B11420_total_tree_travel_history,
    aes(x = date)
  ) +
  geom_point(
    aes(
      y = running_total,
      color = event
    ),
    size = 2
  ) +
  geom_step(
    aes(
      y = running_total,
      color = event
    ),
    size = 1
  ) +
  theme_classic() +
  ylab("Cumulative Markov Jumps") +
  xlab("Date") +
  scale_x_date(
    date_labels = "%b-%Y",
    breaks = seq.Date(ymd("2020-01-01"), ymd("2021-04-01"), by = "month"),
    limits = c(ymd("2020-01-01"), ymd("2021-04-30"))
  ) +
  theme(
    axis.text.x = element_text(size = 16, angle = 90, face = "bold", color = "black", vjust = 1.75, hjust = 1.0),
    axis.title.x = element_text(size = 18, face = "bold", color = "black", vjust = -1),
    axis.text.y = element_text(size = 16, face = "bold", color = "black"),
    axis.title.y = element_text(size = 18, face = "bold", color = "black"),
    legend.position = c(0.3, 0.8),
    legend.text = element_text(size = 14, face = "bold"),
    legend.title = element_blank()
  ) +
  annotate(
    "rect",
    xmin = ymd("2020-01-01"),
    xmax = ymd("2020-07-01"),
    ymin = -2000,
    ymax = 3000,
    fill = "transparent",
    color = "darkgray",
    size = 0.75,
    linetype = "dashed"
  )

B11420_inset_plot <-
  B11420_main_plot + 
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 12, angle = 90, face = "bold", color = "black", vjust = 1.55, hjust = 1.0),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA)
  ) +
  scale_y_continuous(limits = c(0, 800)) +
  scale_x_date(
    date_labels = "%b-%Y",
    breaks = seq.Date(ymd("2020-01-01"), ymd("2020-06-30"), by = "month"),
    labels = seq.Date(ymd("2020-01-01"), ymd("2020-06-30"), by = "month"),
    limits = c(ymd("2020-01-01"), ymd("2020-06-25"))
  )

B11420_plot_with_inset <-
  ggdraw() +
  draw_plot(B11420_main_plot) +
  draw_plot(B11420_inset_plot, x = 0.2, y = 0.275, width = 0.3, height = 0.4)

# arrange plots -----------------------------------------------------------

Fig_S4 <-
  plot_grid(
    B1416_plot_with_inset,
    B11420_plot_with_inset,
    labels = "AUTO",
    rel_heights = c(0.8, 0.8),
    rel_widths = c(0.8, 0.8),
    ncol = 2,
    nrow = 1,
    axis = "tblr",
    label_size = 20,
    vjust = 1,
    greedy = TRUE
  )

# Fig_S4

# export image ------------------------------------------------------------

ggsave(
  plot = Fig_S4,
  filename = "Fig_S4.svg",
  device = "svg",
  scale = 2,
  width = 8,
  height = 4,
  units = c("in"),
  dpi = 1200,
  limitsize = FALSE,
  bg = NULL
)
