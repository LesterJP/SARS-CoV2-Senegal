# Gregory S. Orf, Ph.D.
# Virus Discovery Group, Abbott Laboratories

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(broom)
library(readxl)
library(lubridate)
library(ggrepel)
library(rgdal)
library(viridisLite)
library(ggh4x)
library(cowplot)

# Load data ---------------------------------------------------------------

setwd("Data/")

Senegal_Map <-
  readOGR("SHP_files_senegal/sen_admbnda_adm1_1m_gov_ocha_20190426.shp", stringsAsFactors = FALSE)
Senegal_statistics <-
  read_excel("Fig_1_Senegal_statistics.xlsx", sheet = 1, col_types = c("text", "numeric", "numeric"))
stringency_index <-
  read.csv("Fig_S1_covid-stringency-index.csv") %>%
  filter(Entity %in% c("Senegal", "United States", "South Africa")) %>%
  mutate(Country = Entity) %>%
  mutate(Date = as.Date(ymd(Day))) %>%
  filter(Date <= ymd("2021-02-10")) %>%
  select(Date, Country, stringency_index)

# Merge metadata to map ---------------------------------------------------

Senegal_Map_tidy <-
  tidy(Senegal_Map, region = "ADM1_FR") %>%
  left_join(Senegal_statistics, by = c("id" = "id"))

state_names <-
  Senegal_Map_tidy %>%
  group_by(id) %>%
  summarise(long = mean(long), lat = mean(lat)) %>%
  ungroup()

# Display panel A --------------------------------------------

Fig_S1A <-
  ggplot() +
  geom_polygon(
    data = Senegal_Map_tidy,
    aes(x = long, y = lat, group = group, fill = Population),
    color = "black",
    size = 1
  ) +
  theme_void() +
  geom_label_repel(
    data = state_names,
    aes(x = long, y = lat, label = id),
    size = 6,
    label.size = 1,
    color = "black",
    fontface = "bold",
    segment.color = "black",
    segment.size = 1,
    box.padding = unit(0.75, "lines"),
    label.padding = unit(0.5, "lines")
  ) +
  scale_fill_gradientn(
    name = "Population",
    colors = turbo(
      n = n_distinct(Senegal_Map_tidy$id),
      alpha = 1,
      begin = 0.05,
      end = 1,
      direction = 1
    ),
    breaks = c(0, 1e+06, 2e+06, 3e+06, 4e+06),
    labels = c(0, "1M", "2M", "3M", "4M"),
    limits = c(0, 4e+06),
    na.value = "white",
    guide = "colorbar"
  ) +
  scale_color_discrete(
    na.translate = FALSE
  ) +
  theme(
    legend.direction = "vertical",
    legend.position = c(0.9, 0.8),
    legend.title = element_text(face = "bold", size = 16, hjust = 1, vjust = 2),
    legend.text = element_text(face = "bold", size = 14),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(color = NA, fill = "transparent")
  )

# Fig_S1A


# display panel B ---------------------------------------------------------

Fig_S1B <-
  ggplot(
    data = stringency_index,
    aes(x = Date, y = stringency_index)
  ) +
  geom_line(
    aes(
      color = Country,
      size = Country,
      linetype = Country
    ),
  ) +
  theme_classic() +
  ylab("Response Stringency Index") +
  xlab("Date") +
  scale_x_date(
    date_labels = "%d %b\n%Y",
    breaks = seq.Date(ymd("2020-02-01"), ymd("2021-02-10"), by = "3 months"),
    limits = c(ymd("2020-02-01"), ymd("2021-02-10")),
    minor_breaks = seq.Date(ymd("2020-02-01"), ymd("2021-02-10"), by = "1 month"),
    guide = "axis_minor"
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 20),
    limits = c(0, 100),
    minor_breaks = seq(0, 100, by = 10),
    guide = "axis_minor"
  ) +
  scale_color_manual(
    values = c("darkmagenta", "forestgreen", "black")
  ) +
  scale_size_manual(
    values = c(2, 0.5, 0.5)
  ) +
  scale_linetype_manual(
    values = c("solid", "dashed", "dashed")
  ) +
  theme(
    axis.text.x = element_text(size = 14, face = "bold", color = "black"),
    axis.text.y = element_text(size = 14, face = "bold", color = "black"),
    axis.text.y.right = element_text(size = 16, face = "bold", color = "black"),
    axis.title.y = element_text(size = 18, face = "bold", vjust = 2, color = "black"),
    axis.title.y.right = element_text(size = 18, face = "bold", vjust = 2, color = "black"),
    axis.title.x = element_text(size = 18, face = "bold", vjust = -1),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    ggh4x.axis.ticks.length.minor = rel(0.75)
  ) +
  annotate(
    geom = "point",
    x = as.Date("2020-03-15"),
    y = 0,
    size = 3,
    color = "black"
  ) +
  annotate(
    geom = "text",
    x = as.Date("2020-03-17"),
    y = 7,
    size = 3,
    label = "Int'l flights\nsuspended",
    hjust = 0,
    color = "black"
  ) +
  annotate(
    geom = "point",
    x = as.Date("2020-03-23"),
    y = 30,
    size = 3,
    color = "black"
  ) +
  annotate(
    geom = "text",
    x = as.Date("2020-03-25"),
    y = 39,
    size = 3,
    label = "State of emergency\ninstituted",
    hjust = 0,
    color = "black"
  ) +
  annotate(
    geom = "point",
    x = as.Date("2020-05-15"),
    y = 20,
    size = 3,
    color = "black"
  ) +
  annotate(
    geom = "text",
    x = as.Date("2020-05-17"),
    y = 27,
    size = 3,
    label = "Mosques/churches\nre-open",
    hjust = 0,
    color = "black"
  ) +
  annotate(
    geom = "point",
    x = as.Date("2020-06-02"),
    y = 10,
    size = 3,
    color = "black"
  ) +
  annotate(
    geom = "text",
    x = as.Date("2020-06-05"),
    y = 13,
    size = 3,
    label = "Interregional transport resumes",
    hjust = 0,
    color = "black"
  ) +
  annotate(
    geom = "point",
    x = as.Date("2020-08-08"),
    y = 20,
    size = 3,
    color = "black"
  ) +
  annotate(
    geom = "text",
    x = as.Date("2020-08-10"),
    y = 27,
    size = 3,
    label = "Mandatory masks and\ngathering restrictions",
    hjust = 0,
    color = "black"
  ) +
  annotate(
    geom = "point",
    x = as.Date("2020-07-15"),
    y = 0,
    size = 3,
    color = "black"
  ) +
  annotate(
    geom = "text",
    x = as.Date("2020-07-18"),
    y = 4,
    size = 3,
    label = "Air borders re-open",
    hjust = 0,
    color = "black"
  ) +
  annotate(
    geom = "point",
    x = as.Date("2020-11-12"),
    y = 10,
    size = 3,
    color = "black"
  ) +
  annotate(
    geom = "text",
    x = as.Date("2020-11-15"),
    y = 17,
    size = 3,
    label = "Schools fully\nre-open",
    hjust = 0,
    color = "black"
  ) +
  annotate(
    geom = "point",
    x = as.Date("2021-01-06"),
    y = 0,
    size = 3,
    color = "black"
  ) +
  annotate(
    geom = "text",
    x = as.Date("2021-01-08"),
    y = 7,
    size = 3,
    label = "New\ncurfews",
    hjust = 0,
    color = "black"
  )

Fig_S1B

# create panel ------------------------------------------------------------

Fig_S1 <-
  plot_grid(
    Fig_S1A,
    Fig_S1B,
    labels = "AUTO",
    rel_heights = c(0.8, 0.8),
    rel_widths = c(0.8, 0.8),
    ncol = 1,
    nrow = 2,
    axis = "tblr",
    label_size = 20,
    vjust = 1,
    greedy = TRUE
  )

Fig_S1

# save figure -------------------------------------------------------------

ggsave(
  file = "Fig_S1.svg",
  plot = Fig_S1,
  device = "svg",
  scale = 3.15,
  width = 1000,
  height = 1000,
  units = c("px"),
  limitsize = FALSE,
  bg = "transparent"
)
