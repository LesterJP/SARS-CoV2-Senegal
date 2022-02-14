# Gregory S. Orf, Ph.D.
# Virus Discovery Group, Abbott Laboratories

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(broom)
library(readxl)
library(lubridate)
library(ggrepel)
library(rgdal)

# Load data ---------------------------------------------------------------

setwd("Data/")

Senegal_Map <-
  readOGR("Fig_1_sen_admbnda_adm1_1m_gov_ocha_20190426.shp", stringsAsFactors = FALSE)

Senegal_statistics <-
  read_excel("Fig_1_Senegal_statistics.xlsx", sheet = 1, col_types = c("text", "numeric", "numeric"))

# Merge metadata to map ---------------------------------------------------

Senegal_Map_tidy <-
  tidy(Senegal_Map, region = "ADM1_FR") %>%
  left_join(Senegal_statistics, by = c("id" = "id"))

region_names <-
  Senegal_Map_tidy %>%
  group_by(id) %>%
  summarise(long = mean(long), lat = mean(lat)) %>%
  ungroup()

# Display panel 1D --------------------------------------------------------

Fig_1D <-
  ggplot() +
  geom_polygon(
    data = Senegal_Map_tidy,
    aes(x = long, y = lat, group = group, fill = N235_Count),
    color = "black",
    size = 1
  ) +
  theme_void() +
  geom_label_repel(
    data = region_names,
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
    name = "Collected\nGenomes",
    colors = c("#ffdeff", "darkmagenta"),
    breaks = c(1, 40, 80, 120),
    labels = c("1", "40", "80", "120"),
    limits = c(1, 120),
    na.value = "white"
  ) +
  scale_color_discrete(
    na.translate = TRUE
  ) +
  theme(
    legend.direction = "vertical",
    legend.position = c(0.9, 0.8),
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(face = "bold", size = 16),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(color = NA, fill = "transparent")
  )

Fig_1D

ggsave(
  file = "Fig_1D.svg",
  plot = Fig_1D,
  device = "svg",
  scale = 3.15,
  width = 800,
  height = 480,
  units = c("px"),
  limitsize = FALSE,
  bg = "transparent"
)
