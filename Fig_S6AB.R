# Gregory S. Orf, Ph.D.
# Virus Discovery Group, Abbott Laboratories

# load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(ggpubr)
library(ggtext)
library(ggh4x)
library(cowplot)
library(zoo)

# load data ---------------------------------------------------------------

setwd("Data/")

metadata <-
  read_excel(
    "20210707.curated.metadata.xlsx",
    1,
    col_types = c(
      "text",
      "text",
      "text",
      "text",
      "date",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "date",
      "text",
      "text",
      "text"
    )
  )

senegal_data <-
  read.csv(
    "Fig_S6_TempEst_senegal_refinedML.tsv",
    sep = "\t",
    header = TRUE
  ) %>%
  mutate(date = date_decimal(date)) %>%
  mutate(date = as.Date(cut(date, breaks = "day", start.on.monday = FALSE))) %>%
  inner_join(metadata, by = c("tip" = "Taxon")) %>%
  select(
    tip,
    date,
    distance,
    residual,
    SelRegion,
    Region,
    Country,
    Pango_lineage
  )

global_data <-
  read.csv(
    "Fig_S6_TempEst_all_refinedML.tsv",
    sep = "\t",
    header = TRUE
  ) %>%
  mutate(date = date_decimal(date)) %>%
  mutate(date = as.Date(cut(date, breaks = "day", start.on.monday = FALSE))) %>%
  inner_join(metadata, by = c("tip" = "Taxon")) %>%
  select(
    tip,
    date,
    distance,
    residual,
    SelRegion,
    Region,
    Country,
    Pango_lineage
  )

# create panel A ------------------------------------------------------------

plotA <-
  ggplot(
    data = global_data,
    mapping = aes(
      x = date,
      y = distance
      # alpha = SelRegion,
      # fill = SelRegion
    )
  ) +
  geom_point(
    mapping = aes(
      # alpha = SelRegion,
      fill = SelRegion
    ),
    shape = 21,
    color = "black",
    size = 4
  ) +
  geom_point(
    data = base::subset(global_data, SelRegion == "Senegal"),
    mapping = aes(
      # alpha = SelRegion,
      fill = SelRegion
    ),
    shape = 21,
    color = "black",
    size = 4
  ) +
  theme_classic() +
  scale_x_date(
    date_labels = "%d %b\n%Y",
    breaks = "3 months",
    minor_breaks = "1 month",
    limits = c(
      as.Date("2019-12-18"),
      as.Date("2021-06-30")
    ),
    guide = "axis_minor"
  ) +
  scale_y_continuous(
    limits = c(0, 0.0018),
    breaks = seq(0, 0.0018, by = 0.0006)
  ) +
  xlab(
    "Date"
  ) +
  ylab(
    "Root-to-Tip Distance"
  ) +
  theme(
    axis.title.y = element_text(size = 14, face = "bold", color = "black", vjust = 3, hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold", vjust = -1.5, color = "black"),
    axis.text.x = element_text(size = 12, face = "bold", hjust = 0.5, vjust = -1, color = "black"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent"),
    legend.position = c(0.2, 0.85),
    legend.title.align = 0.5,
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10, face = "bold"),
    ggh4x.axis.ticks.length.minor = rel(1),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) +
  scale_fill_manual(
    expression(bold(underline("Collection Region"))),
    values = c(
      "Countries outside Africa" = "gray90",
      "Countries within Africa" = "skyblue",
      "Senegal" = "darkmagenta"
    )
  ) +
  # scale_alpha_manual(
  #   "Collection Region",
  #   guide = "none",
  #   values = c("Countries outside Africa" = 1, "Countries within Africa" = 1, "Senegal" = 1)
  # ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    color = "black",
    fullrange = TRUE
  ) +
  annotate(
    geom = "text",
    x = as.Date("2021-01-15"),
    y = 0.0002,
    label = "R = 0.72",
    color = "black",
    fontface = 2,
    size = 4,
    hjust = 0,
    vjust = 0.5,
    na.rm = TRUE
  ) +
  annotate(
    geom = "text",
    x = as.Date("2021-01-15"),
    y = 0.0001333,
    label = expression(bold("Adj. "~R^"2"*~"= 0.52")),
    color = "black",
    size = 4,
    hjust = 0,
    vjust = 0.6,
    na.rm = TRUE
  ) +
  annotate(
    geom = "text",
    x = as.Date("2021-01-15"),
    y = 0.00006667,
    label = paste("tMRCA = ", date(date_decimal(2019.7266, tz = "UTC"))),
    color = "black",
    fontface = 2,
    size = 4,
    hjust = 0,
    vjust = 1,
    na.rm = TRUE
  ) +
  annotate(
    geom = "text",
    x = as.Date("2021-01-15"),
    y = 0.00000,
    label = expression(bold("Rate = 6.5 x 10"^"-4"*~"sub yr"^"-1")),
    color = "black",
    fontface = 2,
    size = 4,
    hjust = 0,
    vjust = 0.85,
    na.rm = TRUE
  )

# plotA

# create panel B ----------------------------------------------------------

plotB <-
  ggplot(
    data = senegal_data,
    mapping = aes(
      x = date,
      y = distance
      # alpha = SelRegion,
      # fill = SelRegion
    )
  ) +
  geom_point(
    mapping = aes(
      # alpha = SelRegion,
      fill = SelRegion
    ),
    shape = 21,
    color = "black",
    size = 4
  ) +
  theme_classic() +
  scale_x_date(
    date_labels = "%d %b\n%Y",
    breaks = "3 months",
    minor_breaks = "1 month",
    limits = c(
      as.Date("2019-12-18"),
      as.Date("2021-06-30")
    ),
    guide = "axis_minor"
  ) +
  scale_y_continuous(
    limits = c(0, 0.0018),
    breaks = seq(0, 0.0018, by = 0.0006)
  ) +
  xlab(
    "Date"
  ) +
  ylab(
    "Root-to-Tip Distance"
  ) +
  theme(
    axis.title.y = element_text(size = 14, face = "bold", color = "black", vjust = 3, hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold", vjust = -1.5, color = "black"),
    axis.text.x = element_text(size = 12, face = "bold", hjust = 0.5, vjust = -1, color = "black"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = "transparent"),
    legend.position = c(0.21, 0.87),
    legend.title.align = 0.5,
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10, face = "bold"),
    ggh4x.axis.ticks.length.minor = rel(1),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) +
  scale_fill_manual(
    expression(bold(underline("Collection Region"))),
    values = c(
      "Countries outside Africa" = "gray90",
      # "Countries within Africa" = "skyblue",
      "Senegal" = "darkmagenta"
    )
  ) +
  # scale_alpha_manual(
  #   "Collection Region",
  #   guide = "none",
  #   values = c("Countries outside Africa" = 1, "Countries within Africa" = 1, "Senegal" = 1)
  # ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    color = "black",
    fullrange = TRUE
  ) +
  annotate(
    geom = "text",
    x = as.Date("2019-12-18"),
    y = 0.0013,
    label = "R = 0.63",
    color = "black",
    fontface = 2,
    size = 4,
    hjust = 0,
    vjust = 0.5,
    na.rm = TRUE
  ) +
  annotate(
    geom = "text",
    x = as.Date("2019-12-18"),
    y = 0.0012333,
    label = expression(bold("Adj. "~R^"2"*~"= 0.39")),
    color = "black",
    size = 4,
    hjust = 0,
    vjust = 0.6,
    na.rm = TRUE
  ) +
  annotate(
    geom = "text",
    x = as.Date("2019-12-18"),
    y = 0.00116667,
    label = paste("tMRCA = ", date(date_decimal(2019.7787, tz = "UTC"))),
    color = "black",
    fontface = 2,
    size = 4,
    hjust = 0,
    vjust = 1,
    na.rm = TRUE
  ) +
  annotate(
    geom = "text",
    x = as.Date("2019-12-18"),
    y = 0.0011,
    label = expression(bold("Rate = 6.3 x 10"^"-4"*~"sub yr"^"-1")),
    color = "black",
    fontface = 2,
    size = 4,
    hjust = 0,
    vjust = 0.85,
    na.rm = TRUE
  )

# plotB

# arrange panels ----------------------------------------------------------

Fig_S6 <-
  plot_grid(
    plotA,
    plotB,
    labels = "AUTO",
    rel_heights = c(0.9, 0.9),
    rel_widths = c(0.9, 0.9),
    ncol = 2,
    nrow = 1,
    axis = "tblr",
    label_size = 20,
    vjust = 1,
    greedy = TRUE
  )

Fig_S6

# save an svg -------------------------------------------------------------

ggsave(
  plot = Fig_S6,
  filename = "Fig_S6.svg",
  device = "svg",
  scale = 3.15,
  height = 800,
  width = 1600,
  limitsize = FALSE
)
