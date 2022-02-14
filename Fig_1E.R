# Gregory S. Orf, Ph.D.
# Virus Discovery Group, Abbott Laboratories

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(readxl)
library(treeio)
library(ggtree)

# Read in files -----------------------------------------------------------

setwd("Data/")

Tree <-
  read.beast("Fig_1_20210629-MCC_global.tree")

Metadata <-
  read_excel(
    "20210707.curated.metadata.xlsx",
    sheet = 1,
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

# Display tree ------------------------------------------------------------

P0 <- 
  ggtree(
    Tree,
    mrsd = "2021-05-22",
    as.Date = TRUE,
    color = "gray30",
    # alpha = 0.5,
    size = 0.5
  ) +
  theme_tree2() +
  scale_x_date(
    date_labels = "%b-%Y",
    date_break = "1 month"
  ) +
  vexpand(0.05,
    direction = -1
  ) +
  expand_limits(y = 5500) +
  theme(
    axis.text.x = element_text(
      size = 16,
      angle = 90,
      face = 2,
      color = "black",
      hjust = 1,
      vjust = 1.5
    ),
    axis.title.x = element_text(
      size = 18,
      face = 2,
      vjust = -1
    )
  ) +
  xlab("Date")

# Attach metadata and subset taxa -----------------------------------------

P1 <- 
  P0 %<+% Metadata

Fig_1E <- 
  P1 + 
  geom_tippoint(
    aes(
      fill = SelRegion,
      shape = SelRegion,
      size = SelRegion
    ),
    color = "black"
  ) +
  scale_fill_manual(values = c("white", "skyblue", "darkmagenta")) +
  scale_shape_manual(values = c(21, 21, 21)) +
  scale_size_manual(values = c(4, 4, 6)) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(
      size = 16,
      face = "bold"
    ),
    panel.background = element_rect(
      fill = "transparent",
      color = NA
    ),
    plot.background = element_rect(
      fill = "transparent",
      color = NA
    ),
    legend.background = element_rect(
      fill = "transparent",
      color = NA
    ),
    legend.box.background = element_rect(
      fill = "transparent",
      color = NA
    )
  )

Fig_1E

# Save svg with 800x1200 px size ------------------------------------------

ggsave(
  file = "Fig_1E.svg",
  plot = Fig_1E,
  device = "svg",
  scale = 3.15,
  # width = 800,
  width = 1000,
  height = 1100,
  units = c("px"),
  limitsize = FALSE,
  bg = "transparent"
)
