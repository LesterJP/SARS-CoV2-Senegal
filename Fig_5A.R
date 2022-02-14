# Gregory S. Orf, Ph.D.
# Virus Discovery Group, Abbott Laboratories

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl)
library(ggrepel)
library(lubridate)
library(treeio)
library(ggtree)

# Load data ---------------------------------------------------------------
 
setwd("Data/")

Tree <- 
  read.beast("Fig_5A.tree")

Table1 <-
  read_excel(
    "Fig_5A_1.xlsx", 
    sheet = 1,
    col_types = c(
      "text", "text", "text", "text", "date",
      "text", "text", "text", "text", "text", 
      "text", "text", "text", "text", "text", 
      "text", "text", "date", "text", "text"
    )
  ) %>% 
  select(Taxon, Collection_date.x, Pango_lineage) %>%
  rename("samplingDate" = "Collection_date.x", "Lineages" = "Pango_lineage") %>%
  mutate(samplingDate = as.Date(samplingDate, "%Y-%m-%d", tz = "UTC"))

Metadata <- 
  read_excel("Fig_5A_2.xlsx") %>%
  rename("Taxon" = "Name") %>%
  inner_join(Table1, by = c("Taxon" = "Taxon")) %>%
  mutate(
    Metadata,
    Lineages = ifelse(
      Lineages == "B.1.416", "B.1.416",
      ifelse(Lineages == "B.1.1.420", "B.1.1.420", "Others")
    )
  ) %>%
  mutate(Lineages2 = Lineages) %>%
  mutate(Subtree = as.character(Subtree))
  

# Build tree --------------------------------------------------------------

custom_fills <- 
  c("deeppink1", "red3", "dimgrey")
custom_shapes <- 
  c(23, 22, 24, 25, 32)
custom_sizes <- 
  c(4, 4, 2)
special_nodes <- 
  c(439, 409, 328, 237, 241, 334)
  
P1 <- 
  ggtree(
    Tree, 
    mrsd = "2021-02-05",
    as.Date = TRUE, 
    color = "grey30", 
    size = 0.5
  ) %<+% Metadata

Fig_5A <-
  P1 + 
  theme_tree2() +
  scale_x_date(
    date_labels = "%b-%Y",
    date_breaks = "1 month",
  ) +
  vexpand(
    0.05,
    direction = -1
  ) +
  expand_limits(
    y = 250
  ) +
  xlab(
    "Date"
  ) +
  theme(
    axis.text.x = element_text(
      size = 14,
      angle = 90,
      face = 2, 
      color = "black", 
      hjust = 1,
      vjust = 3.1
    ),
    axis.title.x = element_text(
      size = 18,
      face = "bold",
      color = "black",
      vjust = -1
    ),
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black")
    ) +
  geom_tippoint(
    aes(
      shape = Subtree,
      fill = Lineages,
      size = Lineages2
    ),
    color = "black"
  ) +
  scale_shape_manual(
    values = custom_shapes,
    guide = guide_legend(
      override.aes = list(
        color = "black", 
        size = 4, 
        shape = custom_shapes
        )
      ),
    labels = c("1", "2", "3+5", "4", ""),
    na.translate = FALSE
  ) +
  scale_fill_manual(
    values = custom_fills,
    guide = guide_legend(
      override.aes = list(
        color = "black", 
        size = 4, 
        shape = 21, 
        fill = custom_fills
      )
    )
  ) +
  scale_size_manual(
    values = custom_sizes,
    guide = "none"
    ) +
  # geom_text2(aes(subset = !isTip, label = node)) + geom_tiplab() +
  geom_point2(
    aes(subset = (node %in% special_nodes)),
    size = 6,
    shape = 21,
    color = "black",
    fill = c("dimgrey", "red3", "dimgrey", "deeppink1", "dimgrey", "dimgrey")
    ) +
  theme(
    legend.position = c(0.825, 0.875),
    legend.box = "horizontal",
    legend.direction = "vertical",
    legend.key.height = unit(1, "line"),
    legend.title = element_text(
      size = 12,
      face = "bold",
      vjust = 0.5
    ),
    legend.title.align = 0.5,
    legend.text = element_text(
      size = 10,
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
    legend.key = element_rect(
      fill = "transparent",
      color = NA
    ),
    legend.box.background = element_rect(
      fill = "transparent",
      color = NA
    )
  )

 # Display and save tree ---------------------------------------------------

Fig_5A

ggsave(
  file = "Fig_5A.svg",
  plot = Fig_5A,
  device = "svg",
  scale = 3.15,
  width = 1200,
  height = 600,
  units = c("px"),
  limitsize = FALSE,
  bg = "transparent"
)
  