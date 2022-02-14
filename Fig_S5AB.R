# Gregory S. Orf, Ph.D.
# Virus Discovery Group, Abbott Laboratories

# load libraries ----------------------------------------------------------

library(treeio)
library(ggtree)
library(ggh4x)
library(cowplot)
library(readxl)
library(lubridate)
library(dplyr)

# load data ---------------------------------------------------------------

setwd("Data/")

metadata <- read_excel(
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

global_tree_iqtree <- read.iqtree("Fig_S5_rooted.refined.global.africa.senegal.ML.tree")
senegal_tree_iqtree <- read.iqtree("Fig_S5_rooted.refined.senegal.ML.tree")

# make trees --------------------------------------------------------------

global_tree <-
  ggtree(
    global_tree_iqtree,
    color = "gray40",
    size = 0.5
  ) +
  theme_tree2() +
  vexpand(0.05, direction = -1) +
  expand_limits(y = 5500) +
  xlab("Substitutions per Site") +
  scale_x_continuous(
    limits = c(0, 0.0020),
    breaks = c(0, 0.0005, 0.0010, 0.0015, 0.0020),
    minor_breaks = seq(from = 0, to = 0.0020, by = 0.0001),
    guide = "axis_minor"
  ) +
  theme(axis.text.x = element_text(size = 12, face = 2, color = "black")) +
  theme(axis.title.x = element_text(size = 18, face = 2, color = "black"))

global_tree <- global_tree %<+% metadata

PanelA <-
  global_tree +
  geom_tippoint(
    aes(
      fill = SelRegion,
      shape = SelRegion,
      size = SelRegion
    ),
    color = "black"
  ) +
  scale_fill_manual(
    values = c(
      "white",
      "skyblue",
      "darkmagenta"
    )
  ) +
  scale_shape_manual(
    values = c(
      21,
      21,
      21
    )
  ) +
  scale_size_manual(
    values = c(
      2,
      2,
      4
    )
  ) +
  # geom_text2(mapping = aes(subset = node %in% node & isTip == FALSE, label = node)) +
  # geom_text2(mapping = aes(subset = node %in% c(seq(from = 5233, to = 5400, by = 1)), label = paste(SH_aLRT, "/", UFboot))) +
  geom_cladelab(node = 7238, label = "VOC Alpha", textcolor = "red", barcolor = "red", barsize = 1, offset = 0.00003, fontface = 2) +
  geom_cladelab(node = 9319, label = "VOC Beta", textcolor = "goldenrod", barcolor = "goldenrod", barsize = 1, offset = 0.00003, fontface = 2) +
  geom_cladelab(node = 6390, label = "VOC Gamma", textcolor = "darkblue", barcolor = "darkblue", barsize = 1, offset = 0.00003, extend = 50, fontface = 2) +
  geom_cladelab(node = 8558, label = "VOC Delta", textcolor = "darkgreen", barcolor = "darkgreen", barsize = 1, offset = 0.00003, extend = 50, fontface = 2) +
  geom_cladelab(node = 6620, label = "B.1.1.420", textcolor = "darkmagenta", barcolor = "darkmagenta", barsize = 1, offset = 0.00003, extend = 50, fontface = 2) +
  geom_cladelab(node = 7990, label = "B.1.416", textcolor = "darkmagenta", barcolor = "darkmagenta", barsize = 1, offset = 0.00003, extend = 50, fontface = 2) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(
      size = 12,
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
    ),
    ggh4x.axis.ticks.length.minor = rel(1),
  )

PanelA

senegal_tree <-
  ggtree(
    senegal_tree_iqtree,
    # color = "gray40",
    size = 0.5
  ) +
  theme_tree2() +
  vexpand(0.05, direction = -1) +
  expand_limits(y = 250) +
  xlab("Substitutions per Site") +
  scale_x_continuous(
    limits = c(0, 0.0020),
    breaks = c(0, 0.0005, 0.0010, 0.0015, 0.0020),
    minor_breaks = seq(from = 0, to = 0.0020, by = 0.0001),
    guide = "axis_minor"
  ) +
  theme(axis.text.x = element_text(size = 12, face = 2, color = "black")) +
  theme(axis.title.x = element_text(size = 18, face = 2, color = "black"))

senegal_tree <- senegal_tree %<+% metadata 

custom_palette <- c("lightgoldenrodyellow", "darkgoldenrod1", "navajowhite1", "navy",
                    "lightsalmon", "aquamarine3", "cornflowerblue", "olivedrab","blue3",
                    "yellow", "darkslategray2","deeppink1", "greenyellow", 
                    "mediumpurple4", "dimgrey", "chocolate4", "darkmagenta",
                    "darkred", "hotpink4", "red3", "orchid4",
                    "sandybrown", "plum1", "seashell3"
                  )

PanelB <-
  senegal_tree +
  geom_tippoint(
    aes(
      fill = SelRegion,
      shape = SelRegion,
      size = SelRegion
    ),
    color = "black"
  ) +
  scale_fill_manual(
    values = c(
      "white",
      # "skyblue",
      "darkmagenta"
    )
  ) +
  scale_shape_manual(
    values = c(
      21,
      # 21,
      21
    )
  ) +
  scale_size_manual(
    values = c(
      2,
      # 2,
      4
    )
  ) +
  # geom_tippoint(
  #   aes(
  #     fill = Pango_lineage
  #   ),
  #   shape = 21,
  #   size = 4
  #   ) +
  # geom_tree(
  #   mapping = aes(
  #     color = Pango_lineage
  #   )
  # ) +
  # scale_color_manual(
  #   name = "PANGO Lineage",
  #   values = custom_palette,
  #   na.translate = FALSE,
  #   guide = guide_legend(
  #     override.aes = list(
  #       size = 5
  #     )
  #   )
  # ) +
  # geom_nodepoint(
  #   mapping = aes(
  #     subset = SH_aLRT >= 60 & UFboot >= 60
  #   ),
  #   size = 2,
  #   shape = 21,
  #   color = "black",
  #   fill = "black",
  #   show.legend = NA
  # ) +
  # geom_text2(mapping = aes(subset = node %in% node & isTip == FALSE, label = node)) +
  # geom_text2(mapping = aes(subset = node %in% c(seq(from = 5233, to = 5400, by = 1)), label = paste(SH_aLRT, "/", UFboot))) +
  geom_cladelab(node = 251, label = "B.1.1.420", textcolor = "darkmagenta", barcolor = "darkmagenta", barsize = 1, offset = 0.00003, fontface = 2) +
  geom_cladelab(node = 319, label = "B.1.416", textcolor = "darkmagenta", barcolor = "darkmagenta", barsize = 1, offset = 0.00003, fontface = 2) +
  geom_cladelab(node = 418, label = "VOC Alpha", textcolor = "red", barcolor = "red", barsize = 1, offset = 0.00003, fontface = 2, extend = 5) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    # legend.title = element_text(
    #   size = 12,
    #   face = "bold"
    # ),
    legend.text = element_text(
      size = 12,
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
    ),
    ggh4x.axis.ticks.length.minor = rel(1),
  )

PanelB

# arrange panels ----------------------------------------------------------

Fig_S5 <- 
  plot_grid(
    PanelA,
    PanelB,
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

Fig_S5

# save an svg -------------------------------------------------------------

ggsave(
  Fig_S5,
  file = "Fig_S5.svg",
  device = "svg",
  height = 1200,
  width = 2000,
  scale = 2,
  limitsize = FALSE,
  unit = c("px")
)
