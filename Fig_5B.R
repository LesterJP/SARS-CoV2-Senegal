# Gregory S. Orf, Ph.D.
# Virus Discovery Group, Abbott Laboratories

# load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl)
library(ggpubr)
library(grid)

# load data ---------------------------------------------------------------

setwd("Data/")

WG_NUC <- 
  read_excel(
    "Fig_5B_WG_NUC.xlsx",
    col_names = TRUE,
    col_types = c("numeric", "text", "text", "text")
  )

WG_AA <- 
  read_excel(
    "Fig_5B_WG_AA.xlsx",
    col_names = TRUE,
    col_types = c("numeric", "text", "text", "text")
  )

SPIKE_NUC <-
  read_excel(
    "Fig_5B_SPIKE_NUC.xlsx",
    col_names = TRUE,
    col_types = c("numeric", "text", "text", "text")
  )

SPIKE_AA <- 
  read_excel(
    "Fig_5B_SPIKE_AA.xlsx",
    col_names = TRUE,
    col_types = c("numeric", "text", "text", "text")
  )

My_Theme <- 
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(
      size = 10,
      angle = 45,
      vjust = 0.5
    ),
    axis.text.y = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black")
  )

# create plots ------------------------------------------------------------

a <- 
  ggplot(
    WG_NUC,
    aes(
      x = Cohort,
      y = Substitutions,
      fill = Cohort
    )
  ) +
  geom_violin(
    trim = FALSE,
    size = 0.5
  ) +
  stat_summary(
    fun.data = mean_sdl,
    geom = "pointrange",
    position = "identity",
    fun.args = list(mult = 1),
    color = "black",
    size = 0.5
  ) +
  ylim(-10, 65) +
  theme(
    plot.margin = unit(
      c(2,0.25,0,0.25),
      "cm"
    )
  ) +
  theme(
    axis.title.y = element_text(
      size = 10,
      vjust = 3
    )
  ) +
  My_Theme

b <- 
  ggplot(
    WG_AA,
    aes(
      x = Cohort,
      y = Substitutions,
      fill = Cohort
    )
  ) +
  geom_violin(
    trim = FALSE,
    size = 0.5
  ) +
  stat_summary(
    fun.data = mean_sdl,
    geom = "pointrange",
    position = "identity",
    fun.args = list(mult = 1),
    color = "black",
    size = 0.5
  ) +
  ylim(-10, 65) +
  theme(
    plot.margin = unit(
      c(2,0.25,0,0.75),
      "cm"
    )
  ) +
  theme(
    axis.title.y = element_blank()
  ) +
  My_Theme

c <- 
  ggplot(
    SPIKE_NUC,
    aes(
      x = Cohort,
      y = Substitutions,
      fill = Cohort
    )
  ) +
  geom_violin(
    trim = FALSE,
    size = 0.5
  ) +
  stat_summary(
    fun.data = mean_sdl,
    geom = "pointrange",
    position = "identity",
    fun.args = list(mult = 1),
    color = "black",
    size = 0.5
  ) +
  ylim(-10, 65) +
  theme(
    plot.margin = unit(
      c(2,0.25,0,0.75),
      "cm"
      ),
    axis.title.y = element_blank()
  ) +
  My_Theme

d <-
  ggplot(
    SPIKE_AA,
    aes(
      x = Cohort,
      y = Substitutions,
      fill = Cohort
    )
  ) +
  geom_violin(
    trim = FALSE,
    size = 0.5
  ) +
  stat_summary(
    fun.data = mean_sdl,
    geom = "pointrange",
    position = "identity",
    fun.args = list(mult = 1),
    color = "black",
    size = 0.5
  ) +
  ylim(-10, 65) +
  theme(
    plot.margin = unit(
      c(2,0.25,0,0.75),
      "cm"
    )
  ) +
  theme(
    axis.title.y = element_blank()
  ) +
  My_Theme

e <- 
  ggarrange(
    a,
    b,
    c,
    d,
    ncol = 4,
    nrow = 1
  )

e

# save an svg -------------------------------------------------------------

ggsave(
  filename = "Fig_5B.svg",
  plot = e,
  device = "svg",
  scale = 3.15,
  width = 1200,
  height = 600,
  limitsize = FALSE,
  bg = "transparent"
)
