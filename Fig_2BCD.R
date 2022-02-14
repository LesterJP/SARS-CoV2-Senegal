# Gregory S. Orf, Ph.D.
# Virus Discovery Group, Abbott Laboratories

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(readxl)
library(ggtext)
library(scales)

# Load data ---------------------------------------------------------------

setwd("Data/")

full.bsp <-
  read_excel(
    "Fig_2B.xlsx",
    sheet = 1,
    col_types = c("date", "numeric", "numeric", "numeric")
  ) %>%
  mutate(Date = as.Date(Date, "%Y-%M-%d", tz = "UTC"))

clade.bsp <-
  read_excel(
    "Fig_2C.xlsx",
    sheet = 6,
    col_types = c(
      "date", "numeric", "numeric", "numeric", "date", "numeric",
      "numeric", "numeric", "date", "numeric", "numeric", "numeric",
      "date", "numeric", "numeric", "numeric", "date", "numeric",
      "numeric", "numeric"
    )
  ) %>%
  mutate(Date1 = as.Date(Date1, "%Y-%M-%d", tz = "UTC")) %>%
  mutate(Date2 = as.Date(Date2, "%Y-%M-%d", tz = "UTC")) %>%
  mutate(Date3 = as.Date(Date3, "%Y-%M-%d", tz = "UTC")) %>%
  mutate(Date4 = as.Date(Date4, "%Y-%M-%d", tz = "UTC")) %>%
  mutate(Date5 = as.Date(Date5, "%Y-%M-%d", tz = "UTC"))

clade.Re <-
  read_excel(
    "Fig_2D.xlsx",
    sheet = 1,
    col_types = c(
      "date", "numeric", "numeric", "numeric", "date", "numeric",
      "numeric", "numeric", "date", "numeric", "numeric", "numeric",
      "date", "numeric", "numeric", "numeric", "date", "numeric",
      "numeric", "numeric"
    )
  ) %>%
  mutate(Date1 = as.Date(Date1, "%Y-%M-%d", tz = "UTC")) %>%
  mutate(Date2 = as.Date(Date2, "%Y-%M-%d", tz = "UTC")) %>%
  mutate(Date3 = as.Date(Date3, "%Y-%M-%d", tz = "UTC")) %>%
  mutate(Date4 = as.Date(Date4, "%Y-%M-%d", tz = "UTC")) %>%
  mutate(Date5 = as.Date(Date5, "%Y-%M-%d", tz = "UTC"))

# Panel B - Total BSP -----------------------------------------------------

Fig_2B <-
  ggplot(
    data = full.bsp,
    aes(x = Date, y = Mean, ymin = Lower, ymax = Upper)
  ) +
  geom_line(
    size = 2
  ) +
  geom_ribbon(
    alpha = 0.25,
    fill = "gray50"
  ) +
  scale_x_date(
    name = "Date",
    date_labels = "%b-%Y",
    breaks = seq.Date(ymd("2020-02-01"), ymd("2021-02-28"), by = "month"),
    limits = c(ymd("2020-02-01"), ymd("2021-02-15"))
  ) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(3e-02, 3e+03)
  ) +
  ylab(
    expression(bold(paste("Genetic Diversity (", N[e], tau, ")")))
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 16, angle = 90, face = "bold", color = "black", vjust = 1.75, hjust = 1),
    axis.text.y = element_markdown(size = 16, face = "bold", color = "black"),
    axis.title.y = element_text(size = 18, face = "bold", vjust = 2, color = "black"),
    axis.title.x = element_text(size = 18, face = "bold", vjust = -1, color = "black"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  ) +
  ggtitle(
    label = "Bayesian Skyline Plot:\nAll Genomes"
  )

Fig_2B

ggsave(
  file = "Fig_2B.svg",
  plot = Fig_2B,
  device = "svg",
  scale = 3.15,
  width = 600,
  height = 600,
  units = c("px"),
  limitsize = FALSE,
  bg = "transparent"
)


# Panel C - BSP Deconvolution ---------------------------------

line_width <-
  1.5
line_alpha <-
  1
ribbon_alpha <-
  0.25
line_colors <-
  c(
    "Clade I" = "goldenrod",
    "Clade II" = "olivedrab",
    "Clade III" = "blue3",
    "Clade IV" = "red3",
    "Clade V" = "deeppink1"
  )

Fig_2C <-
  ggplot() +
  geom_line(
    data = clade.bsp,
    aes(
      x = Date1, 
      y = Mean1, 
      color = "Clade I"
    ),
    size = line_width,
    alpha = line_alpha
  ) +
  geom_ribbon(
    data = clade.bsp,
    aes(
      x = Date1,
      y = Mean1,
      ymax = Max1,
      ymin = Min1
    ),
    alpha = ribbon_alpha,
    fill = "gold"
  ) +
  geom_line(
    data = clade.bsp,
    aes(
      x = Date2,
      y = Mean2,
      color = "Clade II"
    ),
    size = line_width,
    alpha = line_alpha,
  ) +
  geom_ribbon(
    data = clade.bsp,
    aes(
      x = Date2,
      y = Mean2,
      ymax = Max2,
      ymin = Min2
    ),
    alpha = ribbon_alpha,
    fill = "olivedrab"
  ) +
  geom_line(
    data = clade.bsp,
    aes(
      x = Date3,
      y = Mean3,
      color = "Clade III"
    ),
    size = line_width,
    alpha = line_alpha,
  ) +
  geom_ribbon(
    data = clade.bsp,
    aes(
      x = Date3,
      y = Mean3,
      ymax = Max3,
      ymin = Min3
    ),
    alpha = ribbon_alpha,
    fill = "cornflowerblue"
  ) +
  geom_line(
    data = clade.bsp,
    aes(
      x = Date4,
      y = Mean4,
      color = "Clade IV"
    ),
    size = line_width,
    alpha = line_alpha,
  ) +
  geom_ribbon(
    data = clade.bsp,
    aes(
      x = Date4,
      y = Mean4,
      ymax = Max4,
      ymin = Min4
    ),
    alpha = ribbon_alpha,
    fill = "salmon"
  ) +
  geom_line(
    data = clade.bsp,
    aes(
      x = Date5,
      y = Mean5,
      color = "Clade V"
    ),
    size = line_width,
    alpha = line_alpha,
  ) +
  geom_ribbon(
    data = clade.bsp,
    aes(
      x = Date5,
      y = Mean5,
      ymax = Max5,
      ymin = Min5
    ),
    alpha = ribbon_alpha,
    fill = "deeppink1"
  ) +
  scale_x_date(
    name = "Date",
    date_labels = "%b-%Y",
    breaks = seq.Date(
      ymd("2020-02-01"),
      ymd("2021-02-28"),
      by = "month"
    ),
    limits = c(
      ymd("2020-02-01"),
      ymd("2021-02-15")
    )
  ) +
  scale_y_log10(
    breaks = trans_breaks(
      "log10",
      function(x) 10^x
    ),
    labels = trans_format(
      "log10", 
      math_format(10^.x)
    ),
    limits = c(
      5e-03,
      5e+02
    )
  ) +
  scale_color_manual(values = line_colors) +
  ylab(expression(bold(paste("Genetic Diversity (", N[e], tau, ")")))) +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      size = 16, 
      angle = 90, 
      face = "bold", 
      color = "black",
      vjust = 2, 
      hjust = 1
    ),
    axis.text.y = element_markdown(
      size = 16, 
      face = "bold", 
      color = "black"
    ),
    axis.title.y = element_text(
      size = 18, 
      face = "bold", 
      vjust = 2, 
      color = "black"
    ),
    axis.title.x = element_text(
      size = 18, 
      face = "bold",
      vjust = -1, 
      color = "black"
    ),
    plot.title = element_text(
      size = 20,
      face = "bold",
      hjust = 0.5
    ),
    legend.direction = "vertical",
    legend.position = c(0.5, 0.225),
    legend.title = element_blank(),
    legend.text = element_text(
      color = "black",
      face = "bold",
      size = 16
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
      color = NA,
      fill = "transparent"
    )
  ) +
  ggtitle(label = "Bayesian Skyline Plot:\nDeconvolved")

Fig_2C

ggsave(
  filename = "Fig_2C_transparent.svg",
  plot = Fig_2C,
  device = "svg",
  scale = 3,
  width = 600,
  height = 600,
  units = c("px"),
  limitsize = TRUE,
  bg = "transparent"
)

# Panel D - Effective Re ------------------------------------------------

line_width <- 
  1.5
line_alpha <- 
  1
ribbon_alpha <- 
  0.25

line_colors <- c(
  "Clade I" = "goldenrod",
  "Clade II" = "olivedrab",
  "Clade III" = "blue3",
  "Clade IV" = "red3",
  "Clade V" = "deeppink1"
)

Fig_2D <- 
  ggplot() +
  geom_hline(
    yintercept = 1.0, 
    show.legend = FALSE,
    color = "black",
    size = 0.5, 
    linetype = 2
  ) +
  geom_line(
    data = clade.Re,
    aes(
      x = Date1,
      y = Mean1,
      color = "Clade I"
    ),
    size = line_width,
    alpha = line_alpha
  ) +
  geom_ribbon(
    data = clade.Re,
    aes(
      x = Date1,
      y = Mean1,
      ymax = Max1,
      ymin = Min1
    ),
    alpha = ribbon_alpha,
    fill = "gold"
  ) +
  geom_line(
    data = clade.Re,
    aes(
      x = Date2,
      y = Mean2,
      color = "Clade II"
    ),
    size = line_width,
    alpha = line_alpha,
  ) +
  geom_ribbon(
    data = clade.Re,
    aes(
      x = Date2,
      y = Mean2,
      ymax = Max2,
      ymin = Min2
    ),
    alpha = ribbon_alpha,
    fill = "olivedrab"
  ) +
  geom_line(
    data = clade.Re,
    aes(
      x = Date3,
      y = Mean3,
      color = "Clade III"
    ),
    size = line_width,
    alpha = line_alpha,
  ) +
  geom_ribbon(
    data = clade.Re,
    aes(
      x = Date3,
      y = Mean3,
      ymax = Max3,
      ymin = Min3
    ),
    alpha = ribbon_alpha,
    fill = "cornflowerblue"
  ) +
  geom_line(
    data = clade.Re,
    aes(
      x = Date4,
      y = Mean4,
      color = "Clade IV"
    ),
    size = line_width,
    alpha = line_alpha,
  ) +
  geom_ribbon(
    data = clade.Re,
    aes(
      x = Date4,
      y = Mean4,
      ymax = Max4,
      ymin = Min4
    ),
    alpha = ribbon_alpha,
    fill = "salmon"
  ) +
  geom_line(
    data = clade.Re,
    aes(
      x = Date5,
      y = Mean5,
      color = "Clade V"
    ),
    size = line_width,
    alpha = line_alpha,
  ) +
  geom_ribbon(
    data = clade.Re,
    aes(
      x = Date5,
      y = Mean5,
      ymax = Max5,
      ymin = Min5
    ),
    alpha = ribbon_alpha,
    fill = "deeppink1"
  ) +
  scale_x_date(
    name = "Date",
    date_labels = "%b-%Y",
    breaks = seq.Date(
      ymd("2020-02-01"),
      ymd("2021-02-28"),
      by = "month"
    ),
    limits = c(
      ymd("2020-02-01"),
      ymd("2021-02-15")
    )
  ) +
  scale_y_continuous(
    name = expression(bold(R[e])),
    limits = c(0, 4),
    breaks = seq(0, 4, by = 1.0)
  ) +
  scale_color_manual(values = line_colors) +
  ylab(expression(bold(R[e]))) +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      size = 16,
      angle = 90,
      face = "bold",
      color = "black",
      vjust = 1.75,
      hjust = 1
    ),
    axis.text.y = element_markdown(
      size = 16, 
      face = "bold", 
      color = "black"
    ),
    axis.title.y = element_text(
      size = 18, 
      face = "bold",
      vjust = 2, 
      color = "black"
    ),
    axis.title.x = element_text(
      size = 18, 
      face = "bold", 
      vjust = -1, 
      color = "black"
    ),
    plot.title = element_text(
      size = 20, 
      face = "bold", 
      hjust = 0.5
    ),
    legend.direction = "vertical",
    legend.position = c(0.2, 0.7),
    legend.title = element_blank(),
    legend.text = element_text(
      face = "bold",
      size = 16
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
      color = NA,
      fill = "transparent"
    )
  ) +
  ggtitle(label = "Effective Reproduction Number:\nDeconvolved")

Fig_2D

# save an svg -------------------------------------------------------------

ggsave(
  file = "Fig_2D.svg",
  plot = Fig_2D,
  device = "svg",
  scale = 3.15,
  width = 600,
  height = 600,
  units = c("px"),
  limitsize = FALSE,
  bg = "transparent"
)
