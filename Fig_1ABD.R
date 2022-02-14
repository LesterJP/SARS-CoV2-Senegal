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

panel_A_B_data <-
  read_csv("Fig_1_owid-covid-data.csv") %>%
  filter(location == "Senegal") %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  mutate(Week = as.Date(cut(date, breaks = "1 week", start.on.monday = FALSE))) %>%
  select(Week, new_cases, new_deaths, new_vaccinations, reproduction_rate) %>%
  replace(is.na(.), 0) %>%
  group_by(Week) %>%
  summarise(
    Cases = sum(new_cases),
    Deaths = sum(new_deaths),
    Vaccinations = sum(new_vaccinations),
    Re = mean(reproduction_rate)
  ) %>%
  ungroup() %>%
  filter(Week <= as.Date("2021-06-05"))

panel_D_data <-
  read_excel("Fig_1_Senegal_pango.xlsx") %>%
  mutate(Collection_date.x = as.Date(Collection_date.x, "%Y-%m-%d", tz = "UTC")) %>%
  mutate(Month = as.Date(cut(Collection_date.x, breaks = "1 month", start.on.monday = FALSE))) %>%
  select(Month, Accession_ID, Pango_lineage) 

lineages_of_interest <-
  panel_D_data %>%
  group_by(Pango_lineage) %>%
  summarise(Value = n()) %>%
  ungroup() %>%
  arrange(desc(Value)) %>%
  slice_head(n = 9) %>%
  arrange(Pango_lineage) %>%
  pull(Pango_lineage)

panel_D_data <-
  panel_D_data %>%
  mutate(
    Pango_lineage = ifelse(
      Pango_lineage %in% lineages_of_interest,
      Pango_lineage,
      "Others"
    )
  )

# Panel A - Case load -----------------------------------------------------

coeff <-
  10

Fig_1A <-
  ggplot(
    panel_A_B_data,
    aes(x = Week)
  ) +
  geom_bar(
    aes(y = Cases),
    stat = "identity",
    fill = "darkmagenta",
    color = "black",
    alpha = 0.6,
    size = 0.75
  ) +
  geom_bar(
    aes(y = Deaths * coeff),
    stat = "identity",
    fill = "black",
    color = "black",
    alpha = 0.75,
    size = 0.75
  ) +
  scale_y_continuous(
    name = "New COVID-19 cases/week",
    expand = expansion(mult = c(0, 0.1)),
    sec.axis = sec_axis(
      ~ . / coeff,
      name = "New COVID-19 deaths/week"
    )
  ) +
  scale_x_date(
    date_labels = "%b-%Y",
    breaks = seq.Date(ymd("2020-03-02"), ymd("2021-06-01"), by = "month"),
    limits = c(ymd("2020-03-02"), ymd("2021-06-04"))
  ) +
  theme_classic() +
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
  )

Fig_1A

ggsave(
  file = "Fig_1A.svg",
  plot = Fig_1A,
  device = "svg",
  scale = 3.15,
  width = 800,
  height = 480,
  units = c("px"),
  limitsize = FALSE,
  bg = "transparent"
)

# Panel B - Effective Reproduction Number ---------------------------------

coeff2 <-
  40000

Fig_1B <-
  ggplot(
    panel_A_B_data,
    aes(x = Week)
  ) +
  geom_line(
    aes(y = Re),
    color = "darkmagenta",
    size = 2
  ) +
  geom_hline(
    yintercept = 1.0,
    show.legend = FALSE,
    color = "black",
    size = 0.5,
    linetype = 2
  ) +
  geom_bar(
    aes(y = Vaccinations / coeff2),
    stat = "identity",
    fill = "black",
    alpha = 0.75,
    color = "black",
    size = 0.5
  ) +
  scale_y_continuous(
    name = expression(bold(R[e])),
    limits = c(0, 2.0),
    breaks = seq(0, 2.0, by = 0.5),
    sec.axis = sec_axis(
      ~ . * coeff2,
      name = "Vaccinations/week",
      breaks = seq(0, 80000, by = 20000),
      labels = c("0", "20K", "40K", "60K", "80K")
    )
  ) +
  scale_x_date(
    date_labels = "%b-%Y",
    breaks = seq.Date(ymd("2020-03-02"), ymd("2021-06-01"), by = "month"),
    limits = c(ymd("2020-03-02"), ymd("2021-06-01"))
  ) +
  theme_classic() +
  xlab(
    "Date"
  ) +
  theme(
    axis.title.y = element_text(size = 18, face = "bold", color = "darkmagenta"),
    axis.title.y.right = element_text(size = 18, face = "bold", color = "black", vjust = 2),
    axis.title.x = element_text(size = 18, face = "bold", color = "black", vjust = -1),
    axis.text.x = element_text(size = 16, angle = 90, face = "bold", color = "black", vjust = 2.1),
    axis.text.y = element_text(size = 16, face = "bold", color = "darkmagenta"),
    axis.text.y.right = element_markdown(size = 16, face = "bold", color = "black"),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA)
  )

Fig_1B

ggsave(
  file = "Fig_1B.svg",
  plot = Fig_1B,
  device = "svg",
  scale = 3.15,
  width = 800,
  height = 480,
  units = c("px"),
  limitsize = FALSE,
  bg = "transparent"
)

# Panel D - PANGO Lineages ------------------------------------------------

Fig_1D <-
  ggplot(
    data = panel_C_data,
    mapping = aes(x = Month, fill = Pango_lineage)
  ) +
  geom_bar(
    position = "fill",
    width = 20,
    color = "black",
    size = 0.5
  ) +
  theme_classic() +
  scale_y_continuous(
    name = "Proportion of Genomes",
    limits = c(0, 1.0),
    breaks = seq(0, 1.0, by = 0.25)
  ) +
  scale_x_date(
    name = "Date",
    date_labels = "%b-%Y",
    breaks = seq.Date(ymd("2020-03-01"), ymd("2021-05-30"), by = "month"),
    limits = c(ymd("2020-02-05"), ymd("2021-06-01"))
  ) +
 scale_fill_manual(
    values = c(
      "lightgoldenrodyellow",
      "aquamarine3",
      "cornflowerblue",
      "olivedrab",
      "blue3",
      "deeppink1",
      "greenyellow",
      "orange",
      "darkred",
      "white"
    ),
    name = "Lineage"
  ) +
  theme(
    plot.margin = unit(c(t = 0, r = 40, b = 5, l = 4), "pt"),
    axis.text.x = element_text(color = "black", size = 16, angle = 90, face = "bold", hjust = 1, vjust = 0.5),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(color = "black", size = 16, face = "bold"),
    axis.title.x = element_text(color = "black", size = 18, face = "bold", vjust = -1),
    axis.title.y = element_text(color = "black", size = 18, face = "bold", vjust = 2),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(face = "bold", size = 14),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent", color = NA)
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
