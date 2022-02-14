# Gregory S. Orf, Ph.D.
# Virus Discovery Group, Abbott Laboratories

# load libraries ----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(readxl)
library(ggtext)
library(viridisLite)
library(zoo)
library(ggrepel)

# load data ---------------------------------------------------------------

setwd("Data/")

cases_by_region <-
  read_excel(
    "Senegal_regional_case_count.xlsx"
  ) %>%
  rename(
    Cumulative_per_1000_res = "Cumulative cases per 1,000 residents",
  )

sequences_by_region <-
  read_excel(
    "Senegal_pango.xlsx"
  ) %>%
  select(
    Province,
    Collection_date.x,
    Pango_lineage
  ) %>%
  rename(
    Region = Province,
    Collection_date = Collection_date.x
  ) %>%
  mutate(
    Region = replace(Region, Region == "Thies", "Thiès")
  ) %>%
  mutate(
    Region = replace(Region, Region == "Sedhiou", "Sédhiou")
  ) %>%
  mutate(
    Region = replace(Region, Region == "Kedougou", "Kédougou")
  ) %>%
  mutate(
    Collection_date = as.Date(Collection_date, "%Y-%m-%d", tz = "UTC")
  ) %>%
  group_by(
    Region
  ) %>%
  summarise(
    Count = n()
  )
    
correlation_table_by_region <-
  cases_by_region %>%
  full_join(
    sequences_by_region,
    by = c("Region")
  ) %>%
  mutate_if(
    is.numeric,
    coalesce,
    0
  ) %>%
  rename(
    Sequences = Count
  ) %>%
  filter(
    Sequences > 0
  )

correlation_by_region <-
  cor.test(
    x = correlation_table_by_region$Cumulative_per_1000_res,
    y = correlation_table_by_region$Sequences,
    method = "kendall"
    ,exact = FALSE
  )

correlation_by_region
    
# plot --------------------------------------------------------------------

Fig_S2E <-
  ggplot() +
  theme_classic() + 
  geom_point(
    data = correlation_table_by_region,
    aes(
      x = Cumulative_per_1000_res,
      y = Sequences
    ),
    size = 4
  ) +
  geom_smooth(
    data = correlation_table_by_region,
    mapping = aes(
      x = Cumulative_per_1000_res,
      y = Sequences
    ),
    formula = y ~ x,
    method = lm,
    se = FALSE,
    fullrange = TRUE,
    color = "#009E73",
    size = 1.5
  ) +
  theme(
    axis.text.x = element_text(size = 14, face = "bold", color = "black"),
    axis.text.y = element_text(size = 14, face = "bold", color = "black"),
    axis.title.y = element_text(size = 18, face = "bold", color = "black"),
    axis.title.x = element_text(size = 18, face = "bold", color = "black"),
    legend.text = element_text(size = 14, face = "bold", color = "black"),
    legend.title = element_text(size = 18, face = "bold", color = "black"),
    legend.position = c(0.2, 0.8),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA)
  ) +
  annotate(
    geom = "text",
    x = 4,
    y = 50,
    label = paste(
      "tau =",
      round(correlation_by_region$estimate, 2),
      "\np-value =",
      round(correlation_by_region$p.value, 2)
    ),
    color = "#009E73",
    size = 8,
    fontface = "bold"
  ) +
  ylab("Cumulative Genomes Collected") +
  xlab("Cumulative Cases per 1,000 Residents") +
  geom_label_repel(
    data = correlation_table_by_region,
    aes(
      x = Cumulative_per_1000_res,
      y = Sequences,
      label = Region
    ),
    size = 4,
    label.size = 1,
    color = "black",
    fontface = "bold",
    segment.color = "black",
    segment.size = 1,
    box.padding = unit(0.75, "lines"),
    label.padding = unit(0.5, "lines")
  )

Fig_S2E

# save image --------------------------------------------------------------

ggsave(
  file = "Fig_S2E.svg",
  plot = Fig_S2E,
  device = "svg",
  scale = 3.1415,
  width = 800,
  height = 600,
  units = c("px"),
  limitsize = FALSE,
  bg = "transparent"
)
