
library(ggrepel)
library(ggplot2)
library(tidyverse)

# load data
load("~/Documents/supply_chains/data/gta.Rdata")
gta <- master; rm(master)

# chart 1: number of new subsidies
gta_1 <- gta %>% 
  mutate(is_subsidy = if_else(`Intervention Type` %in%
                                c("Capital injection and equity stakes (including bailouts)", 
                                  "Financial grant", 
                                  "In-kind grant",
                                  "Interest payment subsidy", 
                                  "Production subsidy", 
                                  "State loan", 
                                  "Tax or social insurance relief"), 
                              1, 
                              0)) %>% 
  filter(is_subsidy == 1) %>% 
  mutate(year = year(`Inception Date`)) %>% 
  select(`Implementing Jurisdiction`, year, `Intervention ID`) %>% 
  unique() %>% 
  na.omit() %>% 
  mutate(is_highincome = if_else(`Implementing Jurisdiction` %in%
                                   c("Australia", "Austria", "Belgium", 
                                     "Bermuda", "Bulgaria", "Canada", 
                                     "Chile", "Croatia", "Cyprus", "Czechia", 
                                     "Denmark", "Estonia", "Faeroe Islands", 
                                     "Finland", "France", "Germany",
                                     "Greece", "Hong Kong", "Hungary", 
                                     "Iceland", "Ireland", "Israel", "Italy", 
                                     "Japan", "Republic of Korea", "Kuwait", 
                                     "Latvia", "Luxembourg", "Lithuania", 
                                     "Malta", "Monaco", "Nauru", "Netherlands", 
                                     "New Zealand", "Norway", "Oman", 
                                     "Palau", "Panama", "Poland", "Portugal", 
                                     "Puerto Rico", "Qatar", "Romania", 
                                     "Russia", "Singapore", "Seychelles",
                                     "Slovakia", "Slovenia", "Spain", 
                                     "Chinese Taipei", "United Arab Emirates", 
                                     "United Kingdom", 
                                     "United States of America", "Uruguay"), 
                                 "High income", 
                                 "Middle or low income")) %>% 
  group_by(is_highincome, year) %>% 
  summarise(count = n()) %>%
  filter(year > 2008, year < 2024) %>% 
  group_by(is_highincome) %>% 
  mutate(count = cumsum(count))

subsidies <- gta_1 %>% 
  mutate(label = if_else(year == 2023, 
                         is_highincome, 
                         NA)) %>%
  ggplot(aes(x = year, y = count, 
             group = is_highincome, colour = is_highincome)) + 
  # add grid lines
  geom_vline(xintercept = seq(2009, 2023, by = 2), 
             colour = "grey91", 
             linewidth = 0.6) + 
  geom_segment(data = tibble(y = seq(0, 18000, by = 2000), 
                             x1 = 2009, 
                             x2 = 2023), 
               aes(x = x1, xend = x2, y = y, yend = y), 
               inherit.aes = FALSE, 
               colour = "grey91", 
               linewidth = 0.6) +
  # add data lines
  geom_line(linewidth = 1.25) + 
  theme_minimal() + 
  xlab("Year") + 
  ylab("Subsidies since start of 2009") +
  theme(panel.grid = element_blank(), 
        axis.title.x = element_blank(), 
        # axis.title.x = element_text(hjust = 0.34), 
        # customise margins (top, right, bottom, left)
        plot.margin = unit(c(0, 0.4, 0.2, 0.2), units = "cm")) + 
  guides(colour = FALSE) + # hide legend
  # add labels
  geom_text_repel(aes(label = label),
                  hjust = 0, 
                  force = 1, 
                  direction = "y",
                  nudge_x = 0, 
                  xlim = c(2023, NA),
                  na.rm = TRUE, 
                  segment.linetype = "dotted") + 
  # accessible colour palette
  scale_colour_manual(values = palette.colors(palette = "R4")) +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(2009, 2030), 
                     breaks = seq(2009, 2023, by = 2)) 

ggsave("output/figures/subsidies.pdf", width = 12, height = 12, units = "cm")
