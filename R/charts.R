
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
  mutate(count = cumsum(count)) %>% 
  mutate(label = if_else(year == 2019, 
                         is_highincome, 
                         NA))

subsidies <- gta_1 %>% 
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
  geom_line(size = 1) + 
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
  geom_text_repel(data = gta_1 %>% filter(is_highincome == "High income"), 
                  aes(label = label),
                  hjust = 0.6, 
                  vjust = -10, 
                  force = 0, 
                  nudge_x = 0, 
                  nudge_y = 0,
                  fontface = "bold",
                  segment.color = "transparent", 
                  na.rm = TRUE) +
  geom_text_repel(data = gta_1 %>% filter(is_highincome == "Middle or low income"), 
                  aes(label = label),
                  hjust = 0.4, 
                  vjust = 6.5, 
                  force = 0, 
                  nudge_x = 0, 
                  nudge_y = 0,
                  segment.color = "transparent",
                  fontface = "bold",
                  colour = "#E69F00",
                  na.rm = TRUE) +
  # accessible colour palette
  scale_colour_manual(values = palette.colors(palette = "Okabe-Ito")) +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(2009, 2024), 
                     breaks = seq(2009, 2023, by = 2)) 

subsidies
ggsave("charts/subsidies.pdf", width = 10, height = 10, units = "cm")

# chart 2: instrument types

palette <- c("#abb0d0", rep("#062E8E", 11))

gta_2 <- gta %>% 
  mutate(year = year(`Inception Date`)) %>% 
  filter(year < 2024) %>% 
  select(`Intervention Type`, `Intervention ID`) %>%
  na.omit() %>%
  unique() %>% 
  group_by(`Intervention Type`) %>%
  summarise(count = n()) %>% 
  arrange(-count) %>%
  mutate(rank = row_number()) %>%
  mutate(`Intervention Type` = if_else(rank > 11, 
                                       "Other", 
                                       `Intervention Type`)) %>% 
  group_by(`Intervention Type`) %>%
  summarise(count = sum(count)) %>% 
  arrange(-count) %>% 
  rename("Intervention type" = `Intervention Type`) %>%
  mutate(`Intervention type` =
           factor(`Intervention type`, 
                  levels = c("Other", 
                             "Export tax", 
                             "Public procurement localisation", 
                             "Financial assistance in foreign market",
                             "Tax or social insurance relief",
                             "Anti-dumping",
                             "Loan guarantee",
                             "Local content incentive",
                             "Trade finance",
                             "Import tariff",
                             "State loan",
                             "Financial grant"))) %>% 
  mutate(label_1 = if_else(count > 6000, 
                           `Intervention type`, 
                           NA), 
         label_2 = if_else(count < 6000, 
                           `Intervention type`, 
                           NA))

instruments <- gta_2 %>%
  ggplot(aes(count, `Intervention type`)) + 
  theme_minimal() +
  geom_col(fill = palette) + 
  labs(x = NULL, y = NULL) +
  theme(axis.text.y = element_blank()) +
  geom_text(aes(label = label_1), 
            hjust = "right", nudge_x = -200,
            size = 4, colour = "white",
            fontface = "bold") +
  geom_text(aes(label = label_2), 
            hjust = "left", nudge_x = 200,
            size = 4, colour = "black",
            fontface = "bold")

instruments
ggsave("charts/instruments.pdf", width = 10, height = 10, units = "cm")

# chart 3: simulated subsidy welfare
ssw_data <- read_csv("data/subsidy_welfare.csv")

ssw_1 <- ssw_data %>%
  head(21) %>%
  pivot_longer(c(2:3), names_to = "type", values_to = "welfare") %>%
  mutate(welfare = welfare / 57.824 * 100, 
         label = if_else(type == "unilateral" &
                           rate == 0.17, 
                         "Last foreign firm exits", 
                         NA),
         label_1 = if_else(type == "unilateral" &
                             rate == 0, 
                           "Unilateral subsidy", 
                           NA), 
         label_2 = if_else(type == "symmetric" &
                             rate == 0, 
                           "Symmetric subsidy", 
                           NA)) %>%
  ggplot(aes(rate, welfare, group = type, colour = type)) +
  scale_colour_manual(values = c("#298bd1", "#e07222")) +
  theme_minimal() +
  geom_hline(yintercept = 100, linetype = "dashed") +
  geom_line(size = 1) + 
  xlab("Subsidy value per input") + 
  ylab("Expected global welfare") + 
  theme(legend.position = "none") +
  geom_text_repel(aes(label = label),
                  hjust = 1.2, 
                  vjust = 3, 
                  force = 0, 
                  na.rm = TRUE, 
                  segment.linetype = "dotted") +
  geom_text_repel(aes(label = label_1),
                  hjust = 0,
                  nudge_x = 0.003,
                  vjust = 4.1, 
                  force = 0, 
                  na.rm = TRUE, 
                  fontface = "bold",
                  segment.colour = "transparent") +
  geom_text_repel(aes(label = label_2),
                  hjust = -0.5, 
                  nudge_x = 0.003,
                  vjust = -3.3, 
                  force = 0, 
                  na.rm = TRUE, 
                  fontface = "bold",
                  segment.colour = "transparent") +
  scale_y_continuous(limits = c(75, 110))

ssw_1
ggsave("charts/welfare.pdf", width = 10, height = 10, units = "cm")

