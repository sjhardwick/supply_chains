
library(ggrepel)
library(ggplot2)
library(tidyverse)

# load data
load("~/Documents/supply_chains/data/gta.Rdata")
gta <- master; rm(master)
br_0 <- read_csv("output_gamma0.csv")
br_2 <- read_csv("output_gamma2.csv")
br_5 <- read_csv("output_gamma5.csv")
br_10 <- read_csv("output_gamma10.csv")

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

# best responses
br_swapped <- br_2 %>%
  rename("tariff_b" = tariff_a,
         "tariff_a" = tariff_b,
         "welfare_b_2" = welfare_a,
         "welfare_a_2" = welfare_b) %>%
  select(-sum)

br_aves_2 <- br_2 %>%
  left_join(br_swapped, by = c("tariff_a", "tariff_b")) %>%
  mutate(welfare_a = (welfare_a + welfare_a_2)/2,
         welfare_b = (welfare_b + welfare_b_2)/2) %>%
  select(tariff_a, tariff_b, welfare_a, welfare_b)

brb_2 <- br_aves_2 %>%
  group_by(tariff_a) %>%
  slice_max(order_by = welfare_b, with_ties = FALSE)

preferred_2 <- br_aves_2 %>%
  filter(welfare_a > 19.13081 & welfare_b > 19.13081)

br_5_swapped <- br_5 %>%
  rename("tariff_b" = tariff_a,
         "tariff_a" = tariff_b,
         "welfare_b_2" = welfare_a,
         "welfare_a_2" = welfare_b)

br_aves_5 <- br_5 %>%
  left_join(br_5_swapped, by = c("tariff_a", "tariff_b")) %>%
  mutate(welfare_a = (welfare_a + welfare_a_2)/2,
         welfare_b = (welfare_b + welfare_b_2)/2) %>%
  select(tariff_a, tariff_b, welfare_a, welfare_b)

brb_5 <- br_aves_5 %>%
  group_by(tariff_a) %>%
  slice_max(order_by = welfare_b, with_ties = FALSE)

preferred_5 <- br_aves_5 %>%
  filter(welfare_a > 15.01944 & welfare_b > 15.01944)

# fit the loess model to B's best response (orange curve)
loess_fit_2 <- loess(tariff_b ~ tariff_a, data = brb_2)
loess_fit_5 <- loess(tariff_b ~ tariff_a, data = brb_5)

# generate a smooth sequence of points for prediction
tariff_a_seq <- seq(min(brb_2$tariff_a), max(brb_2$tariff_a), length.out = 500)
fitted_values_2 <- predict(loess_fit_2, 
                           newdata = data.frame(tariff_a = tariff_a_seq))
fitted_values_5 <- predict(loess_fit_5, 
                           newdata = data.frame(tariff_a = tariff_a_seq))

# create a data frame with the original loess predictions
fitted_data <- data.frame(
  tariff_a = tariff_a_seq,
  tariff_b_2 = fitted_values_2,
  tariff_b_5 = fitted_values_5
)

# mirror the curve by swapping tariff_a and tariff_b
mirrored_data <- data.frame(
  tariff_a_2 = fitted_data$tariff_b_2,  # swap x and y
  tariff_a_5 = fitted_data$tariff_b_5,
  tariff_b = fitted_data$tariff_a
)

# plot the original and mirrored curves
br_chart <- ggplot() +
  # original loess curves
  geom_point(data = fitted_data, 
             aes(x = tariff_a, y = tariff_b_2), 
             color = "blue4",
             size = 0.25) +
  geom_point(data = fitted_data, 
             aes(x = tariff_a, y = tariff_b_5), 
             color = "blue3",
             size = 0.25) +
  # mirrored curve
  geom_point(data = mirrored_data, 
             aes(x = tariff_a_2, y = tariff_b), 
             color = "purple4", 
             size = 0.25) +
  geom_point(data = mirrored_data, 
             aes(x = tariff_a_5, y = tariff_b), 
             color = "purple3", 
             size = 0.25) +
  # most efficient bundle
  annotate("point", x = 0.2, y = 0.2,
           color = "green4", 
           size = 2) +
  # line of preferred options
  #geom_segment(aes(x = 0.1, y = 0.1, xend = 0.48, yend = 0.48),
  #             colour = "#498500", linetype = 5,
  #             size = 1) +
  # annotations
  annotate("text", x = 0.12, y = 0.16, 
           label = "Optimum", 
           colour = "green4", fontface = "bold") +
  annotate("text", x = 0.61, y = 0.85, 
           label = "A's best response", 
           colour = "purple4", fontface = "bold") +
  annotate("text", x = 0.85, y = 0.54, 
           label = "B's best response", 
           colour = "blue4", fontface = "bold") +
  theme_minimal() +
  labs(x = "A's tariff rate", y = "B's tariff rate")

br_chart

####

br_0 <- read_csv("output_gamma0.csv")
br_2 <- read_csv("output_gamma2.csv")
br_5 <- read_csv("output_gamma5.csv")
br_10 <- read_csv("output_gamma10.csv")

br_swapped_0 <- br_0 %>%
  rename("tariff_b" = tariff_a, "tariff_a" = tariff_b,
         "welfare_b" = welfare_a, "welfare_a" = welfare_b)

br_swapped_2 <- br_2 %>%
  rename("tariff_b" = tariff_a, "tariff_a" = tariff_b,
         "welfare_b" = welfare_a, "welfare_a" = welfare_b)

br_swapped_5 <- br_5 %>%
  rename("tariff_b" = tariff_a, "tariff_a" = tariff_b,
         "welfare_b" = welfare_a, "welfare_a" = welfare_b)

br_swapped_10 <- br_10 %>%
  rename("tariff_b" = tariff_a, "tariff_a" = tariff_b,
         "welfare_b" = welfare_a, "welfare_a" = welfare_b)

br_both_0 <- br_0 %>% bind_rows(br_swapped_0)
br_both_2 <- br_2 %>% bind_rows(br_swapped_2)
br_both_5 <- br_5 %>% bind_rows(br_swapped_5)
br_both_10 <- br_10 %>% bind_rows(br_swapped_10)

data <- br_both_0

# Fit polynomial models for welfare_a and welfare_b
model_A <- lm(welfare_a ~ poly(tariff_a, 4) + poly(tariff_b, 4) + tariff_a:tariff_b, data = data)
model_B <- lm(welfare_b ~ poly(tariff_a, 4) + poly(tariff_b, 4) + tariff_a:tariff_b, data = data)

# Check summaries of the models
summary(model_A)
summary(model_B)

# Create a sequence of tariff values to predict responses
tariff_seq <- seq(0, 1, length.out = 100)

# Predict Player A's best responses given B's tariffs
best_response_a <- sapply(tariff_seq, function(tb) {
  optim(par = 0.5, fn = function(ta) -predict(model_A, newdata = data.frame(tariff_a = ta, tariff_b = tb)))$par
})

# Predict Player B's best responses given A's tariffs
best_response_b <- sapply(tariff_seq, function(ta) {
  optim(par = 0.5, fn = function(tb) -predict(model_B, newdata = data.frame(tariff_a = ta, tariff_b = tb)))$par
})

# Define a function to minimize the distance between the two best response curves
objective <- function(par) {
  ta <- par[1]
  tb <- par[2]
  
  # Calculate deviations from best responses
  dev_A <- ta - optim(par = 0.5, fn = function(x) -predict(model_A, newdata = data.frame(tariff_a = x, tariff_b = tb)))$par
  dev_B <- tb - optim(par = 0.5, fn = function(x) -predict(model_B, newdata = data.frame(tariff_a = ta, tariff_b = x)))$par
  
  # Return the sum of squared deviations
  return(dev_A^2 + dev_B^2)
}

# Use optim() to find the Nash equilibrium (intersection)
nash_eq <- optim(par = c(0.5, 0.5), fn = objective, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1))

# print the Nash equilibrium tariffs
cat("Nash Equilibrium Tariff A:", nash_eq$par[1], "\n")
cat("Nash Equilibrium Tariff B:", nash_eq$par[2], "\n")

# create data frames for both response curves
response_data_a <- data.frame(tariff_b = tariff_seq, tariff_a = best_response_a, group = "A's Best Response")
response_data_b <- data.frame(tariff_a = tariff_seq, tariff_b = best_response_b, group = "B's Best Response")

# Combine both response curves into one data frame
response_data <- rbind(response_data_a, response_data_b)

# find the Nash equilibrium point (intersection of best responses)
nash_eq <- data.frame(tariff_a = nash_eq$par[1], tariff_b = nash_eq$par[2])

# Predict welfare for both countries at Nash equilibrium
nash_welfare_a <- predict(model_A, newdata = nash_eq)
nash_welfare_b <- predict(model_B, newdata = nash_eq)

# generate a grid of tariff values and predict welfare
grid <- expand.grid(
  tariff_a = seq(0, 1, length.out = 100),
  tariff_b = seq(0, 1, length.out = 100)
)

# Predict welfare for both countries on the grid
grid$predicted_welfare_a <- predict(model_A, newdata = grid)
grid$predicted_welfare_b <- predict(model_B, newdata = grid)

# Step 3: Identify regions where both utilities exceed Nash equilibrium
grid$above_nash <- with(grid, 
                        predicted_welfare_a > nash_welfare_a & predicted_welfare_b > nash_welfare_b
)
data$above_nash <- with(data, 
                        welfare_a > nash_welfare_a & welfare_b > nash_welfare_b
)

ggplot() +
  # plot the shaded areas where both utilities are higher than nash
  geom_tile(data = data, aes(x = tariff_a, y = tariff_b, fill = above_nash), alpha = 0.5) +
  scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "#69b3a2")) +  # Green for above Nash
  # plot the best response curves
  geom_line(data = response_data, aes(x = tariff_a, y = tariff_b, color = group), size = 1) +
  # highlight the nash equilibrium
  geom_point(data = nash_eq, aes(x = tariff_a, y = tariff_b), color = "red", size = 4) +
  labs(
    title = "Best Response Curves with Nash Equilibrium and Welfare Areas",
    x = "Tariff A", y = "Tariff B", color = "Best Response", fill = "Above Nash Utility"
  ) +
  theme_minimal()

symmetric <- br_both_0 %>% mutate(sum = welfare_a + welfare_b) %>% filter(tariff_a == tariff_b)
