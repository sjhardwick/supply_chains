
# load libraries and data -------------------------------------------------

# load libraries
library(akima) # for (bi)linear interpolation
library(ggplot2)
library(ggrepel)
library(rPref)
library(tidyverse)

# load data
prsub_hi <- read_csv("output/prod_subsidy_loop_high_risk.csv")
prsub_as <- read_csv("output/prod_subsidy_loop_asymmetric_risk.csv")
prsub_lo <- read_csv("output/prod_subsidy_loop_low_risk.csv")

# symmetric (low) risk ----------------------------------------------------

data_raw <- prsub_lo

data_swapped <- data_raw %>%
  rename("sub_B" = sub_A,
         "sub_A" = sub_B,
         "utility_A" = utility_B,
         "utility_B" = utility_A,
         "cvar_A" = cvar_B,
         "cvar_B" = cvar_A)

data <- bind_rows(data_raw, data_swapped) %>%
  group_by(sub_A, sub_B) %>%
  summarise(utility_A = mean(utility_A),
            utility_B = mean(utility_B),
            cvar_A = mean(cvar_A),
            cvar_B = mean(cvar_B)) %>%
  mutate(welfare_A = cvar_A,
         welfare_B = cvar_B)

# fit linear interpolation models

## define grid
grid_x <- seq(min(data$sub_A), max(data$sub_A), length.out = 86)  # sub_A
grid_y <- seq(min(data$sub_B), max(data$sub_B), length.out = 86)  # sub_B

## perform interpolation
interp_A <- interp(
  x = data$sub_A,       # x values (sub_A)
  y = data$sub_B,       # y values (sub_B)
  z = data$welfare_A,   # z values (welfare_A)
  xo = grid_x,          # interpolation points
  yo = grid_y,
  linear = TRUE         # force linear interpolation
)
interp_B <- interp(
  x = data$sub_A,       # x values (sub_A)
  y = data$sub_B,       # y values (sub_B)
  z = data$welfare_B,   # z values (welfare_B)
  xo = grid_x,          # interpolation points
  yo = grid_y,
  linear = TRUE         # force linear interpolation
)

# convert the interpolated results to a data frame
grid <- expand.grid(sub_A = interp_A$x, sub_B = interp_B$y)
grid$pred_welfare_A <- as.vector(interp_A$z)
grid$pred_welfare_B <- as.vector(interp_B$z)

# find nash
best_response_A <- grid %>%
  group_by(sub_B) %>%
  slice_max(order_by = pred_welfare_A, n = 1, with_ties = FALSE) %>%
  select(sub_A, sub_B)

best_response_B <- grid %>%
  group_by(sub_A) %>%
  slice_max(order_by = pred_welfare_B, n = 1, with_ties = FALSE) %>%
  select(sub_A, sub_B)

# find the nash equilibrium as the intersection of best responses
nash_equilibrium_sym <- inner_join(best_response_A, best_response_B, 
                               by = c("sub_A", "sub_B")) %>%
  as.data.frame()

# pareto frontier
pareto_front_sym <- psel(grid, high(pred_welfare_A) * high(pred_welfare_B)) %>%
  na.omit() %>%
  mutate(joint_welfare = pred_welfare_A + pred_welfare_B)

pareto_sym <- pareto_front_sym %>% arrange(-joint_welfare) %>%
  slice(1) %>%
  select(sub_A, sub_B)

# print nash and pareto subsidies
print(nash_equilibrium_sym)
print(pareto_sym)
plot(nash_equilibrium_sym)

###

# asymmetric risk ----------------------------------------------

data <- prsub_as %>%
  group_by(sub_A, sub_B) %>%
  summarise(utility_A = mean(utility_A),
            utility_B = mean(utility_B),
            cvar_A = mean(cvar_A),
            cvar_B = mean(cvar_B)) %>%
  mutate(welfare_A = cvar_A,
         welfare_B = cvar_B)

# fit linear interpolation models

## define grid
grid_x <- seq(min(data$sub_A), max(data$sub_A), length.out = 86)  # sub_A
grid_y <- seq(min(data$sub_B), max(data$sub_B), length.out = 86)  # sub_B

## perform interpolation
interp_A <- interp(
  x = data$sub_A,       # x values (sub_A)
  y = data$sub_B,       # y values (sub_B)
  z = data$welfare_A,   # z values (welfare_A)
  xo = grid_x,          # interpolation points
  yo = grid_y,
  linear = TRUE         # force linear interpolation
)
interp_B <- interp(
  x = data$sub_A,       # x values (sub_A)
  y = data$sub_B,       # y values (sub_B)
  z = data$welfare_B,   # z values (welfare_B)
  xo = grid_x,          # interpolation points
  yo = grid_y,
  linear = TRUE         # force linear interpolation
)

# convert the interpolated results to a data frame
grid <- expand.grid(sub_A = interp_A$x, sub_B = interp_B$y)
grid$pred_welfare_A <- as.vector(interp_A$z)
grid$pred_welfare_B <- as.vector(interp_B$z)

# find nash
best_response_A <- grid %>%
  group_by(sub_B) %>%
  slice_max(order_by = pred_welfare_A, n = 1, with_ties = FALSE) %>%
  select(sub_A, sub_B)

best_response_B <- grid %>%
  group_by(sub_A) %>%
  slice_max(order_by = pred_welfare_B, n = 1, with_ties = FALSE) %>%
  select(sub_A, sub_B)

# find the nash equilibrium as the intersection of best responses
nash_equilibrium_as <- inner_join(best_response_A, best_response_B, 
                               by = c("sub_A", "sub_B")) %>%
  as.data.frame()

# pareto frontier
pareto_front_as <- psel(grid, high(pred_welfare_A) * high(pred_welfare_B)) %>%
  na.omit() %>%
  mutate(joint_welfare = pred_welfare_A + pred_welfare_B)

pareto_as <- pareto_front_as %>% arrange(-joint_welfare) %>%
  slice(1) %>%
  select(sub_A, sub_B)

# print nash and pareto subsidies
print(nash_equilibrium_as)
print(pareto_as)
plot(nash_equilibrium_as)

# chart -------------------------------------------------------------------

# combine symmetric and asymmetric data for visualisation
pareto_sym <- pareto_sym %>% mutate(type = "Pareto symmetric")
pareto_as <- pareto_as %>% mutate(type = "Pareto asymmetric")
nash_equilibrium_sym <- nash_equilibrium_sym %>% mutate(type = "Nash symmetric")
nash_equilibrium_as <- nash_equilibrium_as %>% mutate(type = "Nash asymmetric")

# combine all points
points_to_plot <- bind_rows(
  pareto_sym,
  pareto_as,
  nash_equilibrium_sym,
  nash_equilibrium_as
)

# assign a new category for coloring based on risk type
points_to_plot <- points_to_plot %>%
  mutate(risk_type = case_when(
    str_detect(type, " symmetric") ~ "Country risk is symmetric",
    str_detect(type, "asymmetric") ~ "Country A risk higher than B"
  ))

# Separate Pareto points and Nash points for ordering
pareto_points <- points_to_plot %>% filter(str_detect(type, "Pareto"))
nash_points <- points_to_plot %>% filter(str_detect(type, "Nash"))

# create the plot
subsidy_nash <- ggplot() +
  # 45 degree line
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "lightgray", size = 0.7) +
  # add a "45 degree" marker
  annotate("text", x = 0.03, y = 0.06, label = "45-degree line", color = "gray30", size = 3, angle = 45) +
  geom_point(data = nash_points, aes(x = sub_A, y = sub_B, color = risk_type), 
             size = 3, shape = 16) +  # nash points (circles)
  geom_point(data = pareto_points, aes(x = sub_A, y = sub_B, color = risk_type), 
             size = 5, shape = 17) +  # efficient points (triangles)
  # add a single label "Welfare maximising" pointing to both efficient points
  annotate("text", x = 0.33, y = 0.05, 
           label = "Highest welfare", color = "black", size = 4, hjust = 1.2) +
  # lines pointing to triangles
  geom_segment(aes(x = 0.1, y = 0.1,
                   xend = 0.19, yend = 0.06), 
               color = "gray20", size = 0.5) + 
  geom_segment(aes(x = 0.2, y = 0.1,
                   xend = 0.25, yend = 0.06), 
               color = "gray20", size = 0.5) +
  scale_color_manual(values = c("Country risk is symmetric" = "#1b9377", 
                                "Country A risk higher than B" = "#d95f02")) + 
  labs(
    x = "Subsidy level for Country A",
    y = "Subsidy level for Country B",
    color = "Nash equilibrium when:"
  ) +
  guides(
    color = guide_legend(
      override.aes = list(shape = 16) # circles in legend
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.04, 0.96),  # place legend inside plot area
    legend.justification = c(0, 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray80", size = 0.3, linetype = "solid"),
    panel.grid.minor.y = element_line(color = "gray95", size = 0.2, linetype = "dotted"),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10)
  )

subsidy_nash
ggsave("charts/subsidy_nash.pdf", width = 5, height = 4, units = "in")

