
# load libraries anddata
library(dplyr)
library(fmsb)
library(rPref)

data <- read.csv("output/multivariate_loop_asymmetric_risk.csv")

# discard if combination didn't converge in simulations
filtered_data <- data %>% filter(no_convergence == 0)

filtered_data <- filtered_data %>%
  mutate(welfare_A = cvar_A,
         welfare_B = cvar_B) %>%
  mutate(joint_welfare = welfare_A + welfare_B)

# compute each country's best response
best_response_A <- filtered_data %>%
  group_by(tariff_AB, extax_BA, z_B) %>%
  slice_max(order_by = welfare_A, n = 1, with_ties = FALSE)
best_response_B <- filtered_data %>%
  group_by(tariff_BA, extax_AB, z_A) %>%
  slice_max(order_by = welfare_B, n = 1, with_ties = FALSE)

# find nash equilibria
nash_equilibria <- inner_join(
  best_response_A,
  best_response_B,
  by = c("tariff_AB", "extax_AB", "z_A", "tariff_BA", "extax_BA", "z_B")
) %>%
  select(1:6, 13:14) %>%
  rename("welfare_A" = welfare_A.x,
         "welfare_B" = welfare_B.x) %>%
  mutate(joint_welfare = welfare_A + welfare_B)

# nash_welfare_A <- nash_equilibria %>%
#   ungroup() %>%
#   summarise(max_value = max(welfare_A, na.rm = TRUE)) %>%
#   pull(max_value)
# 
# nash_welfare_B <- nash_equilibria %>%
#   ungroup() %>%
#   summarise(max_value = max(welfare_B, na.rm = TRUE)) %>%
#   pull(max_value)

# compute efficient (best pareto) combination
efficient <- pareto_frontier %>%
  slice_max(joint_welfare, n = 1)

library(knitr)

# Combine efficient and Nash equilibria
comparison_table <- bind_rows(
  efficient %>% mutate(type = "Efficient"),
  nash_equilibria %>% mutate(type = "Nash Equilibrium")
)

# Display table
kable(comparison_table, caption = "Comparison of Efficient and Nash Combinations")

# Combine Efficient and all Nash equilibria
plot_data <- rbind(
  nash_equilibria %>% select(tariff_AB, tariff_BA, extax_AB, extax_BA, z_A, z_B),
  efficient %>% select(tariff_AB, tariff_BA, extax_AB, extax_BA, z_A, z_B)
)

# Rescale variables to a consistent [0, 0.3] range (convert percentages)
plot_data <- plot_data %>%
  mutate(
    tariff_AB = tariff_AB / 0.3 * 100,  # Scale to [0, 0.3]
    tariff_BA = tariff_BA / 0.3 * 100,
    extax_AB = extax_AB / 0.3 * 100,
    extax_BA = extax_BA / 0.3 * 100,
    z_A = z_A / 0.3 * 100,
    z_B = z_B / 0.3 * 100
  )

# Add max and min rows for consistent scaling (using bind_rows)
max_row <- setNames(rep(100, ncol(plot_data)), colnames(plot_data))  # Named vector for max row
min_row <- setNames(rep(0, ncol(plot_data)), colnames(plot_data))    # Named vector for min row

plot_data <- bind_rows(
  max_row,  # Maximum row
  min_row,  # Minimum row
  plot_data # Original data
)

colnames(plot_data) <- c(
  "B's import tax", 
  "A's import tax", 
  "A's export tax", 
  "B's export tax", 
  "A's subsidy", 
  "B's subsidy"
)

# export to PDF
# pdf("charts/radar.pdf", width = 5, height = 4)  # 5x4 inches

# Adjust margins and font size
par(mar = c(1, 1, 1, 1), cex = 1)  # Slightly smaller text size (e.g., 1 for ~10pt)

# radar chart
radar_0 <- radarchart(
  plot_data, axistype = 1,
  seg = 3,
  pcol = c(rep("#e6ab02", nrow(nash_equilibria)), "#7570b3"),  # Blue for efficient, red for Nash
  pfcol = c(rep(rgb(0.9, 0.67, 0.13, 0.3), nrow(nash_equilibria)), rgb(0.46, 0.44, 0.7, 0.3)),  # Transparent fills
  plwd = 2,  # Line width for borders
  cglcol = "gray", cglty = 1,  # Gridline color and style
  cglwd = c(0.8, 0.8, 0.8, 0),  # Hide the innermost gridline
  axislabcol = "black",  # Axis label color
  vlcex = 1,  # Slightly smaller variable label size
  caxislabels = c("0%", "10%", "20%", "30%"),  # Correct percentage labels
  calcex = 1  # Slightly smaller axis label size
)

# Manually place the legend at specific coordinates
legend(
  x = -1.7, y = 1.2,  # Adjust x and y to place the legend outside the chart
  legend = c("Efficient combination", "Nash combinations"),  # Labels
  col = c("#7570b3", "#e6ab02"),  # Colors corresponding to chart
  pch = 15,  # Use filled squares
  pt.cex = 1,  # Slightly smaller point size
  bty = "n",  # No border around the legend
  cex = 1  # Slightly smaller text size for legend
)

# for CVAR

# Combine Efficient and all Nash equilibria
plot_data <- rbind(
  nash_equilibria %>% select(tariff_AB, tariff_BA, extax_AB, extax_BA, z_A, z_B),
  efficient %>% select(tariff_AB, tariff_BA, extax_AB, extax_BA, z_A, z_B)
)

# Rescale variables to a consistent [0, 0.3] range (convert percentages)
plot_data <- plot_data %>%
  mutate(
    tariff_AB = tariff_AB / 0.3 * 100,  # Scale to [0, 0.3]
    tariff_BA = tariff_BA / 0.3 * 100,
    extax_AB = extax_AB / 0.3 * 100,
    extax_BA = extax_BA / 0.3 * 100,
    z_A = z_A / 0.3 * 100,
    z_B = z_B / 0.3 * 100
  )

# Add max and min rows for consistent scaling (using bind_rows)
max_row <- setNames(rep(100, ncol(plot_data)), colnames(plot_data))  # Named vector for max row
min_row <- setNames(rep(0, ncol(plot_data)), colnames(plot_data))    # Named vector for min row

plot_data <- bind_rows(
  max_row,  # Maximum row
  min_row,  # Minimum row
  plot_data # Original data
)

colnames(plot_data) <- c(
  "B's import tax", 
  "A's import tax", 
  "A's export tax", 
  "B's export tax", 
  "A's subsidy", 
  "B's subsidy"
)

# Adjust margins and font size
par(mar = c(1, 1, 1, 1), cex = 1)  # Slightly smaller text size (e.g., 1 for ~10pt)

# radar chart
radarchart(
  plot_data, axistype = 1,
  seg = 3,
  pcol = c(rep("#e6ab02", nrow(nash_equilibria)), "#7570b3"),  # Blue for efficient, red for Nash
  pfcol = c(rep(rgb(0.9, 0.67, 0.13, 0.01), nrow(nash_equilibria)), rgb(0.46, 0.44, 0.7, 0.5)),  # Transparent fills
  plwd = 2,  # Line width for borders
  cglcol = "gray", cglty = 1,  # Gridline color and style
  cglwd = c(0.8, 0.8, 0.8, 0),  # Hide the innermost gridline
  axislabcol = "black",  # Axis label color
  vlcex = 1,  # Slightly smaller variable label size
  caxislabels = c("0%", "10%", "20%", "30%"),  # Correct percentage labels
  calcex = 1  # Slightly smaller axis label size
)
