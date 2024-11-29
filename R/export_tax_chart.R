
# preamble ----------------------------------------------------------------

# load libraries
library(fields)
library(ggplot2)
library(rPref)
library(tidyverse)
library(gridExtra)

# load data
extax_lo <- read_csv("output/export_tax_loop_low_risk.csv")
extax_as <- read_csv("output/export_tax_loop_asymmetric_risk.csv")

# low (symmetric) risk ----------------------------------------------------

# import and clean data
data_raw <- extax_lo %>%
  group_by(extax_AB, extax_BA) %>%
  rename("tax_BA" = extax_BA,
         "tax_AB" = extax_AB) %>%
  summarise(utility_A = mean(utility_A),
            utility_B = mean(utility_B),
            cvar_A = mean(cvar_A),
            cvar_B = mean(cvar_B))

data_swapped <- data_raw %>%
  rename("tax_BA" = tax_AB,
         "tax_AB" = tax_BA,
         "utility_A" = utility_B,
         "utility_B" = utility_A,
         "cvar_A" = cvar_B,
         "cvar_B" = cvar_A)

data <- bind_rows(data_raw, data_swapped) %>%
  group_by(tax_AB, tax_BA) %>%
  summarise(utility_A = mean(utility_A),
            utility_B = mean(utility_B),
            cvar_A = mean(cvar_A),
            cvar_B = mean(cvar_B))

# define the grid for tax rates
grid <- expand.grid(
  tax_BA = seq(0, 1, length.out = 101),
  tax_AB = seq(0, 1, length.out = 101)
)

# initialise loop
count <- 1
results_sym <- list()

for (lambda in seq(0, 0.9, length.out = 19)){
  
  # define welfare based on lambda weight
  data <- data %>%
    mutate(welfare_A = (1 - lambda) * utility_A + lambda * cvar_A,
           welfare_B = (1 - lambda) * utility_B + lambda * cvar_B)
  
  # fit thin-plate spline model
  spline_A <- Tps(
    x = data[, c("tax_BA", "tax_AB")],  # input variables
    Y = data$welfare_A  # output variable (welfare)
  )
  
  spline_B <- Tps(
    x = data[, c("tax_BA", "tax_AB")],  # input variables
    Y = data$welfare_B  # output variable (welfare)
  )
  
  interpolated <- grid %>%
    mutate(pred_welfare_A = predict(spline_A, grid),
           pred_welfare_B = predict(spline_B, grid))
  
  # find nash
  best_response_A <- interpolated %>%
    group_by(tax_BA) %>%
    filter(pred_welfare_A == max(pred_welfare_A)) %>%
    select(tax_BA, tax_AB)
  
  best_response_B <- interpolated %>%
    group_by(tax_AB) %>%
    filter(pred_welfare_B == max(pred_welfare_B)) %>%
    select(tax_AB, tax_BA)
  
  # find the nash equilibrium as the intersection of best responses
  nash_equilibrium <- inner_join(best_response_A, best_response_B, 
                                 by = c("tax_AB", "tax_BA")) 
  
  nash_average <- nash_equilibrium %>%
    ungroup() %>%
    summarise(across(everything(), mean)) %>%
    mutate(type = "nash", lambda = lambda)
  
  # find nash equilibrium welfare
  nash_welfare <- interpolated %>%
    semi_join(nash_equilibrium, by = c("tax_BA", "tax_AB")) %>%
    mutate(joint_welfare = pred_welfare_A + pred_welfare_B) %>%
    arrange(-joint_welfare)
  
  # find efficient tax combination
  efficient <- psel(
    interpolated, high(pred_welfare_A) * high(pred_welfare_B)
    ) %>%
    mutate(joint_welfare = pred_welfare_A + pred_welfare_B) %>% 
    filter(pred_welfare_A >= as.numeric(max(nash_welfare$pred_welfare_A)),
           pred_welfare_B >= as.numeric(max(nash_welfare$pred_welfare_B))) %>%
    arrange(-joint_welfare) %>%
    slice(1) %>%
    select(tax_BA, tax_AB) %>%
    mutate(type = "efficient", lambda = lambda)
  
  results_sym[[count]] <- bind_rows(nash_average, efficient)
  count <- count + 1
  
}

sym_data <- bind_rows(results_sym)

sym_chart <- ggplot(sym_data, aes(x = lambda, y = tax_BA, color = type)) +
  geom_line(size = 1.2) +  # Line plot
  geom_point(aes(
    fill = case_when(
      type == "nash" & tax_BA == 1 ~ "empty", 
      type == "nash" & tax_BA < 1 ~ "filled_nash",
      TRUE ~ "filled_efficient"
    )),
    shape = case_when(
      sym_data$type == "nash" ~ 21,  # Circle for Nash
      sym_data$type == "efficient" ~ 24  # Triangle for Efficient
    ),
    size = 3, stroke = 1  # Adjust size and stroke
  ) +  # Add points with conditional fill
  scale_color_manual(
    values = c("efficient" = "#5e3c99", "nash" = "#e66101"),
    labels = c("efficient" = "Efficient", "nash" = "Average Nash")  # Updated legend labels
  ) +
  scale_fill_manual(values = c(
    "filled_efficient" = "#5e3c99",
    "filled_nash" = "#e66101",
    "empty" = "white"
  ), guide = "none") +  # Include fill in the legend
  guides(
    color = guide_legend(override.aes = list(
      shape = c(24, 21),  # Match shapes for Nash and Efficient
      fill = c("#5e3c99", "#e66101"),  # Match the fill colors for Nash and Efficient
      size = 3, stroke = 1  # Match point size and stroke
    ))
  ) +
  labs(
    x = expression(lambda),
    y = "Export tax rate",
    color = element_blank(),
    title = "Country risk symmetric"
  ) +
  annotate(
    "text", x = 0.75, y = 0.9, label = "Avg. Nash tax rate", 
    color = "#e66101", size = 4
  ) +
  annotate(
    "text", x = 0.75, y = 0.1, label = "Efficient tax rate", 
    color = "#5e3c99", size = 4
  ) +
  theme_minimal() + 
  theme(
    axis.title = element_text(size = 12),
    legend.position = "none",  
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray80", size = 0.3, linetype = "solid"),
    panel.grid.minor.y = element_line(color = "gray95", size = 0.2, linetype = "dotted"),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10)
  )

sym_chart

# asymmetric risk ----------------------------------------------------

# import and clean data
data <- extax_as %>%
  group_by(extax_AB, extax_BA) %>%
  rename("tax_BA" = extax_BA,
         "tax_AB" = extax_AB) %>%
  summarise(utility_A = mean(utility_A),
            utility_B = mean(utility_B),
            cvar_A = mean(cvar_A),
            cvar_B = mean(cvar_B)) %>%
  filter(tax_AB <= 1, tax_BA <= 1)

# define the grid for tax rates
grid <- expand.grid(
  tax_BA = seq(0, 1, length.out = 101),
  tax_AB = seq(0, 1, length.out = 101)
)

# initialise loop
count <- 1
results_asy <- list()

for (lambda in seq(0, 0.9, length.out = 19)){
  
  # define welfare based on lambda weight
  data <- data %>%
    mutate(welfare_A = (1 - lambda) * utility_A + lambda * cvar_A,
           welfare_B = (1 - lambda) * utility_B + lambda * cvar_B)
  
  # fit thin-plate spline model
  spline_A <- Tps(
    x = data[, c("tax_BA", "tax_AB")],  # input variables
    Y = data$welfare_A  # output variable (welfare)
  )
  
  spline_B <- Tps(
    x = data[, c("tax_BA", "tax_AB")],  # input variables
    Y = data$welfare_B  # output variable (welfare)
  )
  
  interpolated <- grid %>%
    mutate(pred_welfare_A = predict(spline_A, grid),
           pred_welfare_B = predict(spline_B, grid))
  
  # find nash
  best_response_A <- interpolated %>%
    group_by(tax_BA) %>%
    filter(pred_welfare_A == max(pred_welfare_A)) %>%
    select(tax_BA, tax_AB)
  
  best_response_B <- interpolated %>%
    group_by(tax_AB) %>%
    filter(pred_welfare_B == max(pred_welfare_B)) %>%
    select(tax_AB, tax_BA)
  
  # find the nash equilibrium as the intersection of best responses
  nash_equilibrium <- inner_join(best_response_A, best_response_B, 
                                 by = c("tax_AB", "tax_BA")) 
  
  nash_average <- nash_equilibrium %>%
    ungroup() %>%
    summarise(across(everything(), mean)) %>%
    mutate(type = "nash", lambda = lambda)
  
  # find nash equilibrium welfare
  nash_welfare <- interpolated %>%
    semi_join(nash_equilibrium, by = c("tax_BA", "tax_AB")) %>%
    mutate(joint_welfare = pred_welfare_A + pred_welfare_B) %>%
    arrange(-joint_welfare)
  
  # find efficient tax combination
  efficient <- psel(
    interpolated, high(pred_welfare_A) * high(pred_welfare_B)
  ) %>%
    mutate(joint_welfare = pred_welfare_A + pred_welfare_B) %>% 
    filter(pred_welfare_A >= as.numeric(max(nash_welfare$pred_welfare_A)),
           pred_welfare_B >= as.numeric(max(nash_welfare$pred_welfare_B))) %>%
    arrange(-joint_welfare) %>%
    slice(1) %>%
    select(tax_BA, tax_AB) %>%
    mutate(type = "efficient", lambda = lambda)
  
  results_asy[[count]] <- bind_rows(nash_average, efficient)
  count <- count + 1
  
}

asy_chart <- ggplot(asy_data, aes(x = lambda, y = value, color = group)) +
  geom_line(size = 1.2) +  # Line plot
  geom_point(aes(
    fill = case_when(
      group == "Avg. Nash, Country A" & value == 1 ~ "empty",
      group == "Avg. Nash, Country A" & value < 1 ~ "filled_nash_A",
      group == "Avg. Nash, Country B" ~ "filled_nash_B",
      group == "Efficient, Country A" ~ "filled_efficient_A",
      TRUE ~ "filled_efficient_B"
    )),
    shape = case_when(
      asy_data$group %in% c("Avg. Nash, Country A", "Avg. Nash, Country B") ~ 21,  # Circles for Nash
      asy_data$group %in% c("Efficient, Country A", "Efficient, Country B") ~ 24   # Triangles for Efficient
    ),
    size = 3, stroke = 1  # Adjust size and stroke
  ) +  # Add points with conditional fill
  scale_color_manual(
    values = c(
      "Efficient, Country A" = "#5e3c99", 
      "Avg. Nash, Country A" = "#e66101",
      "Efficient, Country B" = "#b2abd2",
      "Avg. Nash, Country B" = "#fdb863"
    )
  ) +
  scale_fill_manual(values = c(
    "filled_efficient_A" = "#5e3c99",
    "filled_efficient_B" = "#b2abd2",
    "filled_nash_A" = "#e66101",
    "filled_nash_B" = "#fdb863",
    "empty" = "white"), 
    guide = "none") +  # Include fill in the legend
  guides(
    color = guide_legend(override.aes = list(
      shape = c(21, 21, 24, 24),  # Circles for Nash, Triangles for Efficient
      fill = c("#e66101", "#fdb863", "#5e3c99", "#b2abd2"),  # Correct fill colors
      size = 3, 
      stroke = 1  # Match point size and stroke
    ))
  ) +
  labs(
    x = expression(lambda),
    y = "Export tax rate",
    color = element_blank(),
    title = "Country A risk higher than B"
  ) +
  annotate(
    "text", x = 0.70, y = 0.97, label = "Avg. Nash tax, A", 
    color = "#e66101", size = 4
  ) +
  annotate(
    "text", x = 0.35, y = 0.85, label = "Avg. Nash tax, B", 
    color = "#fdb863", size = 4
  ) +
  annotate(
    "text", x = 0.75, y = 0.08, label = "Efficient tax, A", 
    color = "#5e3c99", size = 4
  ) +
  annotate(
    "text", x = 0.75, y = 0.3, label = "Efficient tax, B", 
    color = "#b2abd2", size = 4
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray80", size = 0.3, linetype = "solid"),
    panel.grid.minor.y = element_line(color = "gray95", size = 0.2, linetype = "dotted"),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10)
  )

asy_chart

# combine and export ------------------------------------------------------

# Function to extract legend
get_legend <- function(plot) {
  ggplotGrob(plot)$grobs[[which(sapply(ggplotGrob(plot)$grobs, function(x) x$name) == "guide-box")]]
}

# Extract legend from asy_chart
legend <- get_legend(asy_chart)

# Combine sym_chart and asy_chart without legends
combined_plot <- gridExtra::grid.arrange(
  gridExtra::arrangeGrob(sym_chart + theme(legend.position = "none"),
                         asy_chart + theme(legend.position = "none"),
                         ncol = 2),
  legend,
  ncol = 1,
  heights = c(9, 1)  # Adjust heights for plot and legend
)

# Define the file name
output_file <- "charts/export_tax_combined.pdf"

# Export the combined plot to a PDF
ggsave(
  filename = output_file,
  plot = combined_plot,  # Your combined plot object
  device = "pdf",
  width = 7,  # Width in inches (adjust to fit single-column)
  height = 4,  # Height in inches (adjust as needed for aspect ratio)
  units = "in"  # Use inches for LaTeX compatibility
)

# Confirm export
message("Plot saved as ", output_file)
