
library(tidyverse)

# import and clean data
data_raw <- read_csv("output/entry_subsidy_loop_high_kappa.csv") %>%
  group_by(e_A, e_B) %>%
  summarise(utility_A = mean(utility_A),
            utility_B = mean(utility_B),
            cvar_A = mean(cvar_A),
            cvar_B = mean(cvar_B))

data_swapped <- data_raw %>%
  rename("e_A" = e_B,
         "e_B" = e_A,
         "utility_A" = utility_B,
         "utility_B" = utility_A,
         "cvar_A" = cvar_B,
         "cvar_B" = cvar_A)

data <- bind_rows(data_raw, data_swapped) %>%
  group_by(e_A, e_B) %>%
  summarise(utility_A = mean(utility_A),
            utility_B = mean(utility_B),
            cvar_A = mean(cvar_A),
            cvar_B = mean(cvar_B))

# data for chart
chart_data <- data %>%
  filter(e_A == e_B) %>%
  mutate(welfare_0 = utility_A,
         welfare_0pt5 = 0.5 * (utility_A + cvar_A),
         welfare_1 = cvar_A) %>%
  pivot_longer(c(welfare_0, welfare_0pt5, welfare_1)) %>%
  mutate(lambda = case_when(name == "welfare_0" ~ "0",
                            name == "welfare_0pt5" ~ "0.5",
                            TRUE ~ "1"),
         welfare = 2 * value,
         subsidy = e_A) %>%
  ungroup() %>%
  select(subsidy, lambda, welfare) %>%
  # index to no subsidy = 100
  mutate(welfare = case_when(lambda == "0" ~ welfare / 19.58043 * 100,
                             lambda == "0.5" ~ welfare / 18.21202 * 100,
                             lambda == "1" ~ welfare / 16.84361 * 100),
         lambda = factor(lambda, levels = c("1", "0.5", "0")))

# chart
ggplot(data = chart_data, mapping = aes(x = subsidy, 
                                        y = welfare,
                                        colour = lambda)) +
  geom_hline(yintercept = 100) +
  geom_line(size = 1.2) +
  geom_point(size = 3, stroke = 1) +
  lims(x = c(0, 0.5), y = c(96, 104)) +
  scale_colour_manual(values = c("#cc3311", "#ee7733", "#009988")) +
  labs(x = "Symmetric entry subsidy rate", 
       y = "Joint welfare (no subsidy = 100)",
       colour = expression(lambda)) +
  theme_minimal() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.85, 0.75),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray80", size = 0.3, linetype = "solid"),
    panel.grid.minor.y = element_line(color = "gray95", size = 0.2, linetype = "dotted"),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold")  # Center-align title
  )

ggsave("charts/entry_subsidy.pdf", width = 5, height = 4, units = "in")
