
# preamble ----------------------------------------------------------------

# load libraries
library(fields)
library(ggplot2)
library(rPref)
library(tidyverse)

# load data
tariff_hi <- read_csv("output/tariff_loop_high_risk.csv")
tariff_as <- read_csv("output/tariff_loop_asymmetric_risk.csv")
tariff_lo <- read_csv("output/tariff_loop_low_risk.csv")
extax_hi <- read_csv("output/export_tax_loop_high_risk.csv")
extax_as <- read_csv("output/export_tax_loop_asymmetric_risk.csv")
extax_lo <- read_csv("output/export_tax_loop_low_risk.csv")
prsub_hi <- read_csv("output/prod_subsidy_loop_high_risk.csv")
prsub_as <- read_csv("output/prod_subsidy_loop_asymmetric_risk.csv")
prsub_lo <- read_csv("output/prod_subsidy_loop_low_risk.csv")

# taxes, symmetric risk ---------------------------------------------------

data_raw <- extax_hi %>%
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
            cvar_B = mean(cvar_B)) %>%
  mutate(welfare_A = cvar_A,
         welfare_B = cvar_B)

# fit thin-plate spline model
spline_A <- Tps(
  x = data[, c("tax_BA", "tax_AB")],  # input variables
  Y = data$welfare_A  # output variable (welfare)
)

spline_B <- Tps(
  x = data[, c("tax_BA", "tax_AB")],  # input variables
  Y = data$welfare_B  # output variable (welfare)
)

# define the grid for tax rates
grid <- expand.grid(
  tax_BA = seq(0, 0.5, length.out = 101),
  tax_AB = seq(0, 0.5, length.out = 101)
)

interpolated <- grid %>%
  mutate(pred_welfare_A = predict(spline_A, grid),
         pred_welfare_B = predict(spline_B, grid))

# find nash
best_response_A <- interpolated %>%
  group_by(tax_AB) %>%
  filter(pred_welfare_A == max(pred_welfare_A)) %>%
  select(tax_BA, tax_AB)

best_response_B <- interpolated %>%
  group_by(tax_BA) %>%
  filter(pred_welfare_B == max(pred_welfare_B)) %>%
  select(tax_AB, tax_BA)

# find the nash equilibrium as the intersection of best responses
nash_equilibrium <- inner_join(best_response_A, best_response_B, 
                               by = c("tax_AB", "tax_BA")) %>%
  as.data.frame()

# pareto frontier
pareto_front <- psel(interpolated, high(pred_welfare_A) * high(pred_welfare_B)) %>%
  mutate(joint_welfare = pred_welfare_A + pred_welfare_B)

pareto <- pareto_front %>% arrange(-joint_welfare) %>%
  slice(1) %>%
  select(tax_BA, tax_AB)

# print nash and pareto tariffs
print(nash_equilibrium)
print(pareto)

# subsidies, symmetric risk -----------------------------------------------

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

# fit thin-plate spline model
spline_A <- Tps(
  x = data[, c("sub_A", "sub_B")],  # input variables
  Y = data$welfare_A  # output variable (welfare)
)

spline_B <- Tps(
  x = data[, c("sub_A", "sub_B")],  # input variables
  Y = data$welfare_B  # output variable (welfare)
)

# define the grid for tariffs
grid <- expand.grid(
  sub_A = seq(0, 1, length.out = 101),
  sub_B = seq(0, 1, length.out = 101)
)

interpolated <- grid %>%
  mutate(pred_welfare_A = predict(spline_A, grid),
         pred_welfare_B = predict(spline_B, grid))

# find nash
best_response_A <- interpolated %>%
  group_by(sub_B) %>%
  filter(pred_welfare_A == max(pred_welfare_A)) %>%
  select(sub_A, sub_B)

best_response_B <- interpolated %>%
  group_by(sub_A) %>%
  filter(pred_welfare_B == max(pred_welfare_B)) %>%
  select(sub_A, sub_B)

# find the nash equilibrium as the intersection of best responses
nash_equilibrium <- inner_join(best_response_A, best_response_B, 
                               by = c("sub_A", "sub_B")) %>%
  as.data.frame()

# pareto frontier
pareto_front <- psel(interpolated, high(pred_welfare_A) * high(pred_welfare_B)) %>%
  mutate(joint_welfare = pred_welfare_A + pred_welfare_B)

pareto <- pareto_front %>% arrange(-joint_welfare) %>%
  slice(1) %>%
  select(sub_A, sub_B)

# print nash and pareto subsidies
print(nash_equilibrium)
print(pareto)

# taxes, asymmetric risk --------------------------------------------------

data <- extax_as %>%
  group_by(extax_AB, extax_BA) %>%
  rename("tax_BA" = extax_BA,
         "tax_AB" = extax_AB) %>%
  summarise(utility_A = mean(utility_A),
            utility_B = mean(utility_B),
            cvar_A = mean(cvar_A),
            cvar_B = mean(cvar_B)) %>%
  mutate(welfare_A = utility_A,
         welfare_B = utility_B)

# fit thin-plate spline model
spline_A <- Tps(
  x = data[, c("tax_BA", "tax_AB")],  # input variables
  Y = data$welfare_A  # output variable (welfare)
)

spline_B <- Tps(
  x = data[, c("tax_BA", "tax_AB")],  # input variables
  Y = data$welfare_B  # output variable (welfare)
)

# define the grid for tax rates
grid <- expand.grid(
  tax_BA = seq(0, 0.5, length.out = 51),
  tax_AB = seq(0, 0.5, length.out = 51)
)

interpolated <- grid %>%
  mutate(pred_welfare_A = predict(spline_A, grid),
         pred_welfare_B = predict(spline_B, grid))

# find nash
best_response_A <- interpolated %>%
  group_by(tax_AB) %>%
  filter(pred_welfare_A == max(pred_welfare_A)) %>%
  select(tax_BA, tax_AB)

best_response_B <- interpolated %>%
  group_by(tax_BA) %>%
  filter(pred_welfare_B == max(pred_welfare_B)) %>%
  select(tax_AB, tax_BA)

# find the nash equilibrium as the intersection of best responses
nash_equilibrium <- inner_join(best_response_A, best_response_B, 
                               by = c("tax_AB", "tax_BA")) %>%
  as.data.frame()

# pareto frontier
pareto_front <- psel(interpolated, high(pred_welfare_A) * high(pred_welfare_B)) %>%
  mutate(joint_welfare = pred_welfare_A + pred_welfare_B)

pareto <- pareto_front %>% arrange(-joint_welfare) %>%
  slice(1) %>%
  select(tax_BA, tax_AB)

# print nash and pareto tariffs
print(nash_equilibrium)
print(pareto)

# subsidies, asymmetric risk ----------------------------------------------

data <- prsub_as %>%
  group_by(sub_A, sub_B) %>%
  summarise(utility_A = mean(utility_A),
            utility_B = mean(utility_B),
            cvar_A = mean(cvar_A),
            cvar_B = mean(cvar_B)) %>%
  mutate(welfare_A = cvar_A,
         welfare_B = cvar_B)

# fit thin-plate spline model
spline_A <- Tps(
  x = data[, c("sub_A", "sub_B")],  # input variables
  Y = data$welfare_A  # output variable (welfare)
)

spline_B <- Tps(
  x = data[, c("sub_A", "sub_B")],  # input variables
  Y = data$welfare_B  # output variable (welfare)
)

# define the grid for tariffs
grid <- expand.grid(
  sub_A = seq(0, 1, length.out = 101),
  sub_B = seq(0, 1, length.out = 101)
)

interpolated <- grid %>%
  mutate(pred_welfare_A = predict(spline_A, grid),
         pred_welfare_B = predict(spline_B, grid))

# find nash
best_response_A <- interpolated %>%
  group_by(sub_B) %>%
  filter(pred_welfare_A == max(pred_welfare_A)) %>%
  select(sub_A, sub_B)

best_response_B <- interpolated %>%
  group_by(sub_A) %>%
  filter(pred_welfare_B == max(pred_welfare_B)) %>%
  select(sub_A, sub_B)

# find the nash equilibrium as the intersection of best responses
nash_equilibrium <- inner_join(best_response_A, best_response_B, 
                               by = c("sub_A", "sub_B")) %>%
  as.data.frame()

# pareto frontier
pareto_front <- psel(interpolated, high(pred_welfare_A) * high(pred_welfare_B)) %>%
  mutate(joint_welfare = pred_welfare_A + pred_welfare_B)

pareto <- pareto_front %>% arrange(-joint_welfare) %>%
  slice(1) %>%
  select(sub_A, sub_B)

# print nash and pareto subsidies
print(nash_equilibrium)
print(pareto)
