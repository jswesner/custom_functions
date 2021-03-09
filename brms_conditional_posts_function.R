library(brms)
library(tidyverse)


# Call custom functions
# fit = name of your model,
# effects = name of effects to extract (e.g., "Species" or "Petal.Width" or "Species:Petal.Width" or "Petal.Width:Species")]

# iris

# test <- brm(Sepal.Length ~ Petal.Length*Species,
#             data = iris)

conditional_posts_fitted <- function(fit, effects, conditions = NULL){
list_of_data <- conditional_effects(fit, effects, conditions)[[1]]
col_i <- which(colnames(list_of_data) == names(fit$data[1])) - 1
new_names <- list_of_data[(1:col_i)]

as_tibble(t(fitted(fit, newdata = list_of_data, re_formula = NA, summary = FALSE))) %>%
  cbind(new_names) %>%
  pivot_longer(cols = contains("V"), names_to = "iter") %>%
  mutate(iter = parse_number(iter))
} # this function should work fine now


conditional_posts_predict <- function(fit, effects, conditions = NULL){
  list_of_data <- conditional_effects(fit, effects, conditions)[[1]]
  col_i <- which(colnames(list_of_data) == names(fit$data[1])) - 1
  new_names <- list_of_data[(1:col_i)]

  as_tibble(t(predict(fit, newdata = list_of_data, summary = FALSE))) %>%
    cbind(new_names) %>%
    pivot_longer(cols = contains("V"), names_to = "iter") %>%
    mutate(iter = parse_number(iter))
} # this function needs some work

