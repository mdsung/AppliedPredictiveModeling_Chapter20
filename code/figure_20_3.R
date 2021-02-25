#!/usr/bin/env Rscript
library(tidyverse)
library(rpart)
set.seed(100)
sd_list = c(0, 0.25, 0.5, 0.75)
N = 50
x = runif(N, min = 2, max = 10)
generate_tibble <- function(sd){
  return(tibble(x = x, y=sin(x) + rnorm(N, 0, sd)))
}

calculate_linear_r_squre <- function(sd){
  linear_model <- lm(y ~ sin(x) + cos(x), data = generate_tibble(sd))
  return(summary.lm(linear_model)$r.squared)
}

calculate_cart_r_squre <- function(sd){
  cart_model <- train(generate_tibble(sd) %>% select(x), 
                      generate_tibble(sd)$y, 
                      model = 'rpart2',
                      maxdepth = 3,
                      tuneLength = 10,
                      trControl = trainControl(method = 'cv'))
  return(mean(cart_model$resample$Rsquared))
}

linear_r_squares <- sd_list %>% 
  purrr::map(~calculate_linear_r_squre(.)) %>% 
  unlist() %>% 
  tibble(r_square = ., sd = sd_list)

cart_r_squares <- sd_list %>%
  purrr::map(~calculate_cart_r_squre(.)) %>% 
  unlist() %>% 
  tibble(r_square = ., sd = sd_list)

r_squares <- bind_rows(list(linear_pred = linear_r_squares, cart_pred = cart_r_squares), .id = 'group')
r_squares$group_f <- factor(r_squares$group, levels = c('linear_pred', 'cart_pred'))

generate_ggplot_data <- function(sd){
  linear_model <- lm(y ~ sin(x) + cos(x), data = generate_tibble(sd))
  cart_model <- train(generate_tibble(sd) %>% select(x), 
                      generate_tibble(sd)$y, 
                      model = 'rpart2',
                      maxdepth = 3,
                      tuneLength = 10,
                      trControl = trainControl(method = 'cv'))
  
  pred_data <- tibble(x = x,  
                      original = generate_y(0.25), 
                      linear_pred = predict(linear_model, data.frame(x)), 
                      cart_pred = predict(cart_model, data.frame(x))) %>%
    pivot_longer(cols = c('linear_pred', 'cart_pred'), names_to = 'group', values_to = 'y')
  
  x_grid <- seq(2, 10, 0.01)
  grid_data <- tibble(x = x_grid,
                      original = sin(x_grid),
                      linear_pred = predict(linear_model, newdata = data.frame(x = x_grid)),
                      cart_pred = predict(cart_model, newdata = data.frame(x = x_grid))) %>%
    pivot_longer(cols = c('linear_pred', 'cart_pred'), names_to = 'group', values_to = 'y')
  return(list(pred_data, grid_data))
}

total_pred_df <- tibble()
total_grid_df <- tibble()
for (sd in sd_list){
  df <- generate_ggplot_data(sd)
  df[[1]]$sd <- sd
  df[[2]]$sd <- sd
  total_pred_df <- bind_rows(total_pred_df, df[[1]])
  total_grid_df <- bind_rows(total_grid_df, df[[2]])
}

total_pred_df$group_f <- factor(total_pred_df$group, levels = c('linear_pred', 'cart_pred'))
total_grid_df$group_f <- factor(total_grid_df$group, levels = c('linear_pred', 'cart_pred'))

ggplot() + 
  geom_point(data = total_pred_df, aes(x = x, y = original), color = 'grey') + 
  geom_point(data = total_pred_df, aes(x = x, y = y), color = 'black') + 
  geom_line(data = total_grid_df, aes(x, y), color = 'red') +
  geom_line(data = total_grid_df, aes(x, original), color = 'grey') + 
  geom_text(aes(label = round(r_square, 3)), x = 8, y = -1, data =r_squares) + 
  theme_classic() +
  facet_grid(sd ~ group_f, switch ='y' )

ggsave('figure/figure_20_3.pdf', width = 6, height = 8)
ggsave('figure/figure_20_3.png', width = 6, height = 8, dpi = 300)

