#!/usr/bin/env Rscript
source('code/read_two_class_dataset.R')

library(cowplot)
library(caret)
library(e1071)

test_N = 50
A_min <- min(predictors$PredictorA)
A_max <- max(predictors$PredictorA)
B_min <- min(predictors$PredictorA)
B_max <- max(predictors$PredictorA)
training_dataset <- tibble(A = predictors$PredictorA, B = predictors$PredictorB, class = as.vector(classes))
random_dataset <- tibble(A = runif(length(classes), min = A_min, max = A_max), B = runif(length(classes), min = B_min, max = B_max))

test_set_A <- tibble(A = rnorm(test_N, mean = 0.25, sd = 0.05),
                     B = rnorm(test_N, mean = 0.25, sd = 0.05))
test_set_B <- tibble(A = rnorm(test_N, mean = 0.5, sd = 0.05),
                     B = rnorm(test_N, mean = 0.25, sd = 0.05))
test_set_C <- tibble(A = rnorm(test_N, mean = 1.25, sd = 0.05),
                     B = rnorm(test_N, mean = 0.25, sd = 0.05))

# simulate dataset

simulate_dataset <- bind_rows(list(training = training_dataset, random = random_dataset), .id = 'group') %>% select(-class)
rfSimClass <- train(x = simulate_dataset %>% select(A, B), y = simulate_dataset %>% select(group) %>% unlist(),
                    method = 'rf',
                    tuneLenth = 5,
                    ntree = 1000,
                    control = trainControl(method = 'LGOCV'))

create_distribution_similarity_plot <- function(testset){
  testGroupProbs <- predict(rfSimClass, testset, type = 'prob')
  p1 <- ggplot(bind_rows(list(training = simulate_dataset %>% filter(group == 'training'),
                              random = simulate_dataset %>% filter(group == 'random'),
                              test = testset),
                         .id = 'group'), aes(A, B, color = group)) +
    geom_point(alpha = 0.5) + 
    theme_classic() + 
    xlim(0, 1.5) + 
    theme(legend.position = c(0.8, 0.8))
  
  p2 <- ggplot(testGroupProbs, aes(x = training, y=..count../sum(..count..))) + 
    geom_histogram(binwidth = 0.1) +
    xlim(0, 1) + 
    xlab('training dataset similarity') + 
    ylab('Ratio') +
    theme_classic()
  
  return(plot_grid(p1, p2))
}

p1 <- create_distribution_similarity_plot(test_set_A)
p2 <- create_distribution_similarity_plot(test_set_B)
p3 <- create_distribution_similarity_plot(test_set_C)
plot_grid(p1, p2, p3, ncol = 1)

ggsave('figure/figure_20_9.pdf', width = 6, height = 8)
ggsave('figure/figure_20_9.png', width = 6, height = 8, dpi = 300)