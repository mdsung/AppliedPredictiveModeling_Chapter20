source('code/read_two_class_dataset.R')

library(cowplot)
library(caret)
library(e1071)

test_N = 50

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

testGroupProbs <- predict(rfSimClass, test_set_C, type = 'prob')
p1 <- ggplot(bind_rows(list(training = simulate_dataset %>% filter(group == 'training'),
                      random = simulate_dataset %>% filter(group == 'random'),
                      test = test_set_C),
                 .id = 'group'), aes(A, B, color = group)) +
  geom_point(alpha = 0.5) + 
  theme_classic() + 
  xlim(0, 1.5) + 
  theme(legend.position = c(0.8, 0.8))
  
p2 <- ggplot(testGroupProbs, aes(x = training, y=..count../sum(..count..))) + 
  geom_histogram(binwidth = 0.1) +
  xlim(0, 1) + 
  theme(axis.title.x = element_text('training dataset similarity')) + 
  theme_classic()

plot_grid(p1, p2)

testGroupProbs <- predict(rfSimClass, test_set_B, type = 'prob')
hist(testGroupProbs$training)

testGroupProbs <- predict(rfSimClass, test_set_C, type = 'prob')
hist(testGroupProbs$training)
