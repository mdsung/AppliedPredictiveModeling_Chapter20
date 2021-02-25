source('code/read_two_class_dataset.R')
## Figure 20.8

A_min <- min(predictors$PredictorA)
A_max <- max(predictors$PredictorA)
B_min <- min(predictors$PredictorA)
B_max <- max(predictors$PredictorA)

training_dataset <- tibble(A = predictors$PredictorA, B = predictors$PredictorB)
random_dataset <- tibble(A = runif(length(classes), min = A_min, max = A_max), B = runif(length(classes), min = B_min, max = B_max))
total_dataset <- bind_rows(list(training = training_dataset, rnadom = random_dataset), .id = 'group')
total_dataset %>% ggplot(aes(A, B, color = group)) + geom_point() + theme_classic()

