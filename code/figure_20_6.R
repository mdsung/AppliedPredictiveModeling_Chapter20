#!/usr/bin/env Rscript

library(AppliedPredictiveModeling)
library(tidyverse)
library(cowplot)
data(solubility)

## Prepare dataset
solTrainXtrans #X
solTrainY #Y
solTrainDiscY <- ifelse(solTrainY > mean(solTrainY), 1, 0) # disc Y

## create modeling plot
create_plot_regression <- function(model){
  return(tibble(original = solTestY, pred = predict(model, solTestXtrans)) %>%
           ggplot(aes(x = original, y = pred)) + 
           geom_point() +
           theme(axis.title.x = element_blank(),
                 axis.title.y = element_blank()) + 
           theme_classic()
  )
}

create_plot_classification <- function(probs){
  return(tibble(original = solTestY, pred = probs) %>%
           ggplot(aes(x = original, y = pred)) + 
            geom_point() +
            theme(axis.title.x = element_blank(),
                 axis.title.y = element_blank()) + 
            theme_classic()
  )
}

## linear regression model
ctrl <- trainCtonrol(method = 'cv', number = 10)

lm_regression_model <- train(
  x = solTrainXtrans,
  y = solTrainY,
  method = 'lm',
  trControl = ctrl
)

## logistic regression model
lm_classification_model <- train(
  x = solTrainXtrans,
  y = solTrainDiscY,
  method = 'glm',
  family = 'binomial',
  trControl = ctrl
)

classification_probs <- predict(lm_classification_model, solTestXtrans)
plot_lm <- plot_grid(create_plot_regression(lm_regression_model), 
                     create_plot_classification(classification_probs))
plot_lm

## SVM
svm_regression_model <- train(
  x = solTrainXtrans,
  y = solTrainY,
  tuneLength = 5,
  method = 'svmLinear',
  preProc = c('center', 'scale'),
  trControl = ctrl
)


svm_classification_model <- train(
  x = solTrainXtrans,
  y = as.factor(solTrainDiscY),
  tuneLength = 5,
  method = 'svmLinear2',
  preProc = c('center', 'scale'),
  trControl = ctrl,
  probability = TRUE
)

svm_classification_probs <- predict(svm_classification_model, newdata = solTestXtrans, type = "prob")[,2]
plot_svm <- plot_grid(create_plot_regression(svm_regression_model),
                      create_plot_classification(svm_classification_probs))


## RF
rf_regression_model <- train(
  x = solTrainXtrans,
  y = solTrainY,
  tuneLength = 5,
  method = 'rf',
  trControl = ctrl
)

rf_classifciation_model <- train(
  x = solTrainXtrans,
  y = as.factor(solTrainDiscY),
  tuneLength = 5,
  method = 'rf',
  trControl = ctrl
)

rf_classification_probs <- predict(rf_classifciation_model, solTestX, type = 'prob')[,2]
rf_classification_probs
plot_rf <- plot_grid(create_plot_regression(rf_regression_model), 
                     create_plot_classification(rf_classification_probs))

## aggregate all plots
title <- ggdraw() + 
  draw_label(
    "Continuous Vs. Discrete",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )

plot_grid(title, plot_lm, plot_svm, plot_rf, ncol = 1, labels = c('', 'A','B','C'), rel_heights = c(0.1, 1, 1, 1))
ggsave('figure/figure_20_6.pdf', width = 6, height = 8)
ggsave('figure/figure_20_6.png', width = 6, height = 8, dpi = 300)
