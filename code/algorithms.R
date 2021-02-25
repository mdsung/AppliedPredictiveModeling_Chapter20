library(AppliedPredictiveModeling)
library(caret)
data(solubility)
set.seed(100)

indx <- caret::createFolds(solTrainY, returnTrain = TRUE)
ctrl <- caret::trainControl(method='cv', index = indx)

mtryVals <- floor(seq(10, ncol(solTrainXtrans), length = 10))
seq(10, 228, length = 10)
mtryVals
mtryGrid <- data.frame(.mtry = mtryVals)
rfTune <- train(x = solTrainXtrans, 
                y = solTrainY,
                method = 'rf',
                ntree = 1000,
                importance = TRUE,
                trControl = ctrl)

ImportanceOrder <- order(rfTune$finalModel$importance[,1], decreasing = TRUE)
top20 <- rownames(rfTune$finalModel$importance[ImportanceOrder,])[1:20]

solTrainXimp <- subset(solTrainX, select = top20)
solTestXimp <- subset(solTestX, select = top20)

permutesolTrainXimp <- apply(solTrainXimp, 2, function(x) sample(x))
solSimX <- rbind(solTrainXimp, permutesolTrainXimp)
groupVals <- c('Training', 'Random')
groupY <- factor(rep(groupVals), each = nrow(solTrainX))

rfSolClass <- train(x = solSimX, y = groupY,
                    methow = 'rf',
                    tuneLength = 5,
                    ntree = 1000,
                    control = trainControl(method = 'LGOCV'))
solTestGroupProbs <- predict(rfSolCalss, solTestXimp, type = 'prob')