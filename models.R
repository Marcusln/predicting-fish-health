library(caret)

salmon.backup = salmon

salmon.no.na = salmon[!is.na(salmon$sea.temp),]
salmon.no.na = salmon.no.na[,-c(12)]

salmon.small = sample_frac(salmon, size = 0.1)
# salmon = salmon.small

salmon.training = createDataPartition(y = salmon.no.na$adult.female.lice, p = 0.75, list = F)
salmon.train = salmon.no.na[salmon.training,]
salmon.test = salmon.no.na[-salmon.training,]

# check distribution of response variable
prop.table(table(salmon.train$lice.above.limit))
prop.table(table(salmon.test$lice.above.limit))

salmon.control = trainControl(method = 'cv', #repeatedcv
                            #repeats = 3,
                            # classProbs = TRUE,
                            allowParallel = T,
                            number = 1,
                            repeats = 1
                            # summaryFunction = twoClassSummary,
                            # verboseIter = T
                            )

salmon.lm.fit = train(adult.female.lice ~.,
                      data = salmon.train,
                      method = 'lm',
                      metric = 'RMSE', # https://machinelearningmastery.com/machine-learning-evaluation-metrics-in-r/
                      trControl = salmon.control, # bank.control,
                      # preProcess = 'knnImpute', # c('center', 'scale'),
                      # na.action = na.omit,
                      tuneLength = 1
)

plot(salmon.rf.fit)

threshold = 0.5

# GLM
salmon.glm.pred = predict(salmon.glm.fit, newdata = salmon.test, type = 'prob', na.action = na.omit)
salmon.glm.prediction = factor(ifelse(salmon.glm.pred$yes >= threshold, "yes", "no"))

confusionmatrix.glm = confusionMatrix(data = salmon.glm.prediction, # a factor of predicted classes
                                      reference = salmon.test$lice.above.limit # a factor of classes to be used as the true results
)

confusionmatrix.log

# RF
salmon.rf.pred = predict(salmon.rf.fit, newdata = salmon.test, type = 'prob', na.action = na.omit)
salmon.rf.prediction = factor(ifelse(salmon.rf.pred$yes >= threshold, "yes", "no"))

confusionmatrix.rf = confusionMatrix(data = salmon.rf.prediction, # a factor of predicted classes
                                      reference = salmon.test$lice.above.limit # a factor of classes to be used as the true results
)

confusionmatrix.rf

plot(varImp(salmon.rf.fit))

exp(coef(salmon.rf.fit$finalModel))
summary(salmon.rf.fit)
