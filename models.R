library(caret)

library(doParallel)
registerDoParallel(makeCluster(4))

salmon.training = createDataPartition(y = salmon.cleaned$adult.female.lice, p = 0.75, list = F)
salmon.train = salmon.cleaned[salmon.training,]
salmon.test = salmon.cleaned[-salmon.training,]

salmon.control = trainControl(method = 'cv',
                              number = 5,
                              allowParallel = T,
                              verboseIter = T
                            )

set.seed(2001)

salmon.lm.fit # basic lm with $location.id, 10-fold CV

salmon.lm.fit = train(adult.female.lice ~.,
                       data = salmon.train,
                       method = 'lm',
                       metric = 'RMSE', # https://machinelearningmastery.com/machine-learning-evaluation-metrics-in-r/
                       trControl = salmon.control
)

salmon.svm.fit = train(adult.female.lice ~.,
                      data = salmon.train,
                      method = 'svmLinear',
                      metric = 'RMSE', # https://machinelearningmastery.com/machine-learning-evaluation-metrics-in-r/
                      trControl = salmon.control
)

salmon.rf.fit = train(adult.female.lice ~.,
                      data = salmon.train,
                      method = 'parRF',
                      metric = 'RMSE', # https://machinelearningmastery.com/machine-learning-evaluation-metrics-in-r/
                      trControl = salmon.control,
                      ntree = 10,
                      importance = T
                      # mtry = 32
) # 5 fold CV

summary(salmon.rf.fit)

plot(salmon.rf.fit)
varImp(salmon.rf.fit)
plot(varImp(salmon.rf.fit))

plot(salmon.train$adult.female.lice, resid(salmon.rf.fit))
abline(0,0)

plot(salmon.lm.fit$finalModel$fitted.values, salmon.lm.fit$finalModel$residuals)
abline(0,0)

d = salmon.train

d$predicted <- predict(salmon.rf.fit)   # Save the predicted values
d$residuals <- residuals(salmon.rf.fit) # Save the residual values

# Quick look at the actual, predicted, and residual values
salmon.no.na %>% select(female.adult.lice, predicted, residuals) %>% head()

#
## GLM
###

salmon.glm.fit = train(abs(adult.female.lice) ~.,
                       data = salmon.train,
                       method = 'glm',
                       metric = 'RMSE', # https://machinelearningmastery.com/machine-learning-evaluation-metrics-in-r/
                       trControl = salmon.control
)

salmon.lm.fit3 = train(adult.female.lice ~.,
                      data = salmon.train,
                      method = 'lm',
                      metric = 'RMSE', # https://machinelearningmastery.com/machine-learning-evaluation-metrics-in-r/
                      trControl = salmon.control
                      # preProcess = 'knnImpute', # c('center', 'scale'),
                      # na.action = na.omit,
                      # tuneLength = 1
                      # ntree = 10,
)

plot(salmon.train$adult.female.lice, resid(salmon.lm.fit3))
abline(0,0)



threshold = 0.5

# LM
salmon.lm.pred = predict(salmon.lm.fit, newdata = salmon.test, type = 'raw')
plot(salmon.lm.pred, salmon.test$adult.female.lice)
abline(a= 0, b = 1)

salmon.resid = resid(salmon.lm.fit)
plot(salmon.test$adult.female.lice, salmon.resid)
