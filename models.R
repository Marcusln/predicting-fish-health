library(caret)

library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)

# remove $lice.above.limit, little variation, info already stored in $adult.female.lice
salmon = salmon[,-13]

salmon.backup = salmon

# remove rows with missing sea.temp
salmon.no.na = salmon[!is.na(salmon$sea.temp),]

# remove $location.id
salmon.no.na = salmon.no.na[,-3]

# salmon.small = sample_frac(salmon, size = 0.1)

# salmon = salmon.small

salmon.training = createDataPartition(y = salmon.no.na$adult.female.lice, p = 0.75, list = F)
salmon.train = salmon.no.na[salmon.training,]
salmon.test = salmon.no.na[-salmon.training,]

salmon.control = trainControl(method = 'cv',
                              number = 5,
                              allowParallel = T,
                              verboseIter = T
                            )

set.seed(2001)

salmon.lm.fit2 # basic lm with $location.id, 10-fold CV

salmon.rf.fit = train(adult.female.lice ~.,
                      data = salmon.train,
                      method = 'parRF',
                      metric = 'RMSE', # https://machinelearningmastery.com/machine-learning-evaluation-metrics-in-r/
                      trControl = salmon.control,
                      ntree = 10,
                      mtry = 32
)

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
plot(salmon.train$adult.female.lice, predict(salmon.lm.fit))


threshold = 0.5

# LM
salmon.lm.pred = predict(salmon.lm.fit, newdata = salmon.test, type = 'raw')
plot(salmon.lm.pred, salmon.test$adult.female.lice)
abline(a= 0, b = 1)

salmon.resid = resid(salmon.lm.fit)
plot(salmon.test$adult.female.lice, salmon.resid)


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

plot(varImp(salmon.lm.fit))

exp(coef(salmon.rf.fit$finalModel))
summary(salmon.rf.fit)
