#Downloaded 31.10.2017
getwd()
setwd("D:/My Documents/Computer Science 3/BigData/")
df = read.csv('drug_consumption.data', header=T, na.strings= c(""), stringsAsFactors = FALSE)

#head(df)
#summary(df)
#naCounts = sapply(df,function(x) sum(is.na(x)))
#naCounts
#uniqueVals = sapply(df, function(x) length(unique(x)))
#uniqueVals
names(df) =c('ID','Age','Gender', 'Education', 'Country', 'Ethnicity', 'Nscore', 'Escore', 'Oscore', 'Ascore', 'Cscore', 'Impulsiveness', 'Sensation','Alcohol', 'Amphet', 'Amyl', 'Benzos', 'Caffeine', 'Cannabis', 'Chocolate', 'Cocaine', 'Crack', 'Ecstasy', 'Heroin', 'Ketamine', 'Legals', 'LSD', 'Meth', 'Mushrooms', 'Nicotine', 'Semer', 'VSA')
df_backup = df
#summary(df$Semer)

#show class label distribution
#barplot(table(df$Cannabis))

semerRows = which(!df$Semer == 'CL0')
#semerRows
df = df[-semerRows,]
df$Semer = NULL

#Dropping ID
df$ID = NULL

#testign
df$Cannabis = as.factor(df$Cannabis)
df$Chocolate = as.factor(df$Chocolate)
df$Caffeine = as.factor(df$Caffeine)
df$Nicotine = as.factor(df$Nicotine)
df$Alcohol = as.factor(df$Alcohol)
df$Amphet = as.factor(df$Amphet)
df$Amyl = as.factor(df$Amyl)
df$Benzos = as.factor(df$Benzos)
df$Cocaine = as.factor(df$Cocaine)
df$Crack = as.factor(df$Crack)
df$Ecstasy = as.factor(df$Ecstasy)
df$Heroin = as.factor(df$Heroin)
df$Ketamine = as.factor(df$Ketamine)
df$Legals = as.factor(df$Legals)
df$LSD = as.factor(df$LSD)
df$Meth = as.factor(df$Meth)
df$Mushrooms = as.factor(df$Mushrooms)
df$Nicotine = as.factor(df$Nicotine)
df$VSA = as.factor(df$VSA)

#making Cannabis a binary factor in both train and test data sets
levels(df$Cannabis) = list("NonU" = c("CL0", "CL1"), "User" = c("CL2","CL3","CL4", "CL5","CL6"))

#install.packages('e1071', dependencies=TRUE)
require(randomForest)

#Creating train and test data sets
set.seed(3)
trainingRowIndex = sample(1:nrow(df), 0.8*nrow(df))
train = df[trainingRowIndex,]
test = df[-trainingRowIndex,]

#RandomForest Model - setting ntree
ntrees = 100
# create dataframe to store accuracies and corresponding number of trees
results = data.frame(NTrees=as.numeric(), Accuracy=as.numeric())

for (i in 1:10){
  fit <- randomForest(
    train[,-18], train[,18],
    xtest=test[,-18], ytest=test[,18],
    ntree=ntrees, proximity=TRUE)
  
  # Test the RF model for this run
  preds = levels(train[,18])[fit$test$predicted]
  
  # compute accuracy
  auc = (sum(preds ==test[,18])/nrow(test))*100
  results = rbind(results, data.frame(NTrees=ntrees,Accuracy=auc))
  cat(" ", ntrees)
  ntrees = ntrees + 100
}# end for loop

library(ggplot2)
plot = ggplot(results, aes(x=NTrees, y=Accuracy))
plot = plot+ geom_line() + geom_point()
plot = plot + xlim(100,1000)


#Predicting
fit = randomForest(as.factor(Cannabis) ~., data=train, importance = TRUE, ntree=300)

probabilities = predict(fit, test[,-18])
#probs[1:10]
test_accuracy = mean(probabilities==test$Cannabis)
test_accuracy

#install.packages("ROCR")
require(ROCR)

predictions = predict(fit, newdata = test[,-18], type="prob")[,2] 
accuracy = prediction(predictions, test$Cannabis)
auc = performance(accuracy, measure="auc")
auc = auc@y.values[[1]]

performance = performance(accuracy, measure = "tpr", x.measure = "fpr")
plot(performance)
table(predictions,test$Cannabis)


#Task 4 - Improving the model
oobError = double(25)
testError = double(25)
medianError = double(25)
mtry = 0


for(mtry in round(sqrt(ncol(test))):(ncol(test)))
{
  fit = randomForest(as.factor(Cannabis)~., data=train, importance = TRUE, mtry=mtry, ntree=400)
  oobError[mtry-5] = fit$err.rate[,1][300]
  probs = predict(fit, newdata=test[-18])
  testError[mtry-5] = with(test, mean(probs!=test$Cannabis))
  medianError[mtry-5] =  (oobError[mtry-5]+testError[mtry-5])/2
  cat(mtry, " ")
}


mtryNum = c(5:29)
testData = data.frame(as.vector(testError), as.vector(oobError), 
                      as.vector(medianError), as.vector(mtryNum))

ggplot(testData, aes(testData[,4])) + 
  geom_line(aes(y = testData[,1], colour = "testError")) + 
  geom_line(aes(y = testData[,2], colour = "oobError") ) +
  geom_line(aes(y = testData[,3], colour = "meanError") ) +
  scale_x_continuous(name = "mtry", breaks = seq(5,29,by=2)) +
  scale_y_continuous(name = "Error %")

#Changing variables
trainingRowIndex = sample(1:nrow(df), 0.8*nrow(df))
train = df[trainingRowIndex,]
test = df[-trainingRowIndex,]

train$Gender = NULL
test$Gender = NULL
fit = randomForest(as.factor(Cannabis)~., data=train, importance = TRUE, ntree=300)

summary(fit)
importance(fit)
varImpPlot(fit)
#importance_frame = measure_importance(fit)

probabilities = predict(fit, test[,-18])
test_accuracy = mean(probabilities==test$Cannabis)
test_accuracy

#0.9148936 - base
#0.8882979 - without Alcohol
#0.8241563 - without Ascore
#0.8324468 - without Escore
#0.8271277 - without Gender
#0.8164894 - without Ascore, Nscore, Escore
#0.8271277 - without Ascore, Nscore, Escore, Ethnicity
#0.8191489 - without Ascore, Nscore, Escore, Ethnicity, Gender


df$Education = NULL

#df$Impulsiveness = NULL
df$Ethnicity = NULL
df$Gender = NULL

df$Ascore = NULL
df$Nscore = NULL
df$Escore = NULL

#require(randomForestExplainer)
#explain_forest(fit, interactions = TRUE, data = train)
