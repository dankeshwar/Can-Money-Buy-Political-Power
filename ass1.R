#import dataset
c_data = read.csv("D:\\ABA\\Assignment_1\\election_campaign_data.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?")) 

library(dplyr)

#drop variables
c_data = select(c_data, -c(cand_id, last_name, first_name, twitterbirth, facebookdate, facebookjan, youtubebirth))
c_data$gen_election <- ifelse(c_data$gen_election == "L", 0, 1)
#c_data

#check the levels for columns
c_data$twitter = as.factor(c_data$twitter)
c_data$facebook = as.factor(c_data$facebook)
c_data$youtube = as.factor(c_data$youtube)
c_data$cand_ici = as.factor(c_data$cand_ici)
c_data$gen_election = as.factor(c_data$gen_election)

levels(c_data$gen_election)
#variables_to_factors = c("twitter", "facebook", "youtube", "cand_ici", "gen_election")
#c_data = as.factor(variables_to_factors                      )

#c_data.factor
view(c_data)



#Check the sum and percent of missing values in the table
sum(is.na(c_data))
mean(is.na(c_data))

c_data = na.omit(c_data)
#c_data

sum(is.na(c_data))
mean(is.na(c_data))

# Random
train_camp = c_data %>% sample_frac(.70)
test_camp = anti_join(c_data, train_camp)

set.seed(32)
summary(train_camp)
library(randomForest)

#train_camp = c_data %>% sample_frac(.7)

rf = randomForest(gen_election~., data=train_camp,ntree = 10 , importance=T, proximity=T)
print(rf)


#train_camp <- train_camp[complete.cases(train_camp),]
rf = randomForest(gen_election~., data=train_camp,ntree = 20 , importance=T, proximity=T)
print(rf)

rf = randomForest(gen_election~., data=train_camp,ntree = 30 , importance=T, proximity=T)
print(rf)

rf = randomForest(gen_election~., data=train_camp,ntree = 40 , importance=T, proximity=T)
print(rf)

rf = randomForest(gen_election~., data=train_camp,ntree = 50 , importance=T, proximity=T)
print(rf)

rf = randomForest(gen_election~., data=train_camp,ntree = 60 , importance=T, proximity=T)
print(rf)

rf = randomForest(gen_election~., data=train_camp,ntree = 70 , importance=T, proximity=T)
print(rf)

rf = randomForest(gen_election~., data=train_camp,ntree = 70 , importance=T, proximity=T)
print(rf)

rf = randomForest(gen_election~., data=train_camp,ntree = 69 , importance=T, proximity=T)
print(rf)


rf = randomForest(gen_election~., data=train_camp,ntree = 80 , importance=T, proximity=T)
print(rf)

#find optimal mtry value
par("mar")
par(mar=c(1,1,1,1))
mtry = tuneRF(train_camp[-26], train_camp$gen_election, ntreeTry = 70, stepFactor = 1.5, improve = 0.01, trace= TRUE, plot= TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)


# Caret
library(caret)
predicted_values <- predict(rf, test_camp,type= "prob")
threshold <- 0.5
pred <- factor( ifelse(predicted_values[,2] > threshold, 1, 0) )
levels(test_camp$gen_election)[2]
confusionMatrix(pred, test_camp$gen_election, 
                positive = levels(test_camp$gen_election)[2])

library(ROCR)

library(ggplot2)

predicted_values <- predict(rf, test_camp,type= "prob")[,2] 
pred <- prediction(predicted_values, test_camp$gen_election)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="RF")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

importance(rf)
varImpPlot(rf)


# Building ANN

str(train_camp)

library(dplyr)
library(nnet)

ann <- nnet(gen_election~., data=train_camp, size=5, maxit=1000,
            trace = F)
summary(ann)

print(ann)

library(caret)
predicted_values <- predict(ann, test_camp,type= "raw")
head(predicted_values)
threshold = 0.5
pred = factor(ifelse(predicted_values[,1] > threshold, 1, 0))
head(pred)
levels(test_camp$gen_election)[1]
confusionMatrix(pred, test_camp$gen_election, 
                positive = levels(test_camp$gen_election)[2])
library(ROCR)

library(ggplot2)
predicted_values <- predict(ann, test_camp,type= "raw")
pred <- prediction(predicted_values, test_camp$gen_election)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
# I know, the following code is bizarre. Just go with it.
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="ANN")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))




ann <- nnet(gen_election~., data=train_camp, size=24, maxit=1000,
            trace = F)
print(ann)
library(caret)
predicted_values <- predict(ann, test_camp,type= "raw")
head(predicted_values)
threshold = 0.5
pred = factor(ifelse(predicted_values[,1] > threshold, 1, 0))
head(pred)
levels(test_camp$gen_election)[1]
confusionMatrix(pred, test_camp$gen_election, 
                positive = levels(test_camp$gen_election)[2])
library(ROCR)

library(ggplot2)
predicted_values <- predict(ann, test_camp,type= "raw")
pred <- prediction(predicted_values, test_camp$gen_election)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
# I know, the following code is bizarre. Just go with it.
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="ANN")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))


ftable(xtabs(~twitter+gen_election, data = c_data))
ftable(xtabs(~facebook+gen_election, data = c_data))
ftable(xtabs(~youtube+gen_election, data = c_data))
