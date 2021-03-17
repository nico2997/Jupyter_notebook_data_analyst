mushrooms_data <- read.csv("mushrooms.csv",header=TRUE)
mushrooms_data

head(mushrooms_data)
str(mushrooms_data)
summary(mushrooms_data)

#test_idx2 = sample(dim(mushrooms_data)[1], 20)
#test_data2 = mushrooms_data[test_idx2, ]
#train_data2 = mushrooms_data[-test_idx2, ]
#dim(train_data2)
#dim(test_data2)

#install.packages("randomForest")

#Random Forest & Prediction
library(randomForest)
model1 = randomForest(Edible ~ CapShape, data= mushrooms_data)
model1$confusion

model2 = randomForest(Edible ~ CapSurface, data= mushrooms_data)
model2$confusion

model3 = randomForest(Edible ~ CapColor, data= mushrooms_data)
model3$confusion

model4 = randomForest(Edible ~ Odor, data= mushrooms_data)
model4$confusion

model5 = randomForest(Edible ~ Height, data= mushrooms_data)
model5$confusion

#model$importance

prediction1 = predict(model1, newdata= mushrooms_data, type = "response")
summary(prediction1)
length(prediction1)

predict_quality1 = table(mushrooms_data[,1], prediction1)
predict_quality1

prediction2 = predict(model2, newdata= mushrooms_data, type = "response")
summary(prediction2)
length(prediction2)

predict_quality2 = table(mushrooms_data[,1], prediction2)
predict_quality2

prediction3 = predict(model3, newdata= mushrooms_data, type = "response")
summary(prediction3)
length(prediction3)

predict_quality3 = table(mushrooms_data[,1], prediction3)
predict_quality3

prediction4 = predict(model4, newdata= mushrooms_data, type = "response")
summary(prediction4)
length(prediction4)

predict_quality4 = table(mushrooms_data[,1], prediction4)
predict_quality4


prediction5 = predict(model5, newdata= mushrooms_data, type = "response")
summary(prediction5)
length(prediction5)

predict_quality5 = table(mushrooms_data[,1], prediction5)
predict_quality5

factor = colnames(mushrooms_data)
factor = factor[2:length(factor)]
correct_factor = rep(NA, length(factor))
correct_factor
factor
df_correct = data.frame("Models"= factor, "Total correct prediction" = correct_factor)

rownames(df_correct)[1] = "model 1"
rownames(df_correct)[2] = "model 2"
rownames(df_correct)[3] = "model 3"
rownames(df_correct)[4] = "model 4"
rownames(df_correct)[5] = "model 5"


for(i in 1:length(factor)){
  models = as.formula(paste("Edible ~", factor[i]))
  forest = randomForest(formula=models, data = mushrooms_data)
  prediction = predict(forest, newdata = mushrooms_data, type = "response")
  correct_factor[i] = sum(mushrooms_data$Edible == prediction)
}
plot(correct_factor, xlab = "model", ylab="number of correct prediction")


#Cross Validation

#install.packages("dagR")
library(dagR)
comb = allCombs(factor)
comb = comb[-1,]
comb
list_formula = rep(NA, nrow(comb))
list_formula
for(i in 1:nrow(comb)){
  list_formula[i]=paste(na.omit(unlist(comb[i,])), collapse ="+")
}
list_formula

gg = 0.5*nrow(mushrooms_data)
gg
winner = rep(NA,gg )

for(iteration in 1:10){

idx = sample(1:8124, gg)
train_data = mushrooms_data[idx, ]
test_data = mushrooms_data[-idx, ]
dim(test_data)
dim(train_data)


predictive = rep(NA, length(list_formula))
for(i in 1:length(list_formula)){
  models = as.formula(paste("Edible~", list_formula[i]))
  forest = randomForest(formula=models, data = train_data)
  prediction = predict(forest, newdata = test_data, type = "response")
  predictive[i] = sum(test_data$Edible == prediction)
}
plot(predictive, xlab = "model", ylab="number of correct prediction")


winner[iteration] = which.max(predictive)
}
length(list_formula)

hist(winner, breaks = seq(0, 31 , 1), xlab='Model', ylab='Frequency', main='')
predictive

df_predictive = data.frame("formula" = list_formula, "Total correct prediction" = predictive)

check_over3000 = list_formula[predictive > 3000]
check_over3000


