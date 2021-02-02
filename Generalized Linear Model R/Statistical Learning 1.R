medal_data <- read.csv("medal_pop_gdp_data_statlearn.csv",header=TRUE)

predict_2008 = glm(Medal2008 ~ GDP+Population, data= medal_data)
summary(predict_2008)

predict_2012 = glm(Medal2012 ~ GDP+Population, data= medal_data)
summary(predict_2012)


#CHECK CONSISTENCY
#2008 Data
summary_table_2008 = summary(predict_2008)$coefficient
summary_table_2008
t1= qt(0.975 , 68)

#GDP 2008
estimate_GDP2008= summary_table_2008[2,1]
sterr_GDP2008= summary_table_2008[2,2]

min_GDP2008 = estimate_GDP2008 - t1*sterr_GDP2008
max_GDP2008 = estimate_GDP2008 + t1*sterr_GDP2008
print(paste("GDP 2008:", paste(min_GDP2008, max_GDP2008)))

#Population 2008
estimate_population2008= summary_table_2008[3,1]
sterr_population2008= summary_table_2008[3,2]

min_population2008 = estimate_population2008 - t1*sterr_population2008
max_population2008 = estimate_population2008 + t1*sterr_population2008
print(paste("Population 2008:", paste(min_population2008, max_population2008)))


#2012 Data
summary_table_2012 = summary(predict_2012)$coefficient
summary_table_2012
t2= qt(0.975 , 68)

#GDP 2012
estimate_GDP2012= summary_table_2012[2,1]
sterr_GDP2012= summary_table_2012[2,2]

min_GDP2012 = estimate_GDP2012 - t1*sterr_GDP2012
max_GDP2012 = estimate_GDP2012 + t1*sterr_GDP2012
print(paste("GDP 2012:", paste(min_GDP2012, max_GDP2012)))

#Population 2012
estimate_population2012= summary_table_2012[3,1]
sterr_population2012= summary_table_2012[3,2]

min_population2012 = estimate_population2012 - t1*sterr_population2012
max_population2012 = estimate_population2012 + t1*sterr_population2012
print(paste("Population 2012:",paste(min_population2012, max_population2012)))


#Regression 2012 predict 2016
new_data = data.frame(medal_data$Population, medal_data$GDP)
colnames(new_data) <- c("Population", "GDP")
predict_2016 = predict(predict_2012,newdata = new_data)
predict_2016
Compare_medal = data.frame(Country = medal_data$Country, Prediction = predict_2016, Actual = medal_data$Medal2016)
Compare_medal

par(mfrow=c(1,1))
plot(predict_2016, type="line", main ="Actual-Prediction 2016 Comparison", 
     xlab = "Country number", ylab = "Total Medals", col="red", lwd = 2)
lines(medal_data$Medal2016, type="line", main ="Actual 2016", 
      xlab = "Country number", ylab = "Total Medals", col="blue", lwd = 2)
legend(1, 95, legend=c("Prediction", "Actual"),
       col=c("red", "blue"), lty=1, lwd=2, cex=0.8)
#predict_2016 = glm(Medal2016 ~ Medal2012, data=medal_data)
#predict_2016

 #scatter plot
library(plotly)

t <- list(
  family = "sans serif",
  size = 14,
  color = toRGB("black"))

plot_ly(Compare_medal, x = ~Prediction, y = ~Actual, type="scatter",
        text = ~Country) %>% 
  add_text(textfont = t, textposition = "top right") %>%
  add_markers()%>%
  layout(title = "Comparison Between Prediction and Actual Medal count", 
         xaxis = list(title = "2016 Medal Prediction"), yaxis = list(title = "2016 Actual Medal"))%>%
  add_lines(x = Compare_medal$Prediction, y = fitted(lm(medal_data$Medal2016 ~ predict_2016)))

plot_ly(Compare_medal, x = ~Prediction, y = ~Actual, type="scatter",
        text = ~Country) %>% 
  add_text(textfont = t, textposition = "top right") %>%
  add_markers()%>%
  layout(xaxis = list(range=c(0,30)), yaxis = list(range=c(0,30))) %>%
  layout(title = "Comparison Between Prediction and Actual Medal count", 
         xaxis = list(title = "2016 Medal Prediction"), yaxis = list(title = "2016 Actual Medal"))%>%
  add_lines(x = Compare_medal$Prediction, y = fitted(lm(medal_data$Medal2016 ~ predict_2016)))

#text(predict_2016, medal_data$Medal2016, labels=Compare_medal$Country, cex=0.7, font=1)
#abline(lm(medal_data$Medal2016 ~ predict_2016),col="red")

difference = predict_2016 - medal_data$Medal2016
Compare_medal2 = data.frame(Country =medal_data$Country, Difference = difference)

box1 = boxplot(difference,col="red",xlab="Difference",ylab="Difference")
grid(nx=NA, ny=NULL)
label = round(box1$stats,2)
outlier_value = boxplot.stats(difference)$out
mtext(paste("Outliers: ", paste(outlier_value, collapse=", ")), cex=0.9)
text(x = col(box1$stats) - 0.23, y = label, labels = label)

#Fit linear regressions models for the total medal count in 2012
predict_2012 = glm(Medal2012 ~ GDP+Population, data= medal_data)
predict_2012_population = glm(Medal2012 ~ Population, data= medal_data)
predict_2012_GDP = glm(Medal2012 ~ GDP, data= medal_data)

summary(predict_2012_GDP)
summary(predict_2012_population)
summary(predict_2012)


#Cross-Validation

winner = rep(NA, 35)
for (iteration in 1:35){
  
idx = sample(1:71, 35)
train_data = medal_data[idx , ]
test_data = medal_data[-idx , ]

formulas = c("Medal2012 ~ Population", "Medal2012 ~ GDP", "Medal2012 ~ Population + GDP")

predictive_log_likelihood = rep(NA, length(formulas))
for (i in 1:length(formulas)){
  #First fit a linear regression model with the training data
  current_model = glm(formula = formulas[i], data = train_data)
  #Extract the 'dispersion parameter' from the model - recall this is the unbiased estimate for the residual variance.
  sigma = sqrt(summary(current_model)$dispersion)
  #Now use this model to evaulate the probability of the test outputs
  #Get the predicted mean for each new data point
  ypredict_mean = predict(current_model, test_data)
  #Now calculate the predictive log probability by summing the
  #log probability of each output value in the test data
  predictive_log_likelihood[i] = sum(dnorm(test_data$Medal2012, ypredict_mean, sigma, log=TRUE))
}
plot(1:length(formulas), predictive_log_likelihood,xlab="Model Number", ylab="Log Probability")

winner[iteration] = which.max(predictive_log_likelihood)
}
hist(winner, breaks = seq(0.5, 7.5, 1), xlab='Model', ylab='Frequency', main='')

     
#Predict 2016 model
new_data2 = data.frame(medal_data$GDP)
colnames(new_data2) <- c("GDP")
predict_2016_GDP = predict(predict_2012_GDP,newdata = new_data2)
predict_2016_GDP

new_data3 = data.frame(medal_data$Population)
colnames(new_data3) <- c("Population")
predict_2016_Pop = predict(predict_2012_population,newdata = new_data3)
predict_2016_Pop

new_data = data.frame(medal_data$Population, medal_data$GDP)
colnames(new_data) <- c("Population", "GDP")
predict_2016 = predict(predict_2012,newdata = new_data)
predict_2016

Model_comparison = data.frame(Country = medal_data$Country, Population_model = predict_2016_Pop,
                              GDP_model=predict_2016_GDP, PopGDP_model = predict_2016, 
                              Actual = medal_data$Medal2016)
Model_comparison

#difference_GDP = predict_2016_GDP - medal_data$Medal2016
#rmse_GDP = sqrt(mean(difference_GDP^2))
#rmse_GDP

library(Metrics)
rmse1 = rmse(predict_2016_Pop,medal_data$Medal2016)
rmse2 = rmse(predict_2016_GDP,medal_data$Medal2016)
rmse3 = rmse(predict_2016,medal_data$Medal2016)
rmse_model = data.frame(model1_Population = rmse1, model2_GDP = rmse2, model3_GDPPopulation = rmse3)
rmse_model

plot(1:3,c(rmse1,rmse2,rmse3) ,xlab="Model Number", ylab="RMSE")
