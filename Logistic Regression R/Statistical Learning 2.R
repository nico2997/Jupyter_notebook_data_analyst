brexit_data <- read.csv("brexit.csv",header=TRUE)

#Logistic Regression Model

fitted_logistic = glm(voteBrexit ~ abc1 + notBornUK + medianIncome + medianAge + withHigherEd, 
                      family = binomial, data = brexit_data)
summary(fitted_logistic)

n =344

#CHECK CONSISTENCY
summary_brexit = summary(fitted_logistic)$coefficient
summary_brexit
zc = qnorm(0.975)

#abc1 
estimate_abc1 = summary_brexit[2,1]
se_abc1 = summary_brexit[2,2]

min_abc1 = estimate_abc1 - zc*se_abc1
max_abc1 = estimate_abc1 + zc*se_abc1
print(min_abc1)
print(paste("CI abc1:", paste(min_abc1, max_abc1)))

#notBornUK
estimate_notBornUK = summary_brexit[3,1]
se_notBornUK = summary_brexit[3,2]

min_notBornUK = estimate_notBornUK - zc*se_notBornUK
max_notBornUK = estimate_notBornUK + zc*se_notBornUK
print(min_notBornUK)
print(paste("CI notBornUK:", paste(min_notBornUK, max_notBornUK)))

#medianIncome
estimate_medianIncome = summary_brexit[4,1]
se_medianIncome = summary_brexit[4,2]

min_medianIncome = estimate_medianIncome - zc*se_medianIncome
max_medianIncome = estimate_medianIncome + zc*se_medianIncome
print(min_medianIncome)
print(paste("CI medianIncome:", paste(min_medianIncome, max_medianIncome)))

#medianAge
estimate_medianAge = summary_brexit[5,1]
se_medianAge = summary_brexit[5,2]

min_medianAge = estimate_medianAge - zc*se_medianAge
max_medianAge = estimate_medianAge + zc*se_medianAge
print(min_medianAge)
print(paste("CI medianAge:", paste(min_medianAge, max_medianAge)))

#withHigherEd
estimate_withHigherEd = summary_brexit[6,1]
se_withHigherEd = summary_brexit[6,2]

min_withHigherEd = estimate_withHigherEd - zc*se_withHigherEd
max_withHigherEd = estimate_withHigherEd + zc*se_withHigherEd
print(min_withHigherEd)
print(paste("CI withHigherEd:", paste(min_withHigherEd, max_withHigherEd)))


#Model Selection
#1st stage
Model_abc1 = glm(voteBrexit ~ abc1, family = binomial, data = brexit_data)
Model_notBornUK = glm(voteBrexit ~ notBornUK, family = binomial, data = brexit_data)
Model_medianIncome = glm(voteBrexit ~ medianIncome, family = binomial, data = brexit_data)
Model_medianAge = glm(voteBrexit ~ medianAge, family = binomial, data = brexit_data)
Model_withHigherEd = glm(voteBrexit ~ withHigherEd, family = binomial, data = brexit_data)
summary(Model_abc1$aic)
summary(Model_notBornUK$aic)
summary(Model_medianIncome$aic)
summary(Model_medianAge$aic)
summary(Model_withHigherEd$aic)

#2nd stage
Model_withHigherEd_abc1 = glm(voteBrexit ~ withHigherEd + abc1, family = binomial, data = brexit_data)
Model_withHigherEd_notBornUK = glm(voteBrexit ~ withHigherEd + notBornUK, family = binomial, data = brexit_data)
Model_withHigherEd_medianIncome = glm(voteBrexit ~ withHigherEd + medianIncome, family = binomial, data = brexit_data)
Model_withHigherEd_medianAge = glm(voteBrexit ~ withHigherEd + medianAge, family = binomial, data = brexit_data)
summary(Model_withHigherEd_abc1$aic)
summary(Model_withHigherEd_notBornUK$aic)
summary(Model_withHigherEd_medianIncome$aic)
summary(Model_withHigherEd_medianAge$aic)

#3rd stage
Model_withHigherEd_abc1_notBornUK = glm(voteBrexit ~ withHigherEd + abc1 + notBornUK, 
                                        family = binomial, data = brexit_data)
Model_withHigherEd_abc1_medianIncome = glm(voteBrexit ~ withHigherEd + abc1 + medianIncome, 
                                           family = binomial, data = brexit_data)
Model_withHigherEd_abc1_medianAge = glm(voteBrexit ~ withHigherEd + abc1 + medianAge, 
                                        family = binomial, data = brexit_data)
summary(Model_withHigherEd_abc1_notBornUK$aic)
summary(Model_withHigherEd_abc1_medianIncome$aic)
summary(Model_withHigherEd_abc1_medianAge$aic)

#4th stage
Model_withHigherEd_abc1_medianAge_notBornUK = glm(voteBrexit ~ withHigherEd + abc1 + 
                                                    medianAge + notBornUK, family = binomial, 
                                                  data = brexit_data)
Model_withHigherEd_abc1_medianAge_medianIncome = glm(voteBrexit ~ withHigherEd + abc1 +
                                                       medianAge + medianIncome, family = binomial, 
                                                     data = brexit_data)
summary(Model_withHigherEd_abc1_medianAge_notBornUK$aic)
summary(Model_withHigherEd_abc1_medianAge_medianIncome$aic)

#5th stage
Model_withHigherEd_abc1_medianAge_medianIncome_notBornUK = glm(voteBrexit ~ withHigherEd + abc1 
                                                               + medianAge + medianIncome + notBornUK,
                                                               family = binomial, data = brexit_data)
summary(Model_withHigherEd_abc1_medianAge_medianIncome_notBornUK$aic)



#DECISION TREE

library(rpart)
#Fit the decision tree model
mytree = rpart(voteBrexit ~ withHigherEd + abc1 + medianAge + medianIncome + notBornUK, 
               data = brexit_data, method='class')
#Visualise the tree
plot(mytree)
text(mytree)
rpart.plot::prp(mytree, extra=106, type=3)
checktree = data.frame(mytree$variable.importance)
checktree
