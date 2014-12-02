

library(gdata)
library(ggplot2)
library(miscTools)
library(xlsx)
library(hash)
library(aod)
library(randomForest)

men_train <- read.csv('~/Desktop/men_train.csv')
women_train <- read.csv('~/Desktop/women_train.csv')
men_test <- read.csv('~/Desktop/men_test.csv')
women_test <- read.csv('~/Desktop/women_test.csv')

# men_train = men_train[men_train[["race"]] == 2 & men_train[["race_o"]] == 2,] 
# women_train = women_train[women_train[["race"]] == 2 & women_train[["race_o"]] == 2,]
# men_test = men_test[men_test[["race"]] == 2 & men_test[["race_o"]] == 2,] 
# women_test = women_test[women_test[["race"]] == 2 & women_test[["race_o"]] == 2,] 


# myRandomForest <- randomForest(men_train, na.action = "na.omit")
# fittedRandomForest <- model$Fit(trainFrame, features, "Producer.SixMonthOilPerFt")

mylogit <- glm(dec_o ~ dec_oAvg + attr_oOthersAvg + expnum + fun_oOthersAvg + sports, data = men_train, family = "binomial")
men_test$predicted_decision <- predict(mylogit, newdata = men_test, type = "response")
men_test["predicted_decision"] <- ifelse(men_test["predicted_decision"]>= 0.5,1, 0)
s = nrow(men_test[!is.na(men_test[["predicted_decision"]]) & men_test[["dec_o"]] == 0,])/nrow(men_test[!is.na(men_test[["predicted_decision"]]),])
t = nrow(men_test[!is.na(men_test[["predicted_decision"]]) & men_test[["predicted_decision"]] == men_test[["dec_o"]],])/nrow(men_test[!is.na(men_test[["predicted_decision"]]),])
print(c(s,t,(1-s)/(1 - t),t-s))

mylogit <- glm(dec_o ~ dec_oAvg + attr_oOthersAvg + expnum + exphappy + fun_oOthersAvg + sports, data = men_train, family = "binomial")
men_train$predicted_decision <- predict(mylogit, newdata = men_train, type = "response")
men_train["predicted_decision"] <- ifelse(men_train["predicted_decision"]>= 0.5,1, 0)
nrow(men_train[!is.na(men_train[["predicted_decision"]]) & men_train[["predicted_decision"]] == men_train[["dec_o"]],])/nrow(men_train[!is.na(men_train[["predicted_decision"]]),])


mylogit <- glm(dec_o ~ dec_oAvg + attr_oOthersAvg + expnum  + fun_oOthersAvg, data = women_train, family = "binomial")
women_test$predicted_decision <- predict(mylogit, newdata = women_test, type = "response")
women_test["predicted_decision"] <- ifelse(women_test["predicted_decision"]>= 0.5,1, 0)
s = nrow(women_test[!is.na(women_test[["predicted_decision"]]) & women_test[["dec_o"]] == 1,])/nrow(women_test[!is.na(women_test[["predicted_decision"]]),])
t = nrow(women_test[!is.na(women_test[["predicted_decision"]]) & women_test[["predicted_decision"]] == women_test[["dec_o"]],])/nrow(women_test[!is.na(women_test[["predicted_decision"]]),])
print(c(s,t,((1 - s)/(1 - t)),t-s))


mylogit <- glm(dec_o ~ dec_oAvg + attr_oOthersAvg + expnum + exphappy + fun_oOthersAvg + sports + go_out + yoga, data = women_train, family = "binomial")
women_train$predicted_decision <- predict(mylogit, newdata = women_train, type = "response")
women_train["predicted_decision"] <- ifelse(women_train["predicted_decision"]>= 0.5,1, 0)
nrow(women_train[!is.na(women_train[["predicted_decision"]]) & women_train[["predicted_decision"]] == women_train[["dec_o"]],])/nrow(women_train[!is.na(women_train[["predicted_decision"]]),])

# cor(men_test$like_oOthersAvg, men_test$like_o, use="pairwise.complete.obs")
# 
# mylogit <- glm(dec_o ~ attr_inter + sinc_inter + intel_inter +fun_inter +amb_inter + shar_inter + dec_oOthersAvg + like_oOthersAvg + attr_oOthersAvg +sinc_oOthersAvg +intel_oOthersAvg +fun_oOthersAvg +amb_oOthersAvg +attr_o +sinc_o + intel_o + fun_o + amb_o + shar_o, data = women_train, family = "binomial")
# women_test$predicted_decision <- predict(mylogit, newdata = women_test, type = "response")
# women_test["predicted_decision"] <- ifelse(women_test["predicted_decision"]>= 0.5,1, 0)
# cor(women_test$predicted_decision, women_test$dec_o, use="pairwise.complete.obs")
# nrow(women_test[women_test[["predicted_decision"]] == women_test[["dec_o"]],])
# nrow(women_test)
# nrow(women_test[women_test[["dec_o"]]==1,])
# nrow(women_test[women_test[["predicted_decision"]] == women_test[["dec_o"]],])/nrow(women_test["predicted_decision"][!is.na(women_test[["predicted_decision"]]),])
