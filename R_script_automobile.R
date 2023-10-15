data <- read.csv("C:/Users/hrith/OneDrive/Documents/RESUME_PROJECTS_COPY/Linear_regression_TSA/Automobile/automobile_cleaned.csv", header = TRUE)
View(data)

mpg<-data$mpg
cylinders<-data$cylinders
displacement<-data$displacement
horsepower<-data$horsepower
weight<-data$weight
acceleration<-data$acceleration
year<-data$year
origin<-data$origin
data$name<-NULL
data$X <- NULL

# Install and load the necessary packages
#install.packages("ggplot2")  # Only if you haven't installed ggplot2
#install.packages("dplyr")
library(ggplot2)
library(dplyr)
# Assuming your data frame is called 'data', select only numerical columns
numeric_data <- data %>%
  select_if(is.numeric)
# Create the scatterplot matrix with numerical columns
pairs(numeric_data)

# making dummy variables for origins
#install.packages("fastDummies")
library(fastDummies)
data_with_dummies_origin <- dummy_cols(data, select_columns = "origin")
View(data_with_dummies_origin)
data_with_dummies_origin$origin_1 <- NULL
data_with_dummies_origin$origin <- NULL
View(data_with_dummies_origin)
#data_with_dummies_origin <- data_with_dummies_origin[, -c(7,8)]
#View(data_with_dummies_origin)

# making dummy variables for years
data_with_dummies <- dummy_cols(data_with_dummies_origin, select_columns = "year")
View(data_with_dummies)
data_with_dummies$year_70 <- NULL
data_with_dummies$year <- NULL
View(data_with_dummies)

#spare_data <- data_with_dummies_origin
#View(spare_data)


# making dummy variables for year,origin,cylinders
#final_data <- dummy_cols(data_with_dummies_origin, select_columns = "cylinders")
#final_data$cylinders_3 <- NULL
#final_data$cylinders <- NULL
#View(final_data)


#fitting full linear model on final data
model_FL <- lm(mpg ~ ., data=data_with_dummies)
summary(model_FL)
View(vcov(model_FL))
anova(model_FL)

#finding the best 1st order model
summary(best_model_1st_order <- step(model_FL, direction='both'))
# best 1st order model keeps the borderline came of cylinders which we removed while doing manual backward elimination.

#checking the assumptions
sres_FL<-residuals(best_model_1st_order)/(3.04*sqrt(1-influence(best_model_1st_order)$hat))
hist(sres_FL)
boxplot(sres_FL,main="sres_FL")
plot(model_FL)
result_shapiro <- shapiro.test(sres_FL)
result_shapiro
#install.packages('nortest')
library(nortest)
result_anderson <- ad.test(sres_FL)
result_anderson
library(lmtest)
bptest(best_model_1st_order)


#finding the best 2nd order model
#final_data$name <- NULL
summary(model_FL_2nd_order <- lm(formula = mpg ~ (cylinders + displacement + horsepower + weight + acceleration + origin_2 + origin_3 + year_71 + year_72 + year_73 + year_74 + year_75 + year_76 + year_77 + year_78 + year_79 + year_80 + year_81 + year_82)^2, data = data_with_dummies))
summary(best_model_2nd_order <- step(model_FL_2nd_order, direction='both'))


#finding the best 2nd order model
#install.packages("MASS")
#library(MASS)
#summary(best_model_2nd_order_2 <- stepAIC(model_full_2nd_order, direction = "both"))


#checking the assumptions
sres_2nd_order<-residuals(best_model_2nd_order)/(2.179*sqrt(1-influence(best_model_2nd_order)$hat))
hist(sres_2nd_order)
boxplot(sres_2nd_order,main="sres_2nd_order")
plot(best_model_2nd_order)
result_shapiro <- shapiro.test(sres_2nd_order)
result_shapiro
#install.packages('nortest')
library(nortest)
result_anderson <- ad.test(sres_2nd_order)
result_anderson
#errors are not normally distributed

#test for homoskedasticity
bptest(best_model_2nd_order)
#the residuals are homoskedastic hence it passed all the test

#finding the best 2nd order model for the spare_data
#spare_data$name <- NULL
#summary(model_full_2nd_order <- lm(mpg ~ (displacement + horsepower + weight + cylinders + acceleration + year_71 + year_72 + year_73 + year_74 + year_75 + year_76 + year_77 + year_78 + year_79 + year_80 + year_81 + year_82 + origin_2 + origin_3)^2, data = spare_data))
#summary(best_model_2nd_order <- step(model_full_2nd_order, direction='both'))


#checking the assumptions for the spare_data
#sres_new_2nd_order<-residuals(best_model_2nd_order)/(2.179*sqrt(1-influence(best_model_2nd_order)$hat))
#hist(sres_new_2nd_order)
#boxplot(sres_new_2nd_order,main="sres_new_2nd_order")
#plot(best_model_2nd_order)
#result_shapiro <- shapiro.test(sres_new_2nd_order)
#result_shapiro
#install.packages('nortest')
#library(nortest)
#result_anderson <- ad.test(sres_new_2nd_order)
#result_anderson
#slightly well behaved but still errors are not normal


#finding the best 3rd order model for the spare_data
#summary(model_full_3rd_order <- lm(mpg ~ (displacement + horsepower + weight + cylinders + acceleration + year_71 + year_72 + year_73 + year_74 + year_75 + year_76 + year_77 + year_78 + year_79 + year_80 + year_81 + year_82 + origin_2 + origin_3)^3, data = spare_data))
#summary(best_model_3rd_order <- step(model_full_3rd_order, direction='both'))


#checking the assumptions for the spare_data
#sres_new_2nd_order<-residuals(best_model_2nd_order)/(2.179*sqrt(1-influence(best_model_2nd_order)$hat))
#hist(sres_new_2nd_order)
#boxplot(sres_new_2nd_order,main="sres_new_2nd_order")
#plot(best_model_2nd_order)
#result_shapiro <- shapiro.test(sres_new_2nd_order)
#result_shapiro
#install.packages('nortest')
#library(nortest)
#result_anderson <- ad.test(sres_new_2nd_order)
#result_anderson
#again errors are not normal
























