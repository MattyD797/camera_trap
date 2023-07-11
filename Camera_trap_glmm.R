install.packages("lme4")
library(lme4)
library(dplyr)
library(corrplot)

test_data <- read.csv(file= "C:\\Users\\mtd72\\Desktop\\test.csv") %>%
              mutate(camera.=as.factor(camera.),
                     night_day = as.factor(night_day),
                     precipitation_no = as.factor(precipitation_no))
str(test_data)
test_data$camera. <- test_data$camera.
dependent_variable <- as.factor(test_data$success_failure)

# Calculate correlation matrix

conf_data <- test_data[,1:6]%>%
  mutate(night_day = as.numeric(night_day),
         precipitation_no = as.numeric(precipitation_no),
         temp_C = as.numeric(temp_C),
         windspeed..km.hr. = as.numeric(windspeed..km.hr.))

correlation_matrix <- cor(conf_data)
corrplot(correlation_matrix, method = 'number')

null_model <- glmer(dependent_variable ~ 1 + (1|test_data$camera.),
                   family = binomial)

summary(null_model)



model1 <- glmer(dependent_variable ~ test_data$contrast +
                                    test_data$temp_C +
                                    test_data$windspeed..km.hr. +
                                    test_data$cloud.cover....+
                                    test_data$night_day +
                                    test_data$precipitation_no +
                                    (1|test_data$camera.),
                family = binomial)
model2 <- glmer(dependent_variable ~ test_data$contrast +
                  (1|test_data$camera.),
                family = binomial)
model3 <- glmer(dependent_variable ~ test_data$contrast +
                  test_data$windspeed..km.hr. +
                  (1|test_data$camera.),
                family = binomial)
model4 <- glmer(dependent_variable ~ test_data$contrast +
                  test_data$windspeed..km.hr. +
                  test_data$precipitation_no +
                  (1|test_data$camera.),
                family = binomial)


model5 <- glmer(dependent_variable ~ test_data$contrast +
                  test_data$windspeed..km.hr. +
                  test_data$precipitation_no +
                  test_data$temp_C +
                  (1|test_data$camera.),
                family = binomial)


lr_test <- anova(null_model, model2, model3, model4,model5, model1);lr_test

model_resid <- resid(model5)

plot(fitted(model5), model_resid)

# Create Residuals vs. Independent Variables plots
plot(test_data$contrast, model_resid, xlab = "Independent Variable 1",
     ylab = "Residuals", main = "Residuals vs. Independent Variable 1")
plot(test_data$temp_C, model_resid, xlab = "Independent Variable 2",
     ylab = "Residuals", main = "Residuals vs. Independent Variable 2")

summary(model)
deviance(model)
logLik(model)
VarCorr(model)

