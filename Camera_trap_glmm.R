#Load necessary libraries
library(lme4)
library(dplyr)
library(corrplot)
library(ggplot2)

#Read in camera trap data
#Replace with path to data
location <- "/home/mattyd797/camera_trap/camera_trap_data.csv"
test_data <- read.csv(file= location) %>%
              mutate(camera.=as.factor(camera.),
                     night_day = as.factor(night_day),
                     precipitation_no = as.factor(precipitation_no), 
                     species = as.factor(species)); str(test_data)

#Evaluate distribution of data
boxplot(test_data$contrast)
boxplot(test_data$temp_C)
boxplot(test_data$windspeed..km.hr.)
boxplot(test_data$cloud.cover....)

#Center&Standardize
test_data$contrast <- scale(test_data$contrast, center = TRUE, scale = TRUE)
test_data$temp_C <- scale(test_data$temp_C, center = TRUE, scale = TRUE)
test_data$windspeed..km.hr. <- scale(test_data$windspeed..km.hr., center = TRUE, scale = TRUE)
test_data$cloud.cover.... <- exp(test_data$cloud.cover....) #skewed

#Remove outliers - total of 3 outliers
Q1_temp <- quantile(test_data$temp_C, 0.25)
Q3_temp <- quantile(test_data$temp_C, 0.75)
IQR <- Q3_temp - Q1_temp
lower_bound <- Q1_temp - 1.5 * IQR
upper_bound <- Q3_temp + 1.5 * IQR
test_data<- test_data[test_data$temp_C >= lower_bound & test_data$temp_C <= upper_bound, ]

#Define y
dependent_variable <- as.factor(test_data$success_failure)

# Calculate correlation matrix
conf_data <- test_data[,1:6]%>%
  mutate(night_day = as.numeric(night_day),
         precipitation_no = as.numeric(precipitation_no),
         temp_C = as.numeric(temp_C),
         windspeed..km.hr. = as.numeric(windspeed..km.hr.))

correlation_matrix <- cor(conf_data)
corrplot(correlation_matrix, method = 'number')

#Define models
null_model <- glmer(dependent_variable ~ 1 + (1|test_data$camera.),
                   family = binomial); summary(null_model)

model1 <- glmer(dependent_variable ~ test_data$contrast +
                                    test_data$windspeed..km.hr. +
                                    test_data$cloud.cover....+
                                    test_data$temp_C +
                                    test_data$precipitation_no +
                                    (1|test_data$camera.),
                family = binomial(link = "logit"))

model2 <- glmer(dependent_variable ~ test_data$contrast +
                  (1|test_data$camera.),
                family = binomial(link = "logit"))

model3 <- glmer(dependent_variable ~ test_data$contrast +
                  test_data$windspeed..km.hr. +
                  (1|test_data$camera.),
                family = binomial(link = "logit"))

model4 <- glmer(dependent_variable ~ test_data$contrast +
                  test_data$windspeed..km.hr. +
                  test_data$precipitation_no +
                  (1|test_data$camera.),
                family = binomial(link = "logit"))

model5 <- glmer(dependent_variable ~ test_data$contrast +
                  test_data$windspeed..km.hr. +
                  test_data$precipitation_no +
                  test_data$temp_C +
                  (1|test_data$camera.),
                family = binomial(link = "logit"))

#ANOVA
lr_test <- anova(null_model, model4,model5, model1);lr_test
anova(null_model, model4)

#Residual Plots
deviance_residuals <- resid(model5, type = "deviance")

residual_data <- data.frame(
  Fitted_Values = fitted(model5),
  Deviance_Residuals = deviance_residuals
)

ggplot(residual_data, aes(x = Fitted_Values, y = Deviance_Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Deviance Residuals")

#QQplots
qqnorm(model_resid)
qqline(model_resid, col = "red")

#Scale-Location Plot
plot(fitted(model5), sqrt(abs(model_resid)), main = "Scale-Location Plot", xlab = "Fitted Values", ylab = "Sqrt(|Residuals|)")

#Model summary statistics
summary(model)
deviance(model)
logLik(model)
VarCorr(model)

