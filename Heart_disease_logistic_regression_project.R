library(tidyverse)
library(ggplot2)


url <- "https://raw.githubusercontent.com/samyvivo/Heart_disease/refs/heads/main/Heart_disease_raw_data.txt"

cols = c("age",
         "sex",# 0 = female, 1 = male
         "cp", # chest pain
         # 1 = typical angina,
         # 2 = atypical angina,
         # 3 = non-anginal pain,
         # 4 = asymptomatic
         "trestbps", # resting blood pressure (in mm Hg)
         "chol", # serum cholestoral in mg/dl
         "fbs",  # fasting blood sugar if less than 120 mg/dl, 1 = TRUE, 0 = FALSE
         "restecg", # resting electrocardiographic results
         # 1 = normal
         # 2 = having ST-T wave abnormality
         # 3 = showing probable or definite left ventricular hypertrophy
         "thalach", # maximum heart rate achieved
         "exang",   # exercise induced angina, 1 = yes, 0 = no
         "oldpeak", # ST depression induced by exercise relative to rest
         "slope", # the slope of the peak exercise ST segment
         # 1 = upsloping
         # 2 = flat
         # 3 = downsloping
         "ca", # number of major vessels (0-3) colored by fluoroscopy
         "thal", # this is short of thallium heart scan
         # 3 = normal (no cold spots)
         # 6 = fixed defect (cold spots during rest and exercise)
         # 7 = reversible defect (when cold spots only appear during exercise)
         "hd" # (the predicted attribute) - diagnosis of heart disease
         # 0 if less than or equal to 50% diameter narrowing
         # 1 if greater than 50% diameter narrowing
)



data <- read.csv(url, header = F , col.names = cols)

head(data)
glimpse(data)
View(data)


data[data == "?"]  <- NA

##turn data to factor and recode some values

data <- data %>% 
  mutate(sex=recode_factor(sex, "1"="M", "0"="F"))

data <- data %>% 
  mutate(hd=recode_factor(hd, "0"="Healthy",
                              "1"="UnHealthy",
                              "2"="UnHealthy",
                              "3"="UnHealthy",
                              "4"="UnHealthy"))

data <- data %>% 
  mutate(across(c(cp,fbs,restecg,exang,slope,ca,thal), as.factor))


## quality control by making sure all of the factor...
## levels of hd are represented by people with and without heart disease (hd)

xtabs(~ hd + sex, data=data)
xtabs(~ hd + cp, data=data)
xtabs(~ hd + fbs, data=data)
xtabs(~ hd + restecg, data=data)
xtabs(~ hd + exang, data=data)
xtabs(~ hd + slope, data=data)
xtabs(~ hd + ca, data=data)
xtabs(~ hd + thal, data=data)

## logistic regression model for sex for see the corelation

logistic_sex_model <- glm(hd~sex, data=data, family = "binomial")

summary(logistic_sex_model)

female_log_odd <- log(25/72)
female_log_odd

male.log.odds.ratio <- log((114 / 92) / (25/72))
male.log.odds.ratio


## logistic regression...
## Null deviance = 2*(0 - LogLikelihood(null model))
##               = -2*LogLikihood(null model)
## Residual deviance= 2*(0 - LogLikelihood(proposed model))
##                   = -2*LogLikelihood(proposed model)
ll_null <- logistic_sex_model$null.deviance/-2
ll_proposed <- logistic_sex_model$deviance/-2

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll_null - ll_proposed) / ll_null

## chi-square value = 2*(LL(Proposed) - LL(Null))
## p-value = 1 - pchisq(chi-square value, df = 2-1)
1 - pchisq(2*(ll_proposed - ll_null), df=1)
1 - pchisq((logistic_sex_model$null.deviance - logistic_sex_model$deviance), df=1)

## logistic regression predicts, given...
## that a patient is either female or male (and no other data about them).
predicted.data <- data.frame(
  probability_of_hd=logistic_sex_model$fitted.values,
  sex=data$sex)

## logistic regression predicts, given...
## that a patient is either female or male (and no other data about them).

predicted_data <- data.frame(
  probability_of_hd=logistic_sex_model$fitted.values,
  sex=data$sex)

## plot the data
ggplot(data=predicted_data, aes(x=sex, y=probability_of_hd)) +
  geom_point(aes(color=sex), size=5) +
  xlab("Sex") +
  ylab("Predicted probability of getting heart disease")

## two probabilities (one for females and one for males)...
## use for a table to summarize the predicted probabilities.
xtabs(~ probability_of_hd + sex, data=predicted.data)


## Finally use all of the data available to predict heart disease
logistic <- glm(hd ~ ., data=data, family="binomial")
summary(logistic)

##calculate the overall "Pseudo R-squared" and its p-value
ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null

## The p-value for the R^2
1 - pchisq(2*(ll.proposed - ll.null), df=(length(logistic$coefficients)-1))


#clean NA values
data_clean <- na.omit(data)

predicted_data <- data.frame(
  probability_of_hd=logistic$fitted.values,
  hd=data_clean$hd)


predicted_data <- predicted_data[
  order(predicted_data$probability_of_hd, decreasing=FALSE),]
predicted_data$rank <- 1:nrow(predicted_data)


#plot the predicted probabilities for each sample having
#heart disease and color by whether or not they actually had heart disease

ggplot(data=predicted_data, aes(x=rank, y=probability_of_hd)) +
  geom_point(aes(color=hd), alpha=1, shape=4, stroke=2) +
  xlab("Indexes") +
  ylab("Predicted probability of getting heart disease")

