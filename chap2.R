# 1. Default flag definition and data preparation
# 1.1. Import data
oneypd <- read.csv('chap2oneypd.txt')

library(dplyr)
# 1.1.1. Data overview: data content and format
dplyr::glimpse(oneypd)

# 1.1.2. Date format
library(vars)
oneypd <- dplyr::mutate_at(oneypd,vars(contains('date')),funs(as.Date)) 
class(oneypd$origination_date)

# 1.1.3. Round arrears count fields
oneypd$max_arrears_12m <- round(oneypd$max_arrears_12m,4)
oneypd$arrears_months <- round(oneypd$arrears_months,4)

# 1.2. Default flag definition
oneypd <- dplyr::mutate(oneypd,default_event = if_else(oneypd$arrears_event ==1|
                                                       oneypd$term_expiry_event == 1| 
                                                       oneypd$bankrupt_event == 1,1,0))

# 1.3. Database split in train and test samples
# Recode default event variables for more convenient use
# 0-default, 1-non-default
oneypd$default_flag <- dplyr::if_else(oneypd$default_event == 1,0,1)

# Perform a stratified sampling: 70% train and 30% test
library(caret)
set.seed(2122)
train.index <- caret::createDataPartition(oneypd$default_event,p = .7, list = FALSE)
train <- oneypd[train.index,]
test <- oneypd[-train.index,]

# 2. Univariate analysis
# Information Value (IV) assessment
library(smbinning)
iv_analysis <- smbinning.sumiv(df = train, y = 'default_flag')

# Plot IV summary table
par(mfrow=c(1,1))
smbinning.sumiv.plot(iv_analysis,cex=1)

# 2.1 woe based on binning analysis

#Bureau score:
train$woe_bureau_score<- rep(NA, length(train$bureau_score))
train$woe_bureau_score[which(is.na(train$bureau_score))] <- -0.0910
train$woe_bureau_score[which(train$bureau_score <= 308)] <- -0.7994
train$woe_bureau_score[which(train$bureau_score > 308 & train$bureau_score <= 404)] <- -0.0545
train$woe_bureau_score[which(train$bureau_score > 404 & train$bureau_score <= 483)] <-  0.7722
train$woe_bureau_score[which(train$bureau_score > 483)] <-  1.0375

test$woe_bureau_score<- rep(NA, length(test$bureau_score))
test$woe_bureau_score[which(is.na(test$bureau_score))] <- -0.0910
test$woe_bureau_score[which(test$bureau_score <= 308)] <- -0.7994
test$woe_bureau_score[which(test$bureau_score > 308 & test$bureau_score <= 404)] <- -0.0545
test$woe_bureau_score[which(test$bureau_score > 404 & test$bureau_score <= 483)] <-  0.7722
test$woe_bureau_score[which(test$bureau_score > 483)] <-  1.0375

#CC utilization:

train$woe_cc_util<- rep(NA, length(train$cc_util))
train$woe_cc_util[which(is.na(train$cc_util))] <- 0
train$woe_cc_util[which(train$cc_util <= 0.55)] <- 1.8323
train$woe_cc_util[which(train$cc_util > 0.55 & train$cc_util <= 0.70)] <- -0.4867
train$woe_cc_util[which(train$cc_util > 0.70 & train$cc_util <= 0.85)] <- -1.1623
train$woe_cc_util[which(train$cc_util > 0.85)] <- -2.3562

test$woe_cc_util<- rep(NA, length(test$cc_util))
test$woe_cc_util[which(is.na(test$cc_util))] <- 0
test$woe_cc_util[which(test$cc_util <= 0.55)] <- 1.8323
test$woe_cc_util[which(test$cc_util > 0.55 & test$cc_util <= 0.70)] <- -0.4867
test$woe_cc_util[which(test$cc_util > 0.70 & test$cc_util <= 0.85)] <- -1.1623
test$woe_cc_util[which(test$cc_util > 0.85)] <- -2.3562

#Number of CCJ events:

train$woe_num_ccj<- rep(NA, length(train$num_ccj))
train$woe_num_ccj[which(is.na(train$num_ccj))] <- -0.0910
train$woe_num_ccj[which(train$num_ccj <= 0)] <- 0.1877
train$woe_num_ccj[which(train$num_ccj > 0 & train$num_ccj <= 1)] <- -0.9166
train$woe_num_ccj[which(train$num_ccj > 1)] <- -1.1322

test$woe_num_ccj<- rep(NA, length(test$num_ccj))
test$woe_num_ccj[which(is.na(test$num_ccj))] <- -0.0910
test$woe_num_ccj[which(test$num_ccj <= 0)] <- 0.1877
test$woe_num_ccj[which(test$num_ccj > 0 & test$num_ccj <= 1)] <- -0.9166
test$woe_num_ccj[which(test$num_ccj > 1)] <- -1.1322

#Maximum arrears in previous 12 months:

train$woe_max_arrears_12m<- rep(NA, length(train$max_arrears_12m))
train$woe_max_arrears_12m[which(is.na(train$max_arrears_12m))] <- 0
train$woe_max_arrears_12m[which(train$max_arrears_12m <= 0)] <- 0.7027
train$woe_max_arrears_12m[which(train$max_arrears_12m > 0 & train$max_arrears_12m <= 1)] <- -0.8291
train$woe_max_arrears_12m[which(train$max_arrears_12m > 1 & train$max_arrears_12m <= 1.4)] <- -1.1908
train$woe_max_arrears_12m[which(train$max_arrears_12m > 1.4)] <- -2.2223

test$woe_max_arrears_12m<- rep(NA, length(test$max_arrears_12m))
test$woe_max_arrears_12m[which(is.na(test$max_arrears_12m))] <- 0
test$woe_max_arrears_12m[which(test$max_arrears_12m <= 0)] <- 0.7027
test$woe_max_arrears_12m[which(test$max_arrears_12m > 0 & test$max_arrears_12m <= 1)] <- -0.8291
test$woe_max_arrears_12m[which(test$max_arrears_12m > 1 & test$max_arrears_12m <= 1.4)] <- -1.1908
test$woe_max_arrears_12m[which(test$max_arrears_12m > 1.4)] <- -2.2223

#Maximum arrears balance in previous 6 months:
train$woe_max_arrears_bal_6m<- rep(NA, length(train$max_arrears_bal_6m))
train$woe_max_arrears_bal_6m[which(is.na(train$max_arrears_bal_6m))] <- 0
train$woe_max_arrears_bal_6m[which(train$max_arrears_bal_6m <= 0)] <- 0.5771
train$woe_max_arrears_bal_6m[which(train$max_arrears_bal_6m > 0 & train$max_arrears_bal_6m <= 300)] <- -0.7818
train$woe_max_arrears_bal_6m[which(train$max_arrears_bal_6m > 300 & train$max_arrears_bal_6m <= 600)] <- -1.2958
train$woe_max_arrears_bal_6m[which(train$max_arrears_bal_6m > 600 & train$max_arrears_bal_6m <= 900)] <- -1.5753
train$woe_max_arrears_bal_6m[which(train$max_arrears_bal_6m > 900)] <- -2.2110

test$woe_max_arrears_bal_6m<- rep(NA, length(test$max_arrears_bal_6m))
test$woe_max_arrears_bal_6m[which(is.na(test$max_arrears_bal_6m))] <- 0
test$woe_max_arrears_bal_6m[which(test$max_arrears_bal_6m <= 0)] <- 0.5771
test$woe_max_arrears_bal_6m[which(test$max_arrears_bal_6m > 0 & test$max_arrears_bal_6m <= 300)] <- -0.7818
test$woe_max_arrears_bal_6m[which(test$max_arrears_bal_6m > 300 & test$max_arrears_bal_6m <= 600)] <- -1.2958
test$woe_max_arrears_bal_6m[which(test$max_arrears_bal_6m > 600 & test$max_arrears_bal_6m <= 900)] <- -1.5753
test$woe_max_arrears_bal_6m[which(test$max_arrears_bal_6m > 900)] <- -2.2110

#Employment length (years):

train$woe_emp_length<- rep(NA, length(train$emp_length))
train$woe_emp_length[which(is.na(train$emp_length))] <- 0
train$woe_emp_length[which(train$emp_length <= 2)] <- -0.7514
train$woe_emp_length[which(train$emp_length > 2 & train$emp_length <= 4)] <- -0.3695
train$woe_emp_length[which(train$emp_length > 4 & train$emp_length <= 7)] <-  0.1783
train$woe_emp_length[which(train$emp_length > 7)] <- 0.5827

test$woe_emp_length<- rep(NA, length(test$emp_length))
test$woe_emp_length[which(is.na(test$emp_length))] <- 0
test$woe_emp_length[which(test$emp_length <= 2)] <- -0.7514
test$woe_emp_length[which(test$emp_length > 2 & test$emp_length <= 4)] <- -0.3695
test$woe_emp_length[which(test$emp_length > 4 & test$emp_length <= 7)] <-  0.1783
test$woe_emp_length[which(test$emp_length > 7)] <- 0.5827

#Months since recent CC delinquency:
train$woe_months_since_recent_cc_delinq<- rep(NA, length(train$months_since_recent_cc_delinq))
train$woe_months_since_recent_cc_delinq[which(is.na(train$months_since_recent_cc_delinq))] <- 0
train$woe_months_since_recent_cc_delinq[which(train$months_since_recent_cc_delinq <= 6)] <- -0.4176
train$woe_months_since_recent_cc_delinq[which(train$months_since_recent_cc_delinq > 6 & train$months_since_recent_cc_delinq <= 11)] <- -0.1942
train$woe_months_since_recent_cc_delinq[which(train$months_since_recent_cc_delinq > 11)] <-  1.3166

test$woe_months_since_recent_cc_delinq<- rep(NA, length(test$months_since_recent_cc_delinq))
test$woe_months_since_recent_cc_delinq[which(is.na(test$months_since_recent_cc_delinq))] <- 0
test$woe_months_since_recent_cc_delinq[which(test$months_since_recent_cc_delinq <= 6)] <- -0.4176
test$woe_months_since_recent_cc_delinq[which(test$months_since_recent_cc_delinq > 6 & test$months_since_recent_cc_delinq <= 11)] <- -0.1942
test$woe_months_since_recent_cc_delinq[which(test$months_since_recent_cc_delinq > 11)] <-  1.3166

#Annual income:

train$woe_annual_income<- rep(NA, length(train$annual_income))
train$woe_annual_income[which(is.na(train$annual_income))] <- 0
train$woe_annual_income[which(train$annual_income <= 35064)] <- -1.8243
train$woe_annual_income[which(train$annual_income > 35064 & train$annual_income <= 41999)] <- -0.8272
train$woe_annual_income[which(train$annual_income > 41999 & train$annual_income <= 50111)] <- -0.3294
train$woe_annual_income[which(train$annual_income > 50111 & train$annual_income <= 65050)] <-  0.2379
train$woe_annual_income[which(train$annual_income > 65050)] <-  0.6234

test$woe_annual_income<- rep(NA, length(test$annual_income))
test$woe_annual_income[which(is.na(test$annual_income))] <- 0
test$woe_annual_income[which(test$annual_income <= 35064)] <- -1.8243
test$woe_annual_income[which(test$annual_income > 35064 & test$annual_income <= 41999)] <- -0.8272
test$woe_annual_income[which(test$annual_income > 41999 & test$annual_income <= 50111)] <- -0.3294
test$woe_annual_income[which(test$annual_income > 50111 & test$annual_income <= 65050)] <-  0.2379
test$woe_annual_income[which(test$annual_income > 65050)] <-  0.6234

# 3. Multivariate analysis
# Compute Spearman rank correlation based on variables’ WOE
# based on Table 2.2 binning scheme

woe_vars<- train %>%
  dplyr::select(starts_with("woe"))
woe_corr<- cor(as.matrix(woe_vars), method = 'spearman')
# Graphical inspection
library(corrplot)
corrplot(woe_corr, method = 'number')


# 4. Step wise regression
# 4.1 Discard highly correlated variable

woe_vars_clean <- woe_vars %>%
  dplyr::select( -woe_max_arrears_bal_6m)

#Support functions and databases
library(MASS)
attach(train)
# 4.2 Step wise model fitting

logit_full <- glm(default_event ~ woe_bureau_score + 
                    woe_annual_income + woe_emp_length + 
                    woe_max_arrears_12m + woe_months_since_recent_cc_delinq +
                    woe_num_ccj + woe_cc_util, family = binomial(link = 'logit'),data  = train)

logit_stepwise <- stepAIC(logit_full, k = qchisq(0.05,1, lower.tail =  F), direction =  'both')
detach(train)

summary(logit_stepwise)


#  5. Model calibration
#  5.1. Define a scaling function

scaled_score <- function(logit, odds, offset = 500, pdo = 20)
{
  b = pdo/log(2)
  a = offset - b*log(odds)
  round(a + b*log((1-logit)/logit))
}

# 2. Score the entire dataset
# 2.1 Use fitted model to score both test and train datasets
predict_logit_test <- predict(logit_stepwise, newdata = test, type = 'response')
predict_logit_train <- predict(logit_stepwise, newdata = train, type ='response')

# 2.2 Merge predictions with train/test data
test$predict_logit <- predict(logit_stepwise, newdata = test, type = 'response')
train$predict_logit <- predict(logit_stepwise, newdata = train, type = 'response')

train$sample = 'train'
test$sample = 'test'


data_whole <- rbind(train, test)
data_score <- data_whole %>%
  dplyr::select(id, default_event, default_flag, woe_bureau_score,
                woe_annual_income, woe_max_arrears_12m,
                woe_months_since_recent_cc_delinq,
                woe_cc_util, sample, predict_logit)

# 2.3 Define scoring parameters in line with objectives
data_score$score<-
  scaled_score(data_score$predict_logit, 72, 660, 40)


#Distribution of scorecard
hist(data_score$score)

# We suppose that our most recent historical data of scorecard
# Is data_score$score
# Then we calibrate our model by logic regression

# 1. Upload data
attach(data_score)
# 2. Fit logistic regression
pd_model<- glm(default_event~ score,
               family = binomial(link = 'logit'), data = data_score)

summary(pd_model)

# 2.1 Use model coefficients to obtain PDs
data_score$pd<- predict(pd_model, newdata = data_score,
                        type = 'response')


l# 2. ROC curve
library(pROC)
plot(roc(train$default_event,train$predict_logit,
         direction="<"),
     col="blue", lwd=3, main="ROC Curve")

# 1. Create a validation database
# 1.1. Create score bands
library(smbinning)
score_cust<- smbinning.custom(data_score, y = 'default_flag',
                              x='score', cuts= c(517,576,605,632,667,716,746,773))
# 1.2. Group by bands
data_score<- smbinning.gen(data_score, score_cust,
                           chrname = 'score_band')

# 2. Compare actual against fitted PDs
# 2.1. Compute mean values
data_pd<- data_score %>%
  dplyr::select(score, score_band, pd, default_event) %>%
  dplyr::group_by(score_band) %>%
  dplyr::summarise(mean_dr = round(mean(default_event),4),
                   mean_pd = round(mean(pd),4))
# 2.2. Compute rmse
rmse<-sqrt(mean((data_pd$mean_dr - data_pd$mean_pd)^2))

data_pd_long <- data_pd %>%
  tidyr::pivot_longer(cols = c(mean_dr, mean_pd), names_to = "type", values_to = "value")

# Create the line plot
ggplot(data_pd_long, aes(x = score_band, y = value, color = type, group = type)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Actual and Fitted PDs per Score Band",
       x = "Score Band",
       y = "Probability of Default",
       color = "Type") +
  theme_minimal()




