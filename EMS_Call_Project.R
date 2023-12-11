library(ggplot2)
library(dplyr)
#Submit whatever I have Sunday 7pm

#Data loaded by "training_data_load.r" in EMS data folder
#Load_weather_data loads weather data and then merges it with EMS


dim(EMS)
# 11,863,759       34

#Incident_travel_tm
merged_data$incident_travel_tm_seconds_qy
summary(merged_data$incident_travel_tm_seconds_qy)

#The outliers are so big compared to the rest of the data that you can't even see anything
ggplot(merged_data, mapping = aes(x = incident_travel_tm_seconds_qy)) + 
  geom_histogram(binwidth = 60)
#There are many outliers
boxplot(EMS_Brooklyn$incident_travel_tm_seconds_qy)


##### REMOVE NA's AND 0S from TRAVEL TIME #####
#There are also many NA's, we would proably want to remove these rows
length(which((merged_data$incident_travel_tm_seconds_qy == 0.0)))
#36660 rows with 0
removed <- merged_data |> filter(
                            !is.na(incident_travel_tm_seconds_qy) 
                            & (incident_travel_tm_seconds_qy != 0))
summary(removed$incident_travel_tm_seconds_qy)
str(removed$incident_travel_tm_seconds_qy)

   
##### ADDRESS OUTLIERS IN TRAVEL TIME #####
length(which((removed$incident_travel_tm_seconds_qy == 38604)))
max(removed$incident_travel_tm_seconds_qy)

#find row with outlier and determine what it means
outlier <- removed |> filter(incident_travel_tm_seconds_qy == 38604)
rm(outlier)
#When I look at the row, I see that there is no value for response time
#Because the "valid_incident_reposnse_time_indicator" is No meaning this time is not valid

#Lets look at how many rows have this indicator as "N"

#dispatch is from incident creation to first assignment
#travel time is from first assignment to first on scene  --> this is the one we care about
#It doesn't matter to us if dispach indicator is not valid because all of that happened before
#travel time, and a N for response could be that travel was fine but the previous was not
#So we only care about the instances where travel time is incorrect. So dispach is Y and
#Response is N
#If we do both as N then rsponse could be due to dispach being N... and that doesn't tell us about 
#Travel time
#It could also be that both at Y so that is fine to use
#D = Y, R = N  --> travel time is wrong
#D = Y, R = Y  --> everything is correct
#D = N, R = N  --> dispach is wrong, travel time may be correct
#D = N, R = Y  --> Not possible bc dispach is a factor of reponse
#We only want valid, BOTH ARE Y
length(which((removed$valid_dispatch_rspns_time_indc == "N")))
not_valid_dispatch <- removed |> filter(valid_dispatch_rspns_time_indc == "N")
summary(not_valid_dispatch)

length(which((removed$valid_incident_rspns_time_indc == "N")))
not_valid_response_time <- removed |> filter(valid_incident_rspns_time_indc == "N")
valid_response_time <- removed |> filter(valid_incident_rspns_time_indc == "Y")
summary(not_valid_response_time)

#All rows that have valid indicators for both dispach and response times
valid_times <- removed |> filter((removed$valid_dispatch_rspns_time_indc == "Y") & (valid_incident_rspns_time_indc == "Y"))

summary(valid_times$incident_travel_tm_seconds_qy)

#Now looking at the boxplot again, we see there are a lot of outliers still
boxplot(valid_times$incident_travel_tm_seconds_qy)
stem(valid_times$incident_travel_tm_seconds_qy)
#^from this plot using just the default settingswe find that most of the data is 
#in the 0 bin (3280951) while total is 3298596. difference is 17645. 

stem(valid_times$incident_travel_tm_seconds_qy, scale = 2)
#drop off is at 7000 for scale = 2
stem(valid_times$incident_travel_tm_seconds_qy, scale = 3)
#Not different from previous
stem(valid_times$incident_travel_tm_seconds_qy, scale = 4)  #3 digits from |
#I'd say outliers can be from 7000 up
stem(valid_times$incident_travel_tm_seconds_qy, scale = 10) #2 digits from |
#The first bin without a number is 101

outliers <- valid_times |> filter(incident_travel_tm_seconds_qy >= 7000)
summary(outliers)

without_outliers <- valid_times |> filter(incident_travel_tm_seconds_qy < 7000)
summary(without_outliers)
boxplot(without_outliers$incident_travel_tm_seconds_qy)
#Okay so there still a ton of outliers
#first I want to look at the histogram 
ggplot(without_outliers, mapping = aes(x = incident_travel_tm_seconds_qy)) + 
  geom_histogram(binwidth = 100)
#It is def looking a bit clearer but we there is a lot of 

## Lets remove some more outliers, split at 4000 seconds
outliers <- valid_times |> filter(incident_travel_tm_seconds_qy >= 4000)
without_outliers <- valid_times |> filter(incident_travel_tm_seconds_qy < 4000)
summary(without_outliers$incident_travel_tm_seconds_qy)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.0   273.0   386.0   450.7   542.0  3999.0 
boxplot(without_outliers$incident_travel_tm_seconds_qy)
#So still not enough
ggplot(without_outliers, mapping = aes(x = incident_travel_tm_seconds_qy)) + 
  geom_histogram(binwidth = 100)



## Lets remove some more outliers, split at 2000 seconds
outliers <- valid_times |> filter(incident_travel_tm_seconds_qy >= 2000)
without_outliers <- valid_times |> filter(incident_travel_tm_seconds_qy < 2000)
summary(without_outliers$incident_travel_tm_seconds_qy)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.0   272.0   384.0   441.4   539.0  1999.0 
boxplot(without_outliers$incident_travel_tm_seconds_qy)
#So still not enough
ggplot(without_outliers, mapping = aes(x = incident_travel_tm_seconds_qy)) + 
  geom_histogram(binwidth = 100)


## Lets remove some more outliers, split at 1000 seconds
outliers <- valid_times |> filter(incident_travel_tm_seconds_qy >= 1000)
without_outliers <- valid_times |> filter(incident_travel_tm_seconds_qy < 1000)
summary(without_outliers$incident_travel_tm_seconds_qy)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.0   268.0   375.0   405.2   513.0   999.0 
boxplot(without_outliers$incident_travel_tm_seconds_qy)
#okay almost there
ggplot(without_outliers, mapping = aes(x = incident_travel_tm_seconds_qy)) + 
  geom_histogram(binwidth = 50)
stem(without_outliers$incident_travel_tm_seconds_qy)


## Lets remove some more outliers, split at 849 seconds
outliers <- valid_times |> filter(incident_travel_tm_seconds_qy >= 849)
without_outliers <- valid_times |> filter(incident_travel_tm_seconds_qy < 849)
summary(without_outliers$incident_travel_tm_seconds_qy)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.0   264.0   368.0   389.9   498.0   848.0  
boxplot(without_outliers$incident_travel_tm_seconds_qy)
#Gets rid of all the outliers
ggplot(without_outliers, mapping = aes(x = incident_travel_tm_seconds_qy)) + 
  geom_histogram(binwidth = 1)
#There is a peak close to 0

#this peak close to 0 is an anomoly in that it represents people that 
#were reached in a few seconds. This cannot be gerneralized so I will remove them
#Anamoly towards 600. Cannot really be explained so I will remove that as well.
#A reasonable time is 3.33 minutes
#So cut from 200 seconds to 600 seconds (3.33 minutes - 10 minutes)

#outliers <- valid_times |> filter(incident_travel_tm_seconds_qy >= 849)
without_outliers <- valid_times |> filter((incident_travel_tm_seconds_qy >= 100) & (incident_travel_tm_seconds_qy <= 600))
summary(without_outliers$incident_travel_tm_seconds_qy)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 100.0   257.0   345.0   350.8   443.0   600.0  
boxplot(without_outliers$incident_travel_tm_seconds_qy)
#Gets rid of all the outliers
ggplot(without_outliers, mapping = aes(x = incident_travel_tm_seconds_qy)) + 
  geom_histogram(binwidth = 1)




##### DISTRIBUTION FOR TRAVEL TIME #####
######If I get the chance I need to do distribution/EDA on predictor variables. maybe,..
#Looking at the plot make by 
ggplot(without_outliers, mapping = aes(x = incident_travel_tm_seconds_qy)) + 
 geom_histogram(binwidth = 1)
#Tukey-lambda distribution
#   - deons't start at 0
# The characteristics I see is there is a right skew with a central peak
#both sides however are concave down

#Check normality with KS
normalality <- ks.test(without_outliers$incident_travel_tm_seconds_qy, "pnorm")
#p-value < 2.2e-16 ===> likely not normally distributed


#### CLEANING PREDICTORS ####
#What did I do here: I took out only the weather measurments that would be relevent
#TSUN and TAVG were columns but they had no values, FMTM and PGTM had too many missing values
#I also didn't think they were relevent. Not really sure what windspeed has to do with anything'
#But anyway it had more then 1 million missing vlaues. I wasn't 
#summary(predictors$WT03)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0       0       0       0       0       0 
#^This variable can be removed
predictors <- without_outliers |> select(AWND | PRCP:SNWD | TMAX | TMIN | WDF2:WT02 | WT04:WT22)
#predictors1 <- without_outliers |> select(AWND:SNWD | TMAX | TMIN | WDF2:WT22)
summary(predictors)
#I can just remove them the NA's from these 5 variables
predictors <- predictors |> filter(!is.na(AWND) & 
                                         !is.na(WDF2)&
                                         !is.na(WDF5) &
                                         !is.na(WSF2) &
                                         !is.na(WSF5))

#### TRAINING AND TESTING SETS ####
regression_data <- without_outliers |> 
  select(incident_travel_tm_seconds_qy | AWND | PRCP:SNWD | TMAX | TMIN | WDF2:WT02 | WT04:WT22) |> 
  filter(!is.na(AWND) & !is.na(WDF2)& !is.na(WDF5) & !is.na(WSF2) & !is.na(WSF5))

n <- dim(regression_data)[1]
training <- sample(1:n, n*0.9, replace = FALSE)
testing <- setdiff(1:n, training)

train_set <- subset(regression_data[training,])
train_y <- train_set[1]
train_x <- train_set[-1]
test_y <- subset(regression_data[testing,])[1]
test_x <- subset(regression_data[testing,])[-1]

#### PRINCIPLE COMPONENT ANALYSIS OF PREDICTORS ####
principle_component <- princomp(predictors, scale = TRUE, cor = TRUE, score = TRUE)
apply(predictors, 2, var)
sapply(predictors, mean)
summary(principle_component)
#Looking at this the first comp only gives 15% and the first 2 give 28%
#The variables are however not standarized. Most of them are but not all of them
#I will do that and then try it again
scaled_predictors <- scale(predictors)
principle_component <- princomp(scaled_predictors, cor = TRUE, score = TRUE)
summary(principle_component)
#Importance of components:
#                         Comp.1    Comp.2     Comp.3     Comp.4     Comp.5
# Standard deviation     2.0178839 1.8267033 1.49272137 1.29871079 1.22843185
# Proportion of Variance 0.1566098 0.1283402 0.08570066 0.06487114 0.05804019
# Cumulative Proportion  0.1566098 0.2849500 0.37065068 0.43552182 0.49356200
#                         Comp.6     Comp.7     Comp.8     Comp.9    Comp.10
# Standard deviation     1.12797770 1.06714047 1.03801282 1.01524866 0.99950474
# Proportion of Variance 0.04893591 0.04379957 0.04144118 0.03964346 0.03842345
# Cumulative Proportion  0.54249792 0.58629748 0.62773866 0.66738212 0.70580557
#                         Comp.11    Comp.12   Comp.13    Comp.14    Comp.15
# Standard deviation     0.99132294 0.91872338 0.8934044 0.85305189 0.79064994
# Proportion of Variance 0.03779697 0.03246356 0.0306989 0.02798837 0.02404336
# Cumulative Proportion  0.74360254 0.77606610 0.8067650 0.83475337 0.85879672
#                         Comp.16    Comp.17    Comp.18    Comp.19    Comp.20
# Standard deviation     0.75798482 0.73463787 0.72152685 0.69049127 0.66843006
# Proportion of Variance 0.02209773 0.02075742 0.02002312 0.01833762 0.01718457
# Cumulative Proportion  0.88089446 0.90165187 0.92167499 0.94001261 0.95719718
#                         Comp.21     Comp.22     Comp.23     Comp.24
# Standard deviation     0.60926969 0.490489530 0.452605596 0.439902572
# Proportion of Variance 0.01427729 0.009253076 0.007878916 0.007442857
# Cumulative Proportion  0.97147447 0.980727542 0.988606459 0.996049315
#                         Comp.25     Comp.26
# Standard deviation     0.259880610 0.187562967
# Proportion of Variance 0.002597613 0.001353072
# Cumulative Proportion  0.998646928 1.000000000
#As we can see here, the first few components do not capture that much of the data
#So this model would not work well in prediction. Should I use it anyway???
#Lets try a principle component regression anyway
library(pls)
help("pcr")
set.seed(1)
pcr_fit <- pcr(incident_travel_tm_seconds_qy~ ., data = regression_data, subset = training, scale = TRUE, validation = "CV")
summary(pcr_fit)
#Looking the cross-validation error, we see that the smallest error occurs at component 7, which accounts for 58.63% of variance
validationplot(pcr_fit, val.type = "MSEP")

pcr_prediction <- predict(pcr_fit, test_x, ncomp = 7)
head(pcr_prediction)
summary(pcr_prediction)
pcr_error <- mean(((pcr_prediction - test_y)^2)[,1])
#MSE: 14713.62



#### MULTIVARIATE REGRESSION MODEL ####
library(boot)
set.seed(1)

glm_fit <- glm(incident_travel_tm_seconds_qy~ .,data = train_set)
summary(glm_fit)
anova(glm_fit)
#Calculate R squared
with(summary(glm_fit), 1 - deviance/null.deviance)
# R-squared =  0.001466146   --> not a good fit at all
#In practice, values over 0.40 indicate that a model fits the data very well.
cv_error_2 <- cv.glm(train_set, glm_fit, K = 10)
cv_error_2$delta
#[1] 14687.64 14687.63   ---> As we can see very high error
#Actually it is lower then PCR
#Cost function is average squared error function
multivar_reg_prediction <- predict(glm_fit, test_x,  interval = "prediction")
multivar_reg_error <- mean(((multivar_reg_prediction - test_y)^2)[,1])
#[1] 14674.72


#### KNN REGRESSION ####
library(class)
library(FNN)
scaled_train_y <- scale(train_y)
scaled_train_x <- scale(train_x)
scaled_test_x <- scale(test_x)
scaled_test_y <- scale(test_y)
knn_fit <- knn.reg(train = scaled_train_x, y = scaled_train_y, k = 5)
#Leave one out cross-validation is performed
summary(knn_fit$R2Pred)
knn_predictions <- knn.reg(train = scaled_train_x, y = scaled_train_y, test = scaled_test_x, k = 5)
#Calculate error
knn_error <- mean(((knn_predictions$pred - scaled_test_y)^2)[,1])
knn_error
#MSE: 1.188324    ---> really nice!!!

knn_error <- rep(0,7)
index <- 0
for(k in c(1, 3, 5, 7, 9, 11, 13)){
  knn_predictions <- knn.reg(train = scaled_train_x, y = scaled_train_y, test = scaled_test_x, k)
  knn_error[index] <- mean(((knn_predictions$pred - scaled_test_y)^2)[,1])
  index <- index + 1
}
knn_error
#[1] 1.329213 1.188324 1.132795 1.102407 1.083085 1.071245 1.059624




