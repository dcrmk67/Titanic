
#################################################################
# Assign libraries
#################################################################

if (!require('tidyverse'))  install.packages('tidyverse')
if (!require('visdat'))     install.packages('visdat')
if (!require('inspectdf'))  install.packages('inspectdf')
if (!require('dlookr'))     install.packages('dlookr')
if (!require('caret'))      install.packages('caret')
if (!require('glmnet'))     install.packages('glmnet')
if (!require('lime'))       install.packages('lime')
if (!require('PerformanceAnalytics')) install.packages('PerformanceAnalytics')


#################################################################
# Read data & initial data cleanup
#################################################################

# Read csv files and rename to lower case for coding convenience
data <-  read_csv('./train.csv') %>%
  rename_all(tolower)
validation <-  read_csv('./test.csv') %>%
  rename_all(tolower)

# Print names to make sure they are lower case
names(data)
names(validation)

glimpse(data)

#################################################################
# Overview of the data set
#################################################################

# Graph of data classes
vis_dat(data)

# Graph of missing data
vis_miss(data)

# Visualizing numeric variables (inspectdf package)
inspect_num(data) %>% show_plot()

# Visualizing character variables (inspectdf package)
inspect_cat(data) %>% show_plot()

# visualize outliers
plot_outlier(data)

# visualize correlations
data %>% keep(is.numeric) %>%
  chart.Correlation(.)

#################################################################
# Data preprocessing 
#################################################################

data <- data %>%
  mutate(survived = as.factor(survived)) %>%
  mutate(# convert several columns to factors :
         age = round(imputate_na(.,age, method='mean'),digits=0),
         pclass = as.factor(pclass),
         sex = as.factor(str_to_title(sex)),
         embarked = as.factor(ifelse(is.na(embarked)==TRUE,'N',embarked)),
         # create new variables:
         family = fct_lump(as.factor(sibsp + parch), prop=0.05),
         cabin_level = fct_lump(as.factor(ifelse(is.na(cabin)==TRUE,'S',substr(cabin,1,1))), prop=0.01),
         cabin_count = ifelse(is.na(cabin)==TRUE,0,(str_count(cabin, '\\s') + 1)),
         # recalculate are based on number of cabins purchased
         fare = ifelse(cabin_count > 1, fare/cabin_count,fare),
         fare = log(fare+1)) %>%
    group_by(pclass,sex) %>% 
  mutate(age=ifelse(is.na(age),mean(age,na.rm=TRUE),age),
         fare=ifelse(is.na(fare),mean(fare,na.rm=TRUE),fare),
          #recode some factor variables
         survived = fct_recode(survived,'Survived'='1','Perished'='0'),
         embarked = fct_recode(embarked,'Cherbourg'='C','Queenstown'='Q','Southampton'='S','N/A'='N'),
         pclass = fct_recode(pclass,'1st Class'='1','2nd Class'='2','3rd Class'='3'),
         cabin_level = fct_recode(cabin_level,'Steerage'='S')) %>%
  select(-cabin, -passengerid, -name, -ticket, -cabin_count, -sibsp, -parch)


validation <- validation %>%
  mutate(# convert several columns to factors :
    age = round(imputate_na(.,age, method='mean'),digits=0),
    pclass = as.factor(pclass),
    sex = as.factor(str_to_title(sex)),
    embarked = as.factor(ifelse(is.na(embarked)==TRUE,'N',embarked)),
    # create new variables:
    family = fct_lump(as.factor(sibsp + parch), prop=0.05),
    cabin_level = fct_lump(as.factor(ifelse(is.na(cabin)==TRUE,'S',substr(cabin,1,1))), prop=0.01),
    cabin_count = ifelse(is.na(cabin)==TRUE,0,(str_count(cabin, '\\s') + 1)),
    # recalculate are based on number of cabins purchased
    fare = ifelse(cabin_count > 1, fare/cabin_count,fare),
    fare = log(fare+1)) %>%
  group_by(pclass,sex) %>% 
  mutate(age=ifelse(is.na(age),mean(age,na.rm=TRUE),age),
         fare=ifelse(is.na(fare),mean(fare,na.rm=TRUE),fare),
         #recode some factor variables
         embarked = fct_recode(embarked,'Cherbourg'='C','Queenstown'='Q','Southampton'='S','N/A'='N'),
         pclass = fct_recode(pclass,'1st Class'='1','2nd Class'='2','3rd Class'='3'),
         cabin_level = fct_recode(cabin_level,'Steerage'='S')) %>%
  select(-cabin, -passengerid, -name, -ticket, -cabin_count, -sibsp, -parch)


#################################################################
# Overview of the data set after preprocessing
#################################################################

glimpse(data)

# Graph of data classes
vis_dat(data)

# Visualizing numeric variables (inspectdf package)
inspect_num(data) %>% show_plot()

# Visualizing character variables (inspectdf package)
inspect_cat(data) %>% show_plot()

# visualize outliers
plot_outlier(data)

# visualize correlations
data %>% keep(is.numeric) %>%
  chart.Correlation(.)

  
#################################################################
# Data Sampling
#################################################################

set.seed(2021)

trainIndex <- createDataPartition(data$survived, p=0.8, list=FALSE)
train_set <- data[trainIndex,]
test_set <- data[-trainIndex,]


#################################################################
# Exploratory Data Analysis (EDA) of 'train_set'
#################################################################

# Survived frequency distribution
train_set %>%
  rename(y = survived) %>%
  group_by(y) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n) * 100) %>%
  ggplot(aes(x=y, y=freq, fill=y)) +
  geom_bar(stat='identity') + 
  geom_text(aes(label=round(freq,digits=2)), nudge_y = -2) +
  geom_text(aes(label=round(n,digits=0)), nudge_y = -6) +
  geom_hline(yintercept=0, color='black') +
  labs(title = "Survivial Rate of Titanic Passengers",
       x = NULL,
       y = "Survival Rate (%)") +
  theme_bw() +
  theme(legend.position = 'none',
        panel.grid = element_blank())

# pclass plot
train_set %>%
  filter(survived=="Survived") %>%
  rename(y = pclass) %>%
  group_by(y) %>%
  summarise (n = n()) %>%
  mutate(freq = (n / sum(n) * 100)) %>%
  ggplot(aes(x=y, y=freq, fill=y)) +
  geom_bar(stat='identity') + 
  geom_text(aes(label=round(freq,digits=2)), nudge_y = -2) +
  geom_text(aes(label=round(n,digits=0)), nudge_y = -6) +
  geom_hline(yintercept=0, color='black') +
  labs(title = "Survivial Rate of Titanic Passengers by Ticket Class",
       x = NULL,
       y = "Survival Rate (%)") +
  theme_bw() +
  theme(legend.position = 'none',
        panel.grid = element_blank())

# gender plot
train_set %>%
  filter(survived=="Survived") %>%
  rename(y = sex) %>%
  group_by(y) %>%
  summarise (n = n()) %>%
  mutate(freq = (n / sum(n) * 100)) %>%
  ggplot(aes(x=y, y=freq, fill=y)) +
  geom_bar(stat='identity') + 
  geom_text(aes(label=round(freq,digits=2)), nudge_y = -2) +
  geom_text(aes(label=round(n,digits=0)), nudge_y = -6) +
  geom_hline(yintercept=0, color='black') +
  labs(title = "Survivial Rate of Titanic Passengers by Gender",
       x = NULL,
       y = "Survival Rate (%)") +
  theme_bw() +
  theme(legend.position = 'none',
        panel.grid = element_blank())

# embarkment plot
train_set %>%
  filter(survived=="Survived") %>%
  rename(y = embarked) %>%
  group_by(y) %>%
  summarise (n = n()) %>%
  mutate(freq = (n / sum(n) * 100)) %>%
  ggplot(aes(x=y, y=freq, fill=y)) +
  geom_bar(stat='identity') + 
  geom_text(aes(label=round(freq,digits=2)), nudge_y = 6) +
  geom_text(aes(label=round(n,digits=0)), nudge_y = 2) +
  geom_hline(yintercept=0, color='black') +
  labs(title = "Survivial Rate of Titanic Passengers by Embarkment Port",
       x = NULL,
       y = "Survival Rate (%)") +
  theme_bw() +
  theme(legend.position = 'none',
        panel.grid = element_blank())

# cabin level
train_set %>%
  filter(survived=="Survived") %>%
  rename(y = cabin_level) %>%
  group_by(y) %>%
  summarise (n = n()) %>%
  mutate(freq = (n / sum(n) * 100)) %>%
  ggplot(aes(x=y, y=freq, fill=y)) +
  geom_bar(stat='identity') + 
  geom_text(aes(label=round(freq,digits=2)), nudge_y = 6) +
  geom_text(aes(label=round(n,digits=0)), nudge_y = 2) +
  geom_hline(yintercept=0, color='black') +
  labs(title = "Survivial Rate of Titanic Passengers by Cabin",
       x = NULL,
       y = "Survival Rate (%)") +
  theme_bw() +
  theme(legend.position = 'none',
        panel.grid = element_blank())

# family level
train_set %>%
  filter(survived=="Survived") %>%
  rename(y = family) %>%
  group_by(y) %>%
  summarise (n = n()) %>%
  mutate(freq = (n / sum(n) * 100)) %>%
  ggplot(aes(x=y, y=freq, fill=y)) +
  geom_bar(stat='identity') + 
  geom_text(aes(label=round(freq,digits=2)), nudge_y = 5) +
  geom_text(aes(label=round(n,digits=0)), nudge_y = 2) +
  geom_hline(yintercept=0, color='black') +
  labs(title = "Survivial Rate of Titanic Passengers by Number of Family Members Aboard",
       x = NULL,
       y = "Survival Rate (%)") +
  theme_bw() +
  theme(legend.position = 'none',
        panel.grid = element_blank())


# Age distribution of survivors
train_set %>%
  filter(survived=='Survived') %>%
  mutate(age = cut(age, breaks=c(0, 10, 20, 30, 40, 50, 60, 71, Inf))) %>%
  group_by(age) %>%
  summarise (n = n()) %>%
  mutate(freq = (n / sum(n) * 100)) %>%
  ggplot(aes(x=age, y=freq, fill=age)) +
  geom_bar(stat='identity') + 
  geom_text(aes(label=round(freq,digits=2)), nudge_y = 5) +
  geom_text(aes(label=round(n,digits=0)), nudge_y = 2) +
  geom_hline(yintercept=0, color='black') +
  labs(title = "Survivial Rate of Female Titanic Passengers by Age",
       x = 'Age Grouping',
       y = "Survival Rate (%)") +
  theme_bw() +
  theme(legend.position = 'none',
        panel.grid = element_blank())

train_set %>%
  filter(survived=='Survived' & sex=='Female') %>%
  mutate(age = cut(age, breaks=c(0, 10, 20, 30, 40, 50, 60, 71, Inf))) %>%
  group_by(age) %>%
  summarise (n = n()) %>%
  mutate(freq = (n / sum(n) * 100)) %>%
  ggplot(aes(x=age, y=freq, fill=age)) +
  geom_bar(stat='identity') + 
  geom_text(aes(label=round(freq,digits=2)), nudge_y = 5) +
  geom_text(aes(label=round(n,digits=0)), nudge_y = 2) +
  geom_hline(yintercept=0, color='black') +
  labs(title = "Survivial Rate of Female Titanic Passengers by Age",
       x = 'Age Grouping',
       y = "Survival Rate (%)") +
  theme_bw() +
  theme(legend.position = 'none',
        panel.grid = element_blank())

train_set %>%
  filter(survived=='Perished' & sex=='Male') %>%
  mutate(age = cut(age, breaks=c(0, 10, 20, 30, 40, 50, 60, 71, Inf))) %>%
  group_by(age) %>%
  summarise (n = n()) %>%
  mutate(freq = (n / sum(n) * 100)) %>%
  ggplot(aes(x=age, y=freq, fill=age)) +
  geom_bar(stat='identity') + 
  geom_text(aes(label=round(freq,digits=2)), nudge_y = 5) +
  geom_text(aes(label=round(n,digits=0)), nudge_y = 2) +
  geom_hline(yintercept=0, color='black') +
  labs(title = "Survivial Rate of Male Titanic Passengers by Age",
       x = 'Age Grouping',
       y = "Survival Rate (%)") +
  theme_bw() +
  theme(legend.position = 'none',
        panel.grid = element_blank())



# Fares chart
train_set %>%
    rename(y = fare) %>%
    group_by(survived) %>%
    summarize(mean = mean(y)) %>%
    ggplot(aes(x=survived, y=mean, fill=survived, label=round(mean,digits=2))) +
    geom_bar(stat='identity') + 
    geom_text(nudge_y = -2) +
    geom_hline(yintercept=0, color='black') +
    labs(title = "Log Price of Titanic Fares",
         x = 'Survival',
         y = "Mean of Ticket Price (Log)") +
    theme_bw() +
    theme(legend.position = 'none',
          panel.grid = element_blank())

# Fares chart
train_set %>%
  rename(y = fare) %>%
  mutate(class_survived = paste0(pclass,"-",survived)) %>%
  group_by(class_survived) %>%
  summarize(mean = mean(y)) %>%
  ggplot(aes(x=class_survived, y=mean, fill=class_survived, label=round(mean,digits=2))) +
  geom_bar(stat='identity') + 
  geom_text(nudge_y = -2) +
  geom_hline(yintercept=0, color='black') +
  labs(title = "Log Price of Titanic Fares by Passenger Class",
       x = 'Passenger Class & Survival',
       y = "Mean of Ticket Price (Log)") +
  theme_bw() +
  theme(legend.position = 'none',
        panel.grid = element_blank())


#################################################################
# Model 1: Logistic Regression Model
#################################################################

# Train the glm model
fit <- train(survived ~ ., 
           data=train_set, 
           trControl=trainControl(method='cv',number=5),
           method='glm',
           family='binomial')
# Print model info
fit$results
summary(fit$finalModel)

# Importance variable plot
plot(varImp(fit),main="Variable Importance from Logistic Regression Model")

# Estimate of test_set$survived
y_hat <- predict(fit, test_set)

# Confusion matrix output
confusionMatrix(y_hat,test_set$survived)


#################################################################
# Model 2: Penalized Logistic Regression Model
# Penalized logistic regression imposes a penalty to the logistic 
# model for having too many variables. This results in shrinking 
# the coefficients of the less contributive variables toward zero. 
# This is also known as regularization.
#################################################################

set.seed(2021)

# Make a custom trainControl - use ROC as a model selection criteria
glmnet_control <- trainControl(
  method = "cv", 
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE) # Super important!

model <- train(survived ~ ., 
             data=train_set, 
             method='glmnet',
             trControl=glmnet_control)
model
plot(model)
max(model[["results"]]$ROC)

# use expand grid to create glmnet tuning parameters
glmnet_grid <- expand.grid(
  alpha=seq(0,1,length=.1),
  lambda = seq(0.0001,1, length=20))

fit <- train(survived ~ ., 
           data=train_set, 
           method='glmnet',
           trContro=glmnet_control,
           tuneGrid=glmnet_grid)

# Print model info
summary(fit$finalModel)

# Print model coefficients
# Since model estimates are a non-linear, non-differentiable function of the response 
# values, it is difficult to obtain an accurate estimate of their standard errors.
coef(fit$finalModel, fit$finalModel$lambdaOpt)


# Importance variable plot
plot(varImp(fit),main="Variable Importance from Penalized Logistic Regression Model")

# Estimate of test_set$survived
y_hat <- predict(fit, test_set)

# Confusion matrix output
confusionMatrix(y_hat,test_set$survived)

# Using Lime package
explainer <- lime(train_set,fit,bin_continuous = FALSE, quantiles_bins=FALSE)
explanation <- explain(test_set, explainer, n_labels=1, n_features=1)

plot_features(explanation, ncol=2)
