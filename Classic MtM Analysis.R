library(readxl)
library(psych)
library(QuantPsyc)
library(tidyverse)
library(dplyr)
library(readr)
# Careful with reshape, will mask rename() in dplyr
library(reshape)
X2nd_MtM_analysis <- read_excel("C:/Users/joncd/OneDrive/Desktop/Explorance/Data Analysis/MTM R Implementation/2nd MtM analysis.xlsx")
workingset<-as_tibble(X2nd_MtM_analysis)
colnames(workingset)
workingset$`Question ID`<-as.factor(workingset$`Question ID`)
# This severely changes our our results, should separate first
# workingset$Answer<-as.numeric(as.factor(workingset$Answer))
# trying to separate only numeric answers now
summary(workingset$`Question ID`)
workingset$`Eval Submitted ID`<-(as.factor(workingset$`Eval Submitted ID`))
# ways of dplyr filtering
workingset<- workingset %>%
  filter(`Question ID` %in% c("518230", "518232", "76509", "518233","518234","2788","2741","2740","1423"))
workingset <- workingset %>% 
  mutate(Answer = as.numeric(Answer))
workingset %>% count(`Question ID`)
workingset %>% count(Answer)
colnames(workingset)
checkset<- workingset %>%
  select(`Eval Submitted ID`, `Question ID`, Answer)
# The avg scores of the primary QIDs
qidavg<- workingset%>%
  group_by(`Question ID`)%>%
  summarize(mean_ans = mean(Answer, na.rm=TRUE))
# Are we dealing with pre-test/post-test data, different trainings, etc.
# Anything containing "data" as the object name is me experimenting with ways to transform the data, skip to line 45 for meaninful analysis
# all this code does  exactly what the qidavg does
data<- workingset %>%
  group_by(`Question ID`, `Eval Submitted ID`) %>%
  summarize(Answer_mean = mean(Answer))
data <- data %>% 
  group_by(`Question ID`) %>% 
  summarise(mean = mean(Answer_mean, na.rm=TRUE))
# Potential for transpose or for loop
data2 <- workingset%>%
  group_by(`Question ID`,Answer)%>%
  select(`Question ID`,Answer)
data2<- t(data2)
# checking some values
other<- checkset %>%
  filter(`Eval Submitted ID` %in% "201078732")%>%
  print(other)
# Primary set to work with and build so far
qidset<- checkset%>%
  spread(key = `Question ID`, value = Answer)
# Second code is good for visualizing missing obs
qidset2<- workingset%>%
  spread(`Question ID`, Answer)

#select(c("518230", "518232", "76509", "518233","518234","2788","2741","2740","1423"))
# Following code shows we are still working with a lot of missing values
na<-na.omit(qidset)
describe(qidset)
summary(qidset)
# KDA Generation
data<-qidset
colnames(data)
scrap_learning <- function(data) {
  if('518230' %in% colnames(data))
    SL <- (100-data$`518230`)
  else
    SL <- (100-data$`2788`)
}
performance_imp <- function(data) {
  if('518232' %in% colnames(data))
    API <- (data$`518232`*data$`518233`*data$`518234`*.65)
  else
    API <- (data$`2741`*data$`2740`*data$`1423`*.65)
}
# Testing out if we can get those missing values, which we can
performance_imp <- function(data) {
  if('518232' %in% colnames(data))
    API <- (data$`2741`*data$`2740`*data$`1423`*.65)
}
perf_imp2 <- function(data) {
  if('518232' %in% colnames(data))
  API <- (data$`518232`*data$`518233`*data$`518234`*.65)
}
# The above is a tedious way to program in all the variables
# Need to think about a for loop we could use to get all the variables to work
#calculating Scrap Learning, NPS and API into new columns here
data$SL <- scrap_learning(data)
data$NPS <- data$`76509`
data$API <- performance_imp(data)
data$API <- perf_imp2(data)
# Lets review where some of the data is missing, probably a function of the ifelse function
statset<- data %>%
  select(SL, NPS, API)
# Do these values make sense? Should Scrap be further transformed?
describe(statset)
summary(statset)
na<-na.omit(statset)
#na<-statset[!complete.cases(statset),]
cor(statset, use = "complete.obs")
summary(lm(SL~NPS+API, data = statset))
summary(lm(SL~API, data = statset))
summary(stepAIC(lm(SL~., data = na)))

# Can we think of additional analysis we might be interested in? 
# i.e., by time, subject, etc


        