#Bank marketing 
  
  
  


#we load the library needed for the project.

load_librarys <- function(librarys) {
  sapply(librarys, function(library) {
    
    # Load the package. If it doesn't exists, install and load.
    if(!require(library, character.only = TRUE)) {
      
      # Install the package
      install.packages(library)
      
      # Load the package
      library(library, character.only = TRUE)
    }
  })}


librarys <- c("tidyverse", "readxl", "ggpubr", 
          "ggthemes", "caret","rpart.plot","ggcorrplot")

load_library(libs)
#Then we load the data from the UCI Repository


data <- read_csv2("bank-additional-full.csv")
Validation <- read_csv2("bank-additional.csv")
head(data)


#To get a quick overview over the data for this project, we use the summary() function. 

summary(data)



#Then we want to know where the data set contains Na values. Na values could lead to calculation problems later.
#We can see that in neither the data dataset nor in the Validation dataset are any Na values


colSums(is.na(data))
colSums(is.na(Validation))


#Then we turn the character features into factors for the machine learning methods later. 
#And we create a vector of all potential predictor names called x_cols.


xcols <- c("age","job","marital","education","default","housing","loan","contact","month", "day_of_week" ,"campaign","pdays","previous",       "poutcome","emp.var.rate","cons.price.idx", "cons.conf.idx", "euribor3m","nr.employed" )
data$job = as.factor(data$job)
data$marital = as.factor(data$marital)
data$education = as.factor(data$education)
data$default = as.factor(data$default)
data$housing = as.factor(data$housing)
data$contact = as.factor(data$contact)
data$month = as.factor(data$month)
data$day_of_week = as.factor(data$day_of_week)
data$poutcome = as.factor(data$poutcome)

data$y <- as.factor(data$y)

data$euribor3m <- as.numeric(data$euribor3m)
```

# divide the data set into a training set and a test set.
#70% will be used for training and 30% will be used for testing.


set.seed(2,sample.kind = "Rounding")
test_index <- createDataPartition(y = data$y, times = 1,
                                  p = 0.3, list = FALSE)
train_set <- data[-test_index,]
test_set <- data[test_index,]



#use nearZeroVar function to identify low variation features
nearZeroVar(train_set[,xcols],saveMetrics = TRUE) %>% select(nzv)
train_set <- train_set %>% select(-pdays)
xcols <- c("age","job","marital","education","default","housing","loan","contact","month", "day_of_week" ,"campaign","previous","poutcome","emp.var.rate","cons.price.idx", "cons.conf.idx", "euribor3m","nr.employed" )



## Variable importance 





# importance calculated by the filterVarImp function for each predictor. 



varimp <- data.frame( Overall = as.data.frame(filterVarImp(train_set[,xcols],as.double(train_set$y)))$Overall)
ev
varimp %>% mutate(xcols=reorder(xcols,(Overall)),color = ifelse(Overall>5,"important","not important"))%>% ggplot(aes(Overall,xcols,fill=color)) + geom_bar(stat = 'identity')
```



## Prevalence



y_plot<- train_set %>% 
  group_by(y) %>% 
	summarise(count = n()) %>% 
  mutate(y=reorder(y,(-count)))%>%
	ggplot(aes(x =count , y =y)) + 
	geom_bar(stat = 'identity')+ggtitle("y")+
  theme(plot.title = element_text(size=10))
y_plot





### Conumer price index plot


cons.price.idx_plot<- train_set %>% ggplot(aes(x=cons.price.idx))+geom_histogram()+ggtitle("cons.price.idx")+theme(plot.title = element_text(size=10))
cons.price.idx_plot


The plot bellow is a Boxplot of the consumer price index that shows the difference between the consumer price index of successful calls and unsuccessful calls. It looks like there is no big difference between calls that were answered with no or with yes. But calls answered with "yes" still tend to have a lower con.price.idx.


cons.price.idx_rate_plot<- train_set %>% filter(cons.price.idx>1000) %>% ggplot(aes(x=cons.price.idx,y=y,fill=y)) +
  geom_boxplot()+coord_flip()+ggtitle("cons.price.idx")
cons.price.idx_rate_plot



### Consumer Confidence Index Plot

cons.conf.idx_plot<- train_set %>% ggplot(aes(x=cons.conf.idx))+geom_histogram()+ggtitle("cons.conf.idx")+theme(plot.title = element_text(size=10))
cons.conf.idx_plot


cons.conf.idx_rate_plot<- data %>% ggplot(aes(x=cons.conf.idx,y=y,fill=y)) +
  geom_boxplot()+ggtitle("cons.conf.idx")+coord_flip()
cons.conf.idx_rate_plot



### Duration plot


duration_plot <- train_set %>% ggplot(aes(x=duration))+geom_histogram()+ggtitle("duration")+theme(plot.title = element_text(size=10))
duration_plot


duration_rate_plot<-train_set  %>% ggplot(aes(x=duration,y=y,fill=y)) +
  geom_boxplot()+coord_flip()
duration_rate_plot



### Euribor3m plot 




euribor3m_plot <- train_set %>%  ggplot(aes(x=euribor3m))+geom_histogram()+ggtitle("euribor3m") + theme(plot.title = element_text(size=10))
euribor3m_plot

train_set  %>% ggplot(aes(y=euribor3m,x=y,fill=y)) +
  geom_boxplot()



### Age Plot


age_plot <- train_set %>% ggplot(aes(x=age))+geom_histogram()+ggtitle("Age") + theme(plot.title = element_text(size=10))
age_plot

train_set$y <- ifelse(train_set$y=="yes",1,0)
test_set$y <- ifelse(test_set$y=="yes",1,0)
age_rate_plot<- train_set %>%   group_by(age) %>% summarise(mean=mean(y))  %>% ggplot(aes(x=age,y=mean),size=10)+geom_point() +geom_smooth(span=0.9)+ ggtitle("Age")
age_rate_plot




### Campaign plot


campaign_plot<- train_set %>% ggplot(aes(x=campaign))+geom_histogram()+ggtitle("campaign")+theme(plot.title = element_text(size=10))
campaign_plot

campaign_rate_plot<- train_set %>%group_by(campaign) %>% summarise(mean=mean(y))  %>% ggplot(aes(x=campaign,y=mean),size=10)+geom_point()
campaign_rate_plot



### emp.var.rate plot 


emp.var.rate_plot<- train_set %>% ggplot(aes(x=emp.var.rate))+geom_histogram()+ggtitle("emp.var.rate")+theme(plot.title = element_text(size=10))
emp.var.rate_plot
```
emp.var.rate_rate_plot<- train_set %>% 
  group_by(emp.var.rate) %>% 
  summarise(mean=mean(y)) %>% 
  ggplot(aes(x=emp.var.rate,y=mean))+
  geom_point()+geom_smooth(method = "lm")
emp.var.rate_rate_plot
```


### Job Plot 

job_plot <- train_set %>% 
  group_by(job) %>% 
	summarise(count = n()) %>% 
  mutate(job=reorder(job,(-count)))%>%
	ggplot(aes(x =count , y = job)) + 
	geom_bar(stat = 'identity')+ggtitle("job")+theme(plot.title = element_text(size=10))
job_plot


job_rate_plot <- train_set %>% 
  group_by(job) %>% 
	summarise(mean=mean(y)) %>% 
  mutate(job=reorder(job,(mean)))%>%
	ggplot(aes(x =mean , y = job),size=10) + 
	geom_bar(stat = 'identity')+ggtitle("job")+theme(plot.title = element_text(size=10))
job_rate_plot

```


### marital plot


marital_plot <- train_set %>% 
  group_by(marital) %>% 
	summarise(count = n()) %>% 
  mutate(marital=reorder(marital,(-count)))%>%
	ggplot(aes(x =count , y = marital)) + 
	geom_bar(stat = 'identity')+ggtitle("marital")+
  theme(plot.title = element_text(size=10))+scale_x_continuous(breaks=c(5000,15000,25000))
marital_plot


marital_rate_plot <- train_set %>% 
  group_by(marital) %>% 
	summarise(mean=mean(y)) %>% 
  mutate(marital=reorder(marital,(-mean)))%>%
	ggplot(aes(x =mean , y = marital),size=10) + 
	geom_bar(stat = 'identity')+ggtitle("marital")+theme(plot.title = element_text(size=10))
marital_rate_plot



### education plot 


education_plot <- train_set %>% 
  group_by(education) %>% 
	summarise(count = n()) %>% 
  mutate(education=reorder(education,(-count)))%>%
	ggplot(aes(x =count , y = education)) + 
	geom_bar(stat = 'identity')+ggtitle("education")+
  theme(plot.title = element_text(size=10))
education_plot



education_rate_plot <- train_set %>% 
  group_by(education) %>% 
	summarise(mean=mean(y)) %>% 
  mutate(education=reorder(education,(-mean)))%>%
	ggplot(aes(x =mean , y = education),size=10) + 
	geom_bar(stat = 'identity')+ggtitle("education")+theme(plot.title = element_text(size=10))
education_rate_plot



### Housing plot 


housing_plot <- train_set %>% 
  group_by(housing) %>% 
	summarise(count = n()) %>% 
  mutate(housing=reorder(housing,(-count)))%>%
	ggplot(aes(x =count , y = housing)) + 
	geom_bar(stat = 'identity')+ggtitle("housing")+
  theme(plot.title = element_text(size=10))
housing_plot

housing_rate_plot <- train_set %>% 
  group_by(housing) %>% 
	summarise(mean=mean(y)) %>% 
  mutate(housing=reorder(housing,(-mean)))%>%
	ggplot(aes(x =mean , y = housing),size=10) + 
	geom_bar(stat = 'identity')+ggtitle("housing")+theme(plot.title = element_text(size=10))
housing_rate_plot
```


### loan plot 


loan_plot <- train_set %>% 
  group_by(loan) %>% 
	summarise(count = n()) %>% 
  mutate(loan=reorder(loan,(-count)))%>%
	ggplot(aes(x =count , y = loan)) + 
	geom_bar(stat = 'identity')+ggtitle("loan")+
  theme(plot.title = element_text(size=10))
loan_plot

loan_rate_plot <- train_set %>% 
  group_by(loan) %>% 
	summarise(mean=mean(y)) %>% 
  mutate(loan=reorder(loan,(-mean)))%>%
	ggplot(aes(x =mean , y = loan),size=10) + 
	geom_bar(stat = 'identity')+ggtitle("loan")+theme(plot.title = element_text(size=10))
loan_rate_plot



### contact plot 

contact_plot <- train_set %>% 
  group_by(contact) %>% 
	summarise(count = n()) %>% 
  mutate(contact=reorder(contact,(-count)))%>%
	ggplot(aes(x =count , y = contact)) + 
	geom_bar(stat = 'identity')+ggtitle("contact")+
  theme(plot.title = element_text(size=10))
contact_plot

contact_rate_plot <- train_set %>% 
  group_by(contact) %>% 
	summarise(mean=mean(y)) %>% 
  mutate(contact=reorder(contact,(-mean)))%>%
	ggplot(aes(x =mean , y = contact),size=10) + 
	geom_bar(stat = 'identity')+ggtitle("contact")+theme(plot.title = element_text(size=10))
contact_rate_plot



### month plot 


month_plot <- train_set %>% 
  group_by(month) %>% 
	summarise(count = n()) %>% 
  mutate(month=reorder(month,(-count)))%>%
	ggplot(aes(x =count , y = month)) + 
	geom_bar(stat = 'identity')+ggtitle("month")+
  theme(plot.title = element_text(size=10))
month_plot



month_rate_plot <- train_set %>% 
  group_by(month) %>% 
	summarise(mean=mean(y)) %>% 
  mutate(month=reorder(month,(-mean)))%>%
	ggplot(aes(x =mean , y = month),size=10) + 
	geom_bar(stat = 'identity')+ggtitle("month")+theme(plot.title = element_text(size=10))
month_rate_plot



### days plot



day_of_week_plot <- train_set %>% 
  group_by(day_of_week) %>% 
	summarise(count = n()) %>% 
  mutate(day_of_week=reorder(day_of_week,(-count)))%>%
	ggplot(aes(x =count , y = day_of_week)) + 
	geom_bar(stat = 'identity')+ggtitle("day_of_week")+
  theme(plot.title = element_text(size=10))
day_of_week_plot


day_of_week_rate_plot <- train_set %>% 
  group_by(day_of_week) %>% 
  summarise(mean=mean(y)) %>% 
  mutate(day_of_week=reorder(day_of_week,(-mean)))%>%
  ggplot(aes(x =mean , y = day_of_week),size=10) + 
  geom_bar(stat = 'identity')+ggtitle("day_of_week")+theme(plot.title = element_text(size=10))
day_of_week_rate_plot



### Previous plot 

previous_plot <- train_set %>% 
  group_by(previous) %>% 
  summarise(count = n()) %>% 
  mutate(previous=reorder(previous,(-count)))%>%
  ggplot(aes(x =count , y =previous)) + 
  geom_bar(stat = 'identity')+ggtitle("previous")+
  theme(plot.title = element_text(size=10))
previous_plot
```


previous_rate_plot <- train_set %>% 
  group_by(previous) %>% 
  summarise(mean=mean(y)) %>%
  ggplot(aes(y =mean , x = previous),size=10) + 
  geom_point()+geom_smooth()+ggtitle("previous")+theme(plot.title = element_text(size=10))
previous_rate_plot



### Poutcome plot

poutcome_plot <- train_set %>% 
  group_by(poutcome) %>% 
  summarise(count = n()) %>% 
  mutate(poutcome=reorder(poutcome,(-count)))%>%
  ggplot(aes(x =count , y =poutcome)) + 
  geom_bar(stat = 'identity')+ggtitle("poutcome")+
  theme(plot.title = element_text(size=10))
poutcome_plot




poutcome_rate_plot <- train_set %>% 
  group_by(poutcome) %>% 
	summarise(mean=mean(y)) %>% 
  mutate(poutcome=reorder(poutcome,(-mean)))%>%
	ggplot(aes(x =mean , y = poutcome),size=10) + 
	geom_bar(stat = 'identity')+ggtitle("poutcome")+theme(plot.title = element_text(size=10))
poutcome_rate_plot


### nr.employed plot 


nr.employed_plot <- train_set %>% ggplot(aes(x=nr.employed))+geom_histogram()+ggtitle("nr.employed")+theme(plot.title = element_text(size=10))
nr.employed_plot


train_set$y<- ifelse(train_set$y == 1, "yes","no")
train_set  %>% filter(nr.employed>10000) %>% ggplot(aes(y=nr.employed,x=y,fill=y)) +
  geom_boxplot()




### QQ-Plots


age_qq<-  
  data%>%
  select(age) %>%
  ggplot(data = ., aes(sample = scale(.))) + 
  stat_qq() +
  stat_qq_line(col = "blue") +
  ggtitle("age")

cons.price.idx_qq<-  
  data%>%
  select(cons.price.idx) %>%
  ggplot(data = ., aes(sample = scale(.))) + 
  stat_qq() +
  stat_qq_line(col = "blue")  +
  ggtitle("cons.price.idx")

cons.conf.idx_qq<-  
  data%>%
  select(cons.conf.idx) %>%
  ggplot(data = ., aes(sample = scale(.))) + 
  stat_qq() +
  stat_qq_line(col= "blue") +
  ggtitle("cons.conf.idx")


duration_qq<-  
  data%>%
  select(duration) %>%
  ggplot(data = ., aes(sample = scale(.))) + 
  stat_qq() +
  stat_qq_line(col = "blue")  +
  ggtitle("duration")

euribor3m_qq<-  
  data%>%
  select(euribor3m) %>%
  ggplot(data = ., aes(sample = scale(.))) + 
  stat_qq() +
  stat_qq_line(col = "blue") +
  ggtitle("euribor3m")

campaign_qq<-  
  data%>%
  dplyr::select(campaign) %>%
  ggplot(data = ., aes(sample = scale(.))) + 
  stat_qq() +
  stat_qq_line(col= "blue")+
  ggtitle("campaign")

emp.var.rate_qq<-  
  data%>%
  select(emp.var.rate) %>%
  ggplot(data = ., aes(sample = scale(.))) + 
  stat_qq() +
  stat_qq_line(col = "blue") +
  ggtitle("emp.var.rate")

nr.employed_qq<-  
  data%>%
  select(nr.employed) %>%
  ggplot(data = ., aes(sample = scale(.))) + 
  stat_qq() +
  stat_qq_line(col = "blue") +
  ggtitle("nr.employed")

ggarrange(age_qq,cons.price.idx_qq,cons.conf.idx_qq,duration_qq,euribor3m_qq,campaign_qq,emp.var.rate_qq,nr.employed_qq)
```


### Correlation plot




ggcorrplot(cor(train_set[,c("age", "campaign","cons.conf.idx", "cons.price.idx","previous","duration","emp.var.rate","euribor3m","nr.employed")]),hc.order = TRUE,
           type = "lower",
           lab = TRUE)



# reference model
test_set$y <- ifelse(test_set$y==1,"yes","no")
reference <- test_set %>% mutate(reference="no") %>% .$reference %>% as.vector()
reference[2] <- "yes"
confusionMatrix(as.factor(reference),as.factor(test_set$y),positive = "yes")

```




### Knn algortihm 


set.seed(1)
train_set$y <- as.factor(train_set$y)
test_set$y <- as.factor(test_set$y)
#setup train control
fitControl <- trainControl(method = "cv",
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           savePredictions="all")
#fit knn model
fit_knn <- train(y~nr.employed+contact+cons.price.idx+previous+default+month+job+poutcome,method="knn",data = train_set,metric="ROC", trControl=fitControl)
# optimize  using threshoulder
stats_knn <- thresholder(fit_knn ,threshold = seq(0.5,1,0.001),final=TRUE)
stats_knn %>% ggplot(aes(x=prob_threshold,`Balanced Accuracy`))+geom_point()



#test model
knn_pred <- as.factor(ifelse(predict(fit_knn,as.data.frame(test_set),type="prob")$no>=stats_knn$prob_threshold[which.max(stats_knn$`Balanced Accuracy`)],"no","yes"))
confusionMatrix(knn_pred,as.factor(test_set$y),positive = "yes")



### Decision tree algorithm



#fit decision tree 
fit_rpart <- train(y~nr.employed+contact+cons.price.idx+previous+default+month+job+poutcome,data = train_set,
                   method="rpart",metric="ROC",trControl=fitControl,tuneLength=20)
# optimize  using threshoulder
stats_rpart <- thresholder(fit_rpart,threshold = seq(0.5,1,0.001),final=TRUE)
stats_rpart %>% ggplot(aes(x=prob_threshold,`Balanced Accuracy`))+geom_point()


#test model
rpart_pred <- as.factor(ifelse(predict(fit_rpart,as.data.frame(test_set),type="prob")$no>stats_rpart$prob_threshold[which.max(stats_rpart$`Balanced Accuracy`)],"no","yes"))

confusionMatrix(data=rpart_pred,reference=as.factor(test_set$y),positive = "yes")



### glm model
#fit glm model
fit_glm <- train(y~nr.employed+contact+cons.price.idx+previous+default+month+job+poutcome,data=train_set,method="glm",metric="ROC",trControl=fitControl)
# optimize  using threshoulder
stats <- thresholder(fit_glm ,threshold = seq(0.5,1,0.001),final=TRUE)
stats%>% ggplot(aes(x=prob_threshold,`Balanced Accuracy`))+geom_point()



#test model
glm_pred <- as.factor(ifelse(predict(fit_glm,as.data.frame(test_set),type="prob")$no>stats$prob_threshold[which.max(stats$`Balanced Accuracy`)],"no","yes"))

confusionMatrix(glm_pred,as.factor(test_set$y) ,positive = "yes")



### Random Forest model

#fit random forest
fit_rf<- train(y~nr.employed+contact+cons.price.idx+previous+default+month+job+poutcome,method="rf",data=train_set,metric="ROC",trControl=fitControl)
# optimize  using threshoulder
stats_rf <- thresholder(fit_rf ,threshold = seq(0.5,1,0.001),final=TRUE)
stats_rf %>% ggplot(aes(x=prob_threshold,`Balanced Accuracy`))+geom_point()
#test model
rf_pred <- as.factor(ifelse(predict(fit_rf,as.data.frame(test_set),type="prob")$no>stats_rf$prob_threshold[which.max(stats_rf$`Balanced Accuracy`)],"no","yes"))
class(rf_pred )
confusionMatrix(rf_pred,as.factor(test_set$y) ,positive = "yes")


### Ensemble



Ensemble_data <- data.frame(glm=ifelse(glm_pred=="yes",1,0), rpart=ifelse(rpart_pred=="yes",1,0), rf=ifelse(rf_pred=="yes",1,0),knn=ifelse(knn_pred=="yes",1,0))

ensemble_pred <- ifelse(rowMeans(Ensemble_data)>= 0.5,"yes","no")
confusionMatrix(data=as.factor(ensemble_pred),reference=as.factor(test_set$y) ,positive = "yes")







## Validation
#validate the ensemle model

knn_pred_Validation <- as.factor(ifelse(predict(fit_knn,as.data.frame(Validation),type="prob")$no>=stats_knn$prob_threshold[which.max(stats_knn$`Balanced Accuracy`)],"no","yes"))

rpart_pred_Validation <- as.factor(ifelse(predict(fit_rpart,as.data.frame(Validation),type="prob")$no>stats_rpart$prob_threshold[which.max(stats_rpart$`Balanced Accuracy`)],"no","yes"))

glm_pred_Validation <- as.factor(ifelse(predict(fit_glm,as.data.frame(Validation),type="prob")$no>stats$prob_threshold[which.max(stats$`Balanced Accuracy`)],"no","yes"))

rf_pred_Validation <- as.factor(ifelse(predict(fit_rf,as.data.frame(Validation),type="prob")$no>stats_rf$prob_threshold[which.max(stats_rf$`Balanced Accuracy`)],"no","yes"))
```



Ensemble_data <- data.frame(glm=ifelse(glm_pred_Validation=="yes",1,0), rpart=ifelse(rpart_pred_Validation=="yes",1,0), rf=ifelse(rf_pred_Validation=="yes",1,0),knn=ifelse(knn_pred_Validation=="yes",1,0))

ensemble_pred <- ifelse(rowMeans(Ensemble_data)>= 0.5,"yes","no")
confusionMatrix(data=as.factor(ensemble_pred),reference=as.factor(Validation$y) ,positive = "yes")


