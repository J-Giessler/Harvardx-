## Movielens Project



library(lubridate)
##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")



# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#define RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}




# Data Exploration 

#In the Data Exploration section we explore the data and try to find features that can help us to predict the ratings. 

## Data set discribtion
#First we look get a rough overview of the data. 

#As we can see the edx data set consists of 6 columns and 9000055 rows.

dim(edx)

#We now can see that the validation data set consists of 6 columns and 999999 rows.

dim(validation)

### Na values
#It is good to check for Na values, since Na values can cause problems by calculations later.
#As we can see there are no Na values in the edx nor in the validation set.

colSums(is.na(edx))

colSums(is.na(validation))

### Overview

#Next we look at the summary statistics from the summary() function.

summary(edx)


### Split data

#We continue by splitting the edx data into a training and a test set.
#The training set is used to explore the data, train the models.The test set is used to  evaluate the models on there performance and choose the best model for the validation data.


edx <- mutate(edx, date = as_datetime(timestamp))
validation <- mutate(validation, date = as_datetime(timestamp))
set.seed(2,sample.kind = "Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")



### features

As we can see, that we have userId, movieId, genres and timpstamp as features to explore. 
To explore the timpsamp feature we have to change it to a date variable.
```{r echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}
names(train_set)
train_set <- train_set %>% mutate(date = round_date(date, unit = "day")) 
```

### Variation

#Now we check if any features doesn't vary across the data set and has therefore no predictive power.

nearZeroVar(train_set[,c("userId","genres","movieId")],saveMetrics = TRUE)





## Vizualisation 

### userId 

We can start by looking at the userId feature. 
The x-axis represents the different users, the y-axis represents the average rating per user and the red horizontal line is the average rating of all ratings. 
We observe that the ratings vary a lot depending on which user is rating the movies. Some users are rating movies well above the average rating and some users rate movies well below the average rating.



train_set %>% group_by(userId) %>% summarise(rating = mean(rating)) %>%
  ggplot(aes(userId,rating))+geom_point(alpha=0.1)+ geom_hline(yintercept = mean(train_set$rating),col="red",size=2)





### movieId

#We plot the movieId against the average rating per movieId. The ratings vary across the different movies. Some movies are rated above the average and some movies are rated way bellow the average. 

train_set %>% group_by(movieId) %>% summarise(rating = mean(rating)) %>%
  ggplot(aes(movieId,rating))+geom_point(alpha=0.2)+ geom_hline(yintercept = mean(train_set$rating),col="red",size=2)





#genres

##Next we look at the different genres and there average ratings. We can clearly see that depending on which genre the movie the movie is also rated very differenly.##


train_set %>% group_by(genres) %>% summarise(rating=mean(rating)) %>% ggplot(aes(genres,rating)) + geom_point() + geom_hline(yintercept = mean(train_set$rating),col="red",size=2)









### Timpstamp

#Now we analyze if there is a trend in time of the ratings. We can see that the ratings are constant over time.

#In the plot below is seem like there isn't any trend in time. But we will further analyze if it matters if the movies were rated on different days of the week, months or years. 


train_set <- train_set %>% mutate(date = round_date(date, unit = "day")) 
test_set <- test_set %>% mutate(date = round_date(date, unit = "day")) 
train_set %>% group_by(date) %>% summarise(rating=mean(rating)) %>% ggplot(aes(date,rating)) + geom_point()+geom_hline(yintercept = mean(train_set$rating),col= "red",size=2)





### day of the week 

#In the plot below the average ratings for each day of the week are shown. As we can see there is no difference between the day.


train_set<- train_set %>% mutate(day=weekdays(date),month=months(date),year=year(date))
test_set<- test_set %>% mutate(day=weekdays(date),month=months(date),year=year(date))
train_set %>% ggplot(aes(day,rating,goup=day)) + geom_boxplot()+geom_hline(yintercept = mean(train_set$rating),col= "red",size=2)




### months

#The next plot shows the average rating for each month. 
#By looking at the plot, we can see that the month the movie was rated has no influence on the rating of the movie.

train_set %>%  ggplot(aes(month,rating,group=month)) +geom_boxplot()+geom_hline(yintercept = mean(train_set$rating),col= "red",size=2)






### year

#Furthermore we can look at the year the movie was rated. 
#We can see that the only year the  movies were rated different was the first year 1995.
#But apparently in year 1995 are not enough ratings. 


train_set %>%  ggplot(aes(year,rating, group=year)) + geom_boxplot()+geom_hline(yintercept = mean(train_set$rating),col= "red",size=2)




#The visualization of the features showed us that userId, movieId and genres do indeed have an influence on movie ratings. 

# Modeling 

#In the modeling section we use the insights we have gained from exploring the data and use machine learning techniques to predict the ratings.
#The model performance will be evaluated with the Root-mean-squares-error (RMSE).


### Naive model

#We start with the Naive mode, which predicts all ratings to be the average rating. 
#We use this model as a reference to see, how much we can improve the predictive capabilities of our model.


mu <- mean(train_set$rating)

pred_mu <-  test_set %>% 
  mutate(pred =mu ) %>%
  .$pred


model_rmse[1,1]<-"Naive model"
model_rmse[1,2] <- RMSE(pred_mu , test_set$rating)
model_rmse


### userId model

#The userId model tries to account for the user bias, since as we saw in the data visualization section some user rate movies different than others.
#In order to calculate the user  bias we group the data set by userid and subtract the average rating from the average rating of each user.
#If the user bias is -1 than this means, that this user rates movies on average 1 point worse than the average movie rating. 


  

mu <- mean(train_set$rating)
bu <- train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu))

pred_bu <-  test_set %>% 
  left_join(bu, by = "userId") %>% 
  mutate(pred =mu+b_u ) %>%
  .$pred


model_rmse[2,1]<-"userId model"
model_rmse[2,2] <- RMSE(pred_bu , test_set$rating)
model_rmse


### userId + movieId model

#The userId+ movieId model tries to account for the user bias and movie bias, 
#since as we saw in the data exploration section some movies are rated different than others,
#since some movies are just better than others. 
#In order to calculate the movie bias we group the dataset by movieId and subtract the average rating and the user bias from the average rating of each movie.




bi <- train_set %>% 
  left_join(bu, by = "userId") %>%
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu - b_u ))

pred_bu_bi <-  test_set %>% 
  left_join(bu, by = "userId") %>% 
  left_join(bi, by = "movieId") %>% 
  mutate(pred =mu+b_u+ b_i ) %>%
  .$pred

RMSE(pred_bu_bi , test_set$rating)

model_rmse[3,1]<-"userId + movieId model"
model_rmse[3,2] <- RMSE(pred_bu_bi , test_set$rating)
model_rmse







### userId + movieId + genres model

#The userId+ movieId model  tries to account for the user bias and movie bias,
#since as we saw in the data visualization section some movies are rated different than others, since some movies are just better than others.
#In order to calculate the movie bias we group the dataset by movieId and subtract the average rating and the user bias from the average rating of each movie.



genre_avgs <- train_set %>% 
  left_join(bu, by = "userId") %>% 
  left_join(bi, by = "movieId") %>% 
  group_by(genres)%>%
  summarize(b_g =mean(rating - mu - b_u - b_i ))

pred_bu_bi_g <-  test_set %>% 
  left_join(bu, by = "userId") %>% 
  left_join(bi, by = "movieId") %>% 
  left_join(genre_avgs, by="genres")%>%
  mutate(pred =mu+ b_u + b_i +b_g) %>%
  .$pred

model_rmse[4,1]<-"userId + movieId + genres model"
model_rmse[4,2] <- RMSE(pred_bu_bi_g , test_set$rating)
model_rmse






### Regularization userId + movieId + genres model

#Next we use regularization to punish movie ratings that were not rated as much. 
#Since the a low number of ratings will lead to inaccurate predictions. 
#For example, if a movie was only rated by one user and this user gave the movie a 5 star rating, we cannot conclude that this movie is in fact much better that a movie which  was rated 1000 times and only has a rating of 4. 
#In order to punish predictions that were made on a small sample size we use lambda as an optimization parameter. 
#As we can see in the plot bellow the optimal value for lambda is 15. 





lambdas <- seq(10, 20, 1)

rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_u <- train_set %>% 
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu)/(n()+l))
  
  b_i <- train_set %>%
    left_join(b_u, by = "userId") %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu - b_u)/(n()+l))
  
  
  genre_avgs <- train_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(genres)%>%
    summarize(b_g =sum(rating - mu - b_u - b_i  )/(n()+l))
  
  predicted_ratings <- test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(genre_avgs, by='genres') %>%
    mutate(pred = mu + b_i + b_u  + b_g ) %>%
    .$pred
  
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)  













#Validation

#Lastly we train the Regularization userId + movieId + genres model on the entire edx dataset and test it on the  validation dataset. 
#This should show us how the model will perform in the real world, since we didn't look at the validation set at all throughout the project.

```{r echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}
final_model<-function(test_set,train_set){
  
  l <-  15
  mu <- mean(train_set$rating)
  genre_avgs <- train_set %>% 
           group_by(genres)%>%
           summarize(b_g =sum(rating - mu  )/(n()+l))

  b_i <- train_set %>%
          left_join(genre_avgs, by='genres') %>%
          group_by(movieId) %>%
          summarize(b_i = sum(rating - mu - b_g)/(n()+l))
  
  b_u <- train_set %>% 
          left_join(b_i, by="movieId")%>%
          left_join(genre_avgs, by='genres') %>%
          group_by(userId) %>%
          summarize(b_u = sum(rating - b_i - mu- b_g )/(n()+l))
  
  predicted_ratings <- test_set %>% 
          left_join(b_i, by = "movieId") %>%
          left_join(b_u, by = "userId") %>%
          left_join(genre_avgs, by='genres') %>%
          mutate(pred = mu + b_i + b_u  + b_g ) %>%
          .$pred
  return(RMSE(test_set$rating,predicted_ratings))
  
}



# RMSE of the final model
final_model(validation,edx)







# Conclusion


#The goal of this project was to built a movie recommendation system, which predicts the ratings each user would make for each movie.
#We explored the  data to get insights  about the predictive power of the features. Than we used a machine learning technique that doesn't take a lot of computational power for large data set. Than we evaluated our models we built and choose a model for the validation set.
#Our final model is the "Regularization userId + movieId + genres model" and  achieved a RMSE of 0.8654897 on the validation set.




