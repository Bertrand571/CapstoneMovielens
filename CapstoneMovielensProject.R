#########################################################################################################
#########################################################################################################
#########################################################################################################
#
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>CODE PROVIDED ON EDX<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#
#########################################################################################################
#########################################################################################################
#########################################################################################################


##########################################################################################################
# Create edx and validation sets
##########################################################################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
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


#########################################################################################################
#########################################################################################################
#########################################################################################################
#
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> MY CODE <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#
#########################################################################################################
#########################################################################################################
#########################################################################################################

#########################################################################################################
#                                         Environment Creation
#########################################################################################################

# As the creation of the edx and validation sets have been quite long, let's first back up these data,
# in case something goes wrong and we need to come back to it.

# Backup to files
# saveRDS(edx,"edx.Rda")
# saveRDS(validation,"validation.Rda")

# Delete objects
# rm(edx,validation)

#reload saved datasets
# edx <- readRDS(file="edx.Rda")
# validation <- readRDS(file="validation.Rda")



# Loading required libraries
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(formattable)) install.packages("formattable", repos = "http://cran.us.r-project.org")




# Creation of the test and train sets, as well as the RMSE function

# Out of the edx dataset that was previously created, we create 2 data sets (train_set and test_set)
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2,list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

# Let's make sure to exclude from the test set users and movies that do not appear in the
# training set.
temp <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# and put back in the training set these elements (no waste of data):
removed<- test_set %>% anti_join(temp)

test_set<- temp
train_set<- rbind(train_set,removed)

rm(removed,temp)

# As the quality of the model will be assessed by the RMSE between the prediction and the true rating,
# a function to calculate that will come handy, and used repeatedly
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#########################################################################################################
#                             First try: Mimicking the course approach
#########################################################################################################

# We use first the same approach as professor Irizzary in his course, with 3 steps:
# a) first model with the mean
# b) adding movie effect
# c) adding user effect


# a) for each prediction we provide the mean of all ratings, extracted from the training set
#########################################################################################################
mu<-mean(train_set$rating)
naive_rmse<-RMSE(test_set$rating,mu)

#We initiate a tibble with this first result:
rmse_results<-tibble("Methods"= c("Just the average"),results=naive_rmse)

# b) adding movie effect: considering that some movies are better than others, and therefore usually receive higher ratings,
# we initiate a numeric for each movie. This value is calculated for each movie as the mean of the rating differences with
# the average rating for a movie
#########################################################################################################

movie_avgs<-train_set %>%
                    group_by(movieId) %>%
                    summarize(b_i=mean(rating-mu))

# Now that we have determined the "bias" for every movie, we can test how effective it is for prediction accuracy:
predicted_ratings<- mu + test_set %>%
  left_join(movie_avgs, by="movieId") %>%
  pull(b_i)

# and store the result pour memoire in the previous tibble
rmse_results<-rbind(rmse_results,list("Movie Effect", RMSE(test_set$rating, predicted_ratings)))


# c) adding user effects: It is obvious that some users are cranky, while others are more generous.
# Therefore we introduce a user effectwhich is a bias implemented as a numeric per user.
# This value (b_u) is, for this user, the mean of the difference between the ratings given and
# the "public rating" of this movie, estimated as mu+b_i as we have seen previously:
#########################################################################################################
user_avgs <- train_set %>%
  left_join(movie_avgs,by="movieId") %>%
  group_by(userId) %>% 
  summarize(b_u=mean(rating-mu-b_i))

# we can immediately test the accuracy of our model on the test set.
# Let's first create the predictions
predicted_ratings <- test_set %>% 
  left_join(movie_avgs,by="movieId") %>%
  left_join(user_avgs,by="userId") %>%
  mutate(pred=mu+b_i+b_u) %>%
  pull(pred)

# And then store, in the previous tibble the result of this method
rmse_results<-rbind(rmse_results,list("Movie+User Effect", RMSE(predicted_ratings,test_set$rating)))

# As the result on the test set is 0.8657 and our first target at 0.86549, we need to enhance our methods a bit

#########################################################################################################
#                   Second try: Elaborating further using biases and regularization
#########################################################################################################

# Using bias on dates
#########################################################################################################

# Let's have a look at dates. The number of the week during the year, as well as the 
# day of the week could have some influence on the ratings

# First let's add for each vote, the week number as well as the day of the week
train_set_with_dates<-train_set %>%
  mutate(date=as_datetime(timestamp),week=week(date),day_of_week=as.POSIXlt(date)$wday)


# Is there an influence of the week on the rating?
week_averages<-train_set_with_dates %>%
  group_by(week) %>%
  summarize(week_avg=mean(rating))

week_averages %>% ggplot(aes(week,week_avg))+geom_point()
# It looks like there is an influence, but the pattern is not very clear, and therefore difficult to exploit

# Is there an influence of the day of the week ("Monday", Tuesday", ...) on the vote?
# To determine it, let's compute, for every weekday the average difference between a rating and its week average
train_set_with_dates %>%
  left_join(week_averages,by="week") %>%
  group_by(day_of_week) %>%
  summarize(avg_day_to_week_diff=mean(rating-week_avg)) %>%
  ggplot(aes(day_of_week,avg_day_to_week_diff))+geom_point()

# This diagram shows some influence of the day of the week, with the middle of the week providing less generous ratings
# we add this in our model, using the same technique (calculating for each day of the week a bias as the mean of
# the difference of the ratings with the value previously determined (mu+b_i+b_u)) 

weekday_avgs <- train_set_with_dates %>%
  left_join(movie_avgs,by="movieId") %>%
  left_join(user_avgs,by="userId") %>%
  group_by(day_of_week) %>% 
  summarize(d_i=mean(rating-mu-b_i-b_u))

max(abs(weekday_avgs$d_i))
#It accounts for 0.0018 of difference at maximum, which is not much, but still worth integrating in our model.

# Let's evaluate our new model on the test set.
# Firstly we calculate the predictions :
predicted_ratings <- test_set %>% 
  mutate(date=as_datetime(timestamp),week=week(date),day_of_week=as.POSIXlt(date)$wday) %>%
  left_join(movie_avgs,by="movieId") %>%
  left_join(user_avgs,by="userId") %>%
  left_join(weekday_avgs,by="day_of_week") %>%
  mutate(pred=mu+b_i+b_u+d_i) %>%
  pull(pred)

range(predicted_ratings)  
# Since the model is linear, it has created values above 5 and below 1 which makes no sense as
# Netflix ratings go from 1 to 5 stars only.
# Let's put that in order by re assigning every predicted rating that is above 5 to 5 (maximum rating)
# and every predicted ratingthat is below 1 to 1 (minimum rating)
predicted_ratings[which(predicted_ratings>5)]<- 5

predicted_ratings[which(predicted_ratings<1)]<- 1

# Asses results and store it pour mémoire with the results of other models
rmse_results<-rbind(rmse_results,list("Movie + User + Weekday Effect", RMSE(predicted_ratings,test_set$rating) ))


# Using bias on genres
#########################################################################################################

# Now let's examine information on the genres, because it is plausible that certain genres
# induce a more positive rating than others.
# Does the data support that view?
# If we define a category as any existing combination of genres in the data frame,
# and restrict this analysis to categories that have at least 1000 elements.
# What are the average and standard error for each category:

major_genres_ratings<-train_set %>%
  group_by(genres) %>%
  summarize(count=n(),avg_rating=mean(rating),std_rating=sd(rating)) %>%
  filter(count>=1000) 

# A plot will help to make things more apparent:
major_genres_ratings$genres <- factor(major_genres_ratings$genres,
                                      levels = major_genres_ratings$genres[order(major_genres_ratings$avg_rating)])

major_genres_ratings %>%
  ggplot(aes(genres, avg_rating)) + 
  geom_col() +  
  geom_errorbar(aes(ymin = avg_rating - std_rating, ymax = avg_rating + std_rating), color="blue" ,width=0.2)

# There is a clear influence of the category on the rating
# So let's try to  use it, but again, let's be conservative and use it only when we are
# certain about the influence a certain category has. Which means that we will not use it
# for categories that have less than 1000 reviews


category_avgs <- train_set %>%
  mutate(date=as_datetime(timestamp),week=week(date),day_of_week=as.POSIXlt(date)$wday) %>%
  left_join(movie_avgs,by="movieId") %>%
  left_join(user_avgs,by="userId") %>%
  left_join(weekday_avgs,by="day_of_week") %>%
  group_by(genres) %>% 
  summarize(g_u_i=mean(rating-mu-b_i-b_u-d_i),count=n()) %>%
  filter(count>=1000)

# What is the maximal influence a category has on rating?
max(abs(category_avgs$g_u_i))
# 0.118 is a material influence. Much stronger than the day of the week.

# Let's examine its impact on the test sample. Will it enhance our previous score?

#Let's first create the data frame with all its 4 needed predictors:
predicted_ratings_predictors <- test_set %>% 
  mutate(date=as_datetime(timestamp),week=week(date),day_of_week=as.POSIXlt(date)$wday) %>%
  left_join(movie_avgs,by="movieId") %>%
  left_join(user_avgs,by="userId") %>%
  left_join(weekday_avgs,by="day_of_week") %>%
  left_join(category_avgs,by="genres")

# unfortunately this  dataframe contains a lot of NAs corresponding to the categories.
# which have less than 1000 reviews. We fill these blanks with "0" (which means that
# this category has no influence) to ensure that this category will play no role in
# the corresponding predictions 

predicted_ratings_predictors$g_u_i[which(is.na(predicted_ratings_predictors$g_u_i))]<-0

# Then let's predict ratings
predicted_ratings<-predicted_ratings_predictors %>%
  mutate(pred=mu+b_i+b_u+d_i+g_u_i) %>%
  pull(pred)

range(predicted_ratings)

# Since the model is linear, it has created values above 5 and below 1, which makes no sense.
# Let's put that in order, as explained previously:
predicted_ratings[which(predicted_ratings>5)]<-5

predicted_ratings[which(predicted_ratings<1)]<-1

# and again, calculate and store the results
rmse_results<-rbind(rmse_results,list("Movie + User + Weekday + Category Effect", RMSE(predicted_ratings,test_set$rating)))


# Using Regularization
#########################################################################################################

#Having a look at the distribution of ratings per movie provides a first insight:
    train_set %>%
    group_by(movieId) %>%
    summarize(count=n()) %>%
    .$count %>%
    sort() %>%
    range()

# Some movies have been rated more than 25000 times while other have been rated only once.
# Let's have a look at the distributions: how many movies have been rated between 0 and 5000
# times, how many have been rated between 5000 and 10.000, etc...
train_set %>%
  group_by(movieId) %>%
  summarize(count=n()) %>%
  mutate(group=round(count/5,-3)*5) %>%
  ggplot(aes(x=factor(group)))+
  geom_bar(stat="count", width=0.7)+
  scale_x_discrete(name="Number of ratings") +
  scale_y_discrete(name="Number of movies rated",limits=c(2500,5000,7500, 10000)) +
  theme_minimal()

# Therefore the confidence one can have in the ratings of these seldom rated movies based
# on this data set varies a lot. And since the RMSE is quite sensitive to large errors we'd
# better be conservative for these ratings, and pick a value closer to the mean of all ratings.
# This can be achieved using regularization, penalizing the predictions with a certain "lambda".
# The optimal lambda parameter can be determined by testing different values using cross validation.
# Therefore we apply penalization using several lambda, on the same model we have built so far:

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  
  train_set_with_dates<-train_set %>%
    mutate(date=as_datetime(timestamp),week=week(date),day_of_week=as.POSIXlt(date)$wday)
  
  b_i <- train_set_with_dates %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))

    b_u <- train_set_with_dates %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))

   d_i <- train_set_with_dates %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(day_of_week) %>%
    summarize(d_i = mean(rating - b_i -b_u - mu)/(n()+l))

    g_u_i <- train_set_with_dates %>%
     left_join(movie_avgs,by="movieId") %>%
     left_join(user_avgs,by="userId") %>%
     left_join(weekday_avgs,by="day_of_week") %>%
     group_by(genres) %>% 
     summarize(g_u_i=mean(rating-mu-b_i-b_u-d_i)/(n()+l),count=n())
    
    g_u_i$g_u_i[g_u_i$count<1000]<-0
    
   test_set_with_dates<-test_set %>%
     mutate(date=as_datetime(timestamp),week=week(date),day_of_week=as.POSIXlt(date)$wday)
   
   
  predicted_ratings <- test_set_with_dates %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(d_i, by = "day_of_week") %>%
    left_join(g_u_i, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + d_i + g_u_i) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

# The lambdas have a noticeable influence on the RMSEs, with an optimum around 4
qplot(lambdas,rmses)

# picking the optimal lambda
optimal_lambda<-lambdas[which.min(rmses)]

rmse_results<-rbind(rmse_results,list("Regularized Movie + User + Weekday + category Effect", min(rmses)))
# Result is 0.8649592, which is in line with our target. We can therefore accept the model as such,
# since we have reasonnable hope that it will provide the expected results on the validation set

#########################################################################################################
#                   Assesment of the method on the validation set
#########################################################################################################

# To increase the volume of training material, we can use the entire edx set, including the test set.
# This will NOT break the golden rule of not training on the validation set

mu <- mean(edx$rating)

edx_set_with_dates<-edx %>%
  mutate(date=as_datetime(timestamp),week=week(date),day_of_week=as.POSIXlt(date)$wday)

b_i <- edx_set_with_dates %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+optimal_lambda))

b_u <- edx_set_with_dates %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+optimal_lambda))

d_i <- edx_set_with_dates %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(day_of_week) %>%
  summarize(d_i = mean(rating - mu - b_i - b_u)/(n()+optimal_lambda))

g_u_i <- edx_set_with_dates %>%
  left_join(b_i,by="movieId") %>%
  left_join(b_u,by="userId") %>%
  left_join(d_i,by="day_of_week") %>%
  group_by(genres) %>% 
  summarize(g_u_i=mean(rating - mu - b_i - b_u - d_i)/(n()+optimal_lambda),count=n())

# As previously, we suppress any influence from the category for categories that have less than 1000 ratings
g_u_i$g_u_i[g_u_i$count<1000]<-0


predicted_ratings <- validation %>%
  mutate(date=as_datetime(timestamp),week=week(date),day_of_week=as.POSIXlt(date)$wday) %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(d_i, by = "day_of_week") %>%
  left_join(g_u_i, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + d_i + g_u_i) %>%
  pull(pred)


rmse_results<-rbind(rmse_results,list("Validation for: Regularized Movie+User+day+genre Effect", RMSE(predicted_ratings, validation$rating)))

# Accuracy on the validation set is 0.86482, which is already below the target of 0.86490 for 25 points.

range(predicted_ratings)
# But as previously, we can observe that some of the predictions can easily be enhanced since they
# are below 1 or above 5, which is the range of possible ratings

# We can easily rectify these elements:
rectified_predicted_ratings<-predicted_ratings
rectified_predicted_ratings[rectified_predicted_ratings<1]<-1
rectified_predicted_ratings[rectified_predicted_ratings>5]<-5

# And  calculate a new RMSE which is even better:
RMSE(rectified_predicted_ratings, validation$rating)

rmse_results<-rbind(rmse_results,list("Validation for: Regularized then rectified Movie+User+day+genre Effect", RMSE(rectified_predicted_ratings, validation$rating)))


#########################################################################################################
# Printing results
#########################################################################################################
# For the assessor's convenience, printing the table with all results:
formattable(rmse_results)
