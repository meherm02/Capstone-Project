if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(dplyr)
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("https://grouplens.org/datasets/movielens/10m/", dl)

ratings <- read.table(text = gsub("::", "\t", readLines("/Users/mehermankikar/Downloads/ml-10M100K/ratings.dat")),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

###ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
##                      col.names = )
movies <- str_split_fixed(readLines("/Users/mehermankikar/Downloads/ml-10M100K/movies.dat"), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1) # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")
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

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

mu_hat <- mean(edx$rating)
 mu_hat

rmse_1 <- RMSE(edx$rating, mu_hat)
 rmse_1

 predictions <- rep(2.5, nrow(edx))
 RMSE(edx$rating, predictions)

 rmse_results <- data_frame(method = "Just the average", RMSE = rmse_1)

 # fit <- lm(rating ~ as.factor(userId), data = movielens)
 mu <- mean(edx$rating)
 movie_avgs <- edx %>%
   group_by(movieId) %>%
   summarize(b_i = mean(rating - mu))

 movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

 predicted_ratings <- mu + edx %>%
   left_join(movie_avgs, by='movieId') %>%
   .$b_i

 model_1_rmse <- RMSE(predicted_ratings, edx$rating)
 rmse_results <- bind_rows(rmse_results,
                           data_frame(method="Movie Effect Model",
                                      RMSE = model_1_rmse ))
 rmse_results %>% knitr::kable()

 # lm(rating ~ as.factor(movieId) + as.factor(userId))
 user_avgs <- edx %>%
   left_join(movie_avgs, by='movieId') %>%
   group_by(userId) %>%
   summarize(b_u = mean(rating - mu - b_i))

 predicted_ratings <- edx %>%
   left_join(movie_avgs, by='movieId') %>%
   left_join(user_avgs, by='userId') %>%
   mutate(pred = mu + b_i + b_u) %>%
   .$pred

 model_2_rmse <- RMSE(predicted_ratings, edx$rating)
 rmse_results <- bind_rows(rmse_results,
                           data_frame(method="Movie + User Effects Model",
                                      RMSE = model_2_rmse ))
 rmse_results %>% knitr::kable()

#lm(rating ~as.factor(movieId) + as.factor(rating) + as.factor(genre))
genre_avgs  <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_y = mean(rating - mu - b_i - b_u))

predicted_ratings <- validation %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_y) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                            data_frame(method="Movie + User + Genre Effects Model",
                                       RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()
