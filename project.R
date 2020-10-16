library(ggplot2)
library(dplyr)
library(statsr)
library(leaps)

load('movies.Rdata')
View(movies)

g = lm(movies$audience_score ~ movies$runtime 
       + movies$mpaa_rating + movies$imdb_rating 
       + movies$imdb_num_votes + movies$critics_rating 
       + movies$critics_score + movies$audience_rating 
       + movies$best_pic_nom + movies$best_dir_win
       + movies$top200_box)
summary(g)
plot(g)
hist(g$residuals)

quantile(movies$audience_score)
movies %>% summarise(mean = mean(movies$audience_score), median = median(movies$audience_score), SD = sd(movies$audience_score))

ggplot(data = movies, aes(x = audience_score)) +
  geom_histogram(binwidth = 5, fill = 'lightblue', color = 'skyblue') +
  ggtitle('Histogram of Audience Scores') +
  xlab('Score') +
  ylab('Count')

ggplot(data = movies, aes(x = critics_score, y = audience_score)) +
  geom_jitter(color = 'chocolate1') +
  stat_smooth(method = "lm") +
  ggtitle('Critics Score vs Audience Score') +
  xlab('Critics Score') +
  ylab('Audience Score')

g = lm(movies$audience_score ~ movies$runtime 
       + movies$mpaa_rating + movies$imdb_rating 
       + movies$imdb_num_votes + movies$critics_rating 
       + movies$critics_score + movies$audience_rating 
       + movies$best_pic_nom + movies$best_dir_win
       + movies$top200_box)
summary(g)

g = lm(movies$audience_score ~ movies$runtime 
       + movies$mpaa_rating + movies$imdb_rating 
       + movies$imdb_num_votes + movies$critics_rating 
       + movies$critics_score + movies$audience_rating 
       + movies$best_pic_nom
       + movies$top200_box)
summary(g)

g = lm(movies$audience_score ~ movies$runtime 
       + movies$mpaa_rating + movies$imdb_rating 
       + movies$imdb_num_votes
       + movies$critics_score + movies$audience_rating 
       + movies$best_pic_nom
       + movies$top200_box)
summary(g)

g = lm(movies$audience_score ~ movies$runtime 
       + movies$mpaa_rating + movies$imdb_rating 
       + movies$imdb_num_votes
       + movies$critics_score + movies$audience_rating 
       + movies$best_pic_nom
       + movies$top200_box)
summary(g)

g = lm(movies$audience_score ~ movies$runtime 
       + movies$imdb_rating 
       + movies$imdb_num_votes
       + movies$critics_score + movies$audience_rating 
       + movies$best_pic_nom
       + movies$top200_box)
summary(g)

g = lm(movies$audience_score ~ movies$runtime 
       + movies$imdb_rating 
       + movies$imdb_num_votes
       + movies$critics_score + movies$audience_rating 
       + movies$best_pic_nom)
summary(g)

g = lm(movies$audience_score ~ movies$runtime 
       + movies$imdb_rating 
       + movies$imdb_num_votes
       + movies$critics_score + movies$audience_rating 
       + movies$best_pic_nom)
summary(g)

g = lm(movies$audience_score ~ movies$runtime 
       + movies$imdb_rating
       + movies$critics_score + movies$audience_rating 
       + movies$best_pic_nom)
summary(g)
106 - 60
g = lm(movies$audience_score ~ movies$runtime 
       + movies$imdb_rating + movies$audience_rating 
       + movies$best_pic_nom)
summary(g)
levels(movies$audience_rating)
Train to Busan: runtime = 118, imdb = 7.8, audience rating = Upright, best pic nom = no

ttb = data.frame(runtime = 118, imdb_rating = 7.8, audience_rating = 'Upright', best_pic_nom = 'no')
predict(movie_model, ttb)
predict(movie_model, ttb, interval = "prediction", level = 0.95)

movie_model = lm(audience_score ~ runtime + imdb_rating + audience_rating + best_pic_nom, data = movies)

is.na(movies$imdb_url)
