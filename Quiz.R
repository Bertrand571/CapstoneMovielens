library(stringr)
library(tidyverse)

#Q1
nrow(edx)
ncol(edx)

#Q2
edx %>% filter(rating==0) %>% nrow()
edx %>% filter(rating==3) %>% nrow()

edx %>% filter(rating==0) %>% tally()
edx %>% filter(rating==3) %>% tally()


#Q3
edx %>% select(movieId) %>% unique() %>% nrow()
n_distinct(edx$movieId)

#Q4
edx %>% select(userId) %>% unique() %>% nrow()


#Q5
sum(str_detect(edx$genres,"Drama"))

sum(str_detect(edx$genres,"Comedy"))

sum(str_detect(edx$genres,"Thriller"))

sum(str_detect(edx$genres,"Romance"))


#Solution du cours(Attention, elle tourne durant des heures avec 8GB de RAM):
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


#Q6

Movie<-c("Forrest Gump","Jurassic Park","Pulp Fiction","Shawshank Redemption","Speed 2: Cruise Control")

Ratings<-c(sum(str_detect(edx$title,"Forrest Gump \\(1994\\)")),
            sum(str_detect(edx$title,"Jurassic Park \\(1993\\)")),
            sum(str_detect(edx$title,"Pulp Fiction \\(1994\\)")),
            sum(str_detect(edx$title,"Shawshank Redemption")),
            sum(str_detect(edx$title,"Speed 2: Cruise Control"))
)

Q6.data <- data.frame(Movie,Ratings)
Q6.data[which.max(Q6.data$Ratings),1]


edx[str_which(edx$title,"Forrest Gump"),] %>% select(title)

#Q7
edx %>% group_by(rating) %>% summarize(Nombre=n()) %>% arrange(desc(Nombre))


#Q8
StratifiedRatings<-edx %>% group_by(rating) %>% summarize(Nombre=n()) %>% arrange(desc(Nombre))

3.5 %/% 1

Entiers<-which((StratifiedRatings$rating%/%1)==(StratifiedRatings$rating))
SommeRatingsEntiers<-sum(StratifiedRatings[Entiers,2])
SommeRatingsNonEntiers<-sum(StratifiedRatings[-Entiers,2])
SommeRatingsEntiers+SommeRatingsNonEntiers
sum(StratifiedRatings[,2])

SommeRatingsEntiers
SommeRatingsNonEntiers


edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()
