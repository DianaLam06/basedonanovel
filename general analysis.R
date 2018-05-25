setwd("/home/yichijin/Desktop/Diana/DataIncubator/library")

source("read all data.R")

# take out movies whose bumpp is still not over
names(book_movie)

sum(is.na(book_movie$certification)) # 33 are missing certification, 
# some are netflix which have no theatrical release and no ceritification

unique(book_movie$certification)
sum(book_movie$certification == "no us release", na.rm = T) # 12 with no us release

# want to get a dictionary of keyword, genre, and production companies
# since not perfect 1:1 matching in extraction just get a unique list first, then look up their id

keyword_name <- " "
genre_name <- " "
production_name <- " "
for (i in 1: nrow(book_movie)){
  
  append <- unlist(strsplit(book_movie$keyword_name[i], split = ","))
  keyword_name <- list(keyword_name, append)
  
  append2 <-   unlist(strsplit(book_movie$genre_name[i], split = ","))
  genre_name <-list(genre_name , append2)
  
  append3 <- unlist(strsplit(book_movie$production_names[i], split = ","))
  production_name <-list(production_name , append3)
}

keyword_based <- as.tibble(unlist(keyword_name)) %>% filter(
  !is.na(value), 
  value != " ", 
  str_detect(value, "based on")
  ) %>% unique()

genre_name_all <- as.tibble(unlist(genre_name)) %>% mutate(
  simple_value = str_trim(str_to_lower(value))
) %>% filter( value != " ")
genre_list <- as.tibble(unique(genre_name_all$simple_value))
View(genre_list)

production_name_all <- as.tibble(unlist(production_name)) %>% filter(
  !is.na(value), 
  value != " "
) %>% group_by(value) %>% summarize( count = n()) %>% arrange(desc(count))
unique(production_name_all)

# taking out those whose bump still not over and those not released in us

book_movie_full <-  book_movie %>% filter( num_months_wait != 999, certification != "no us release") %>%
  mutate(
    # R vs less than R
    R = (certification == "R"),
    
    # if genre is family 10751
    family = str_detect(genre_id , "10751"),
    
    # more strongly based on a novel (have more see keyword_based)
    based_novel = str_detect(keyword_id , "818") | str_detect(keyword_id,  "223438"),
    
    # see if non-fiction
    non_fiction = str_detect(keyword_name, "based on non fiction book"),
    
    both = (family == T & based_novel == T)
    
  )

  # make indicator variables for genre

for (i in 1 : nrow(genre_list)){
  
  name <- genre_list$value[i]
  book_movie_full[[name]] = str_detect(str_trim(str_to_lower(book_movie_full$genre_name)), name)
  
}

book_movie_full <- book_movie_full %>% mutate(
  adventure_fantasy = (fantasy == T | adventure == T),
  adventure_fantasy_family = (fantasy == T | adventure == T | family == T)
)

sum(book_movie_full$adventure_fantasy)

sum(book_movie_full$adventure_fantasy_family)

# find out how many of each genre we have


rm(by_genre, count, genre)

by_genre <- tibble(
   genre = 1:nrow(genre_list) ,  count = 1:nrow(genre_list) ,
   mean = 1: nrow(genre_list), sd = 1:nrow(genre_list), median = 1:nrow(genre_list)
)


#make <- function(){
for (i in 1:nrow(genre_list)){
 
  genre <- genre_list$value[i]
  test <- book_movie_full[book_movie_full[[genre]] == T, ]
  count <- sum(book_movie_full[[genre]] , na.rm = T)
  
 
  mean <- mean(test$num_months_wait, na.rm = T)
  sd <- sd(test$num_months_wait, na.rm = T)
  median <- median(test$num_months_wait, na.rm = T)
  
  by_genre$genre[i] <- genre
  by_genre$count[i] <- count
  by_genre$mean[i] <- mean
  by_genre$sd[i] <- sd
  by_genre$median[i] <- median
}

drama <- book_movie_full[book_movie_full$drama == T, ]
test <- book_movie_full[book_movie_full[[genre]] == T, ]

names(book_movie_full)
View(genre_count$n_drama)

View(book_movie_full$drama)
sum(is.na(book_movie_full$num_months_wait)) # 0

# make plot by certification

ggplot(data = book_movie_full) +
  geom_histogram(mapping = aes(x = num_months_wait, fill = certification ), 
                 position = "identity", alpha = 0.5, bins = 30) +
  labs( y = "Count", x = "# of months before book is available") +
  scale_fill_discrete( name = "Rating")


ggplot(data = book_movie_full) +
  geom_histogram(mapping = aes(x = num_months_wait, fill = R ), 
                 position = "identity", alpha = 0.5, bins = 30) +
  labs( y = "Count", x = "# of months before book is available") +
  scale_fill_discrete( name = " ", labels = c("Not an R rating", "R rated"))



# plots looking at genre
ggplot(data = book_movie_full) +
  geom_histogram(mapping = aes(x = num_months_wait, fill = drama ), 
                 position = "identity", alpha = 0.5, bins = 30) +
  labs( y = "Count", x = "# of months before book is avilable") +
  scale_fill_discrete( name = "Genre", labels = c("Not a drama", "Drama"))



ggplot(data = book_movie_full) +
  geom_histogram(mapping = aes(x = num_months_wait, fill = thriller ), 
                 position = "identity", alpha = 0.5, bins = 30) +
labs( y = "Count", x = "# of months before book is available") +
  scale_fill_discrete( name = "Genre", labels = c("Not a thriller", "Thriller"))
ggsave("./output/thriller.pdf")

ggplot(data = book_movie_full) +
  geom_histogram(mapping = aes(x = num_months_wait, fill = adventure_fantasy_family ), 
                 position = "identity", alpha = 0.5, bins = 30) +
  labs( y = "Count", x = "# of months before book is free") +
  scale_fill_discrete(position = "top", name = "Genre",
              labels = c("Neither adventure, fantasay, nor family", "Adventure, fantasy, or family")) +
  theme(legend.position = "top")


# linear regression -------------------------------------------------------

lm(num_months_wait ~ certification + drama + adventure_fantasy_family , data = book_movie_full)



by_R <- book_movie_full %>% group_by(R) %>% summarize(
  avg_wait = mean(num_months_wait),
  sd_wait = sd(num_months_wait)
)


by_certification <-   book_movie_full %>% group_by(certification) %>% summarize(
  avg_wait = mean(num_months_wait),
  sd_wait = sd(num_months_wait),
  count = n()
)

zero_wait <- book_movie_full %>% filter (num_months_wait == 0)

book_movie_full$budget # doesn't seem too reliable since a lot of 0's
unique(book_movie_full$production_names)

sum(book_movie_full$non_fiction, na.rm = T) # just 1

check_family <- book_movie_full %>% filter( family == T) %>% select ( 
  simple_title, certification, genre_name, genre_id)

check_based_novel <- book_movie_full %>% filter( based_novel == T) %>% select ( 
  simple_title, certification, keyword_name, keyword_id)

check_both <- book_movie_full %>% filter( both == T) %>% select ( 
  simple_title, certification, keyword_name, keyword_id, genre_id, genre_name)