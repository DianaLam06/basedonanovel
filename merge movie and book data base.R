# merge movie and book data base
setwd("/home/yichijin/Desktop/Diana/DataIncubator/library")

source("read all data.R")

library(lubridate)
# merge inventory with tmdb data base first
tmdb2 <- tmdb_full %>% mutate(
  simpleTitleMovie = simple_title
)


movies <- semi_join( tmdb2, imdb_movies, by = "simpleTitleMovie")
# has duplicates, take the more popular one for now

movies_int <- movies %>% group_by(simpleTitleMovie) %>% 
  arrange(simpleTitleMovie, desc(popularity)) %>% slice(1)

# then for each movie in the data base, calculate the months needed to wait after the release

# seed book_movie with the first movie in data base -----------------------

# need to populate with one
i = 1

movieTitle <- imdb_books$simpleTitleMovie[i]
bookTitle <- imdb_books$simpleTitleBook[i] 

book <- checkedout_summary %>% filter( simpleTitle == str_trim(str_to_lower((bookTitle))))
book_copies <- catalog_copies %>% filter (simpleTitle == str_trim(str_to_lower((bookTitle))))
book_copies_num <- sum(book_copies$copies)

release <- tmdb_full %>% filter( str_trim(str_to_lower(title)) == str_trim(str_to_lower((movieTitle)))) %>%
  arrange(desc(popularity)) %>% slice(1) %>%
  mutate(
    release_date = parse_date(release_date)
  )
# issue when we have a book with no movie equivalent


if( is.null(release$title) || is.null(book$simpleTitle) || book_copies_num ==0) {
  print(paste0("skipping ", movieTitle,is.null(release$title), is.null(book$simpleTitle), (book_copies_num ==0))) 
  next
}


book2 <- book %>% mutate(
  hold = (checkout_sum > book_copies_num)
)

book2[["lag_hold"]] <- lag(book2$hold)


book2 <- book2 %>% mutate(
  hold_free_int = ifelse( time < release$release_date , NA, 
                          ifelse(time >= release$release_date && hold == F && lag_hold == F, 0, 1 ))  ,
  hold_free = ifelse(month(time) == month(release$release_date) && year(time) == year(release$release_date) &&
                       hold == F && lag_hold == F, 0,
                     ifelse(month(time) == month(release$release_date) && year(time) == year(release$release_date) &&
                              (hold != F || lag_hold != F) , 1, hold_free_int   ) )
  
) 




if (book2$hold[nrow(book2)] == T && book2$hold[nrow(book2)] == T ){ 
  num_months_wait = 999
} else{
  num_months_wait <- sum(book2$hold_free, na.rm = T)
}
# 999 indicates that the bump is still not over
release[["simpleTitleBook"]] <- bookTitle
release[["num_months_wait"]] <- num_months_wait


book_movie <- release


# start loop --------------------------------------------------------------



#might need to seed book_movie with the first one (i = 4)
for (i in 5: nrow(imdb_books)) {

  movieTitle <- imdb_books$simpleTitleMovie[i]
  bookTitle <- imdb_books$simpleTitleBook[i] 
  
  book <- checkedout_summary %>% filter( simpleTitle == str_trim(str_to_lower((bookTitle))))
  book_copies <- catalog_copies %>% filter (simpleTitle == str_trim(str_to_lower((bookTitle))))
  book_copies_num <- sum(book_copies$copies)
  
  release <- tmdb_full %>% filter( str_trim(str_to_lower(title)) == str_trim(str_to_lower((movieTitle)))) %>%
    arrange(desc(popularity)) %>% slice(1) %>%
    mutate(
      release_date = parse_date(release_date)
    )
  # issue when we have a book with no movie equivalent
  
 hunger <-  tmdb_full %>% filter(str_detect( str_trim(str_to_lower(title)) , "hunger games"))
 
  if( is.null(release$title) || is.null(book$simpleTitle) || book_copies_num ==0) {
   print(paste0("skipping ", movieTitle,is.null(release$title), is.null(book$simpleTitle), (book_copies_num ==0))) 
    next
  }
  
  
  book2 <- book %>% mutate(
    hold = (checkout_sum > book_copies_num)
    )

  book2[["lag_hold"]] <- lag(book2$hold)
 
  
   book2 <- book2 %>% mutate(
     hold_free_int = ifelse( time < release$release_date , NA, 
      ifelse(time >= release$release_date && hold == F && lag_hold == F, 0, 1 ))  ,
     hold_free = ifelse(month(time) == month(release$release_date) && year(time) == year(release$release_date) &&
                          hold == F && lag_hold == F, 0,
                        ifelse(month(time) == month(release$release_date) && year(time) == year(release$release_date) &&
                                 (hold != F || lag_hold != F) , 1, hold_free_int   ) )
  
  ) 
   

   
  
if (book2$hold[nrow(book2)] == T && book2$hold[nrow(book2)] == T ){ 
  num_months_wait = 999
} else{
  num_months_wait <- sum(book2$hold_free, na.rm = T)
  }
   # 999 indicates that the bump is still not over
   release[["simpleTitleBook"]] <- bookTitle
   release[["num_months_wait"]] <- num_months_wait
   

book_movie <- rbind(book_movie, release)

print(paste0("imdb_book id ", i))
  rm(book, book_copies_num, book_copies, release)
}

# merge back in