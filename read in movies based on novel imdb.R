# read in movies based on novel top 500 from imdb from Jan 1 2005 to Dec 31 2017

# Most Popular "Based On Novel" Feature Film Titles Released 1 January 2005 to 31 December 2017

#setwd("/home/yichijin/Desktop/Diana/DataIncubator/library")

library(tidyverse)
#library(plyr)
library(dplyr)
library(stringr)

graphic <- read_delim("./data/CSV/test movies 6.csv", delim = "\t", 
                      col_names = c("Title", "Title2","Rating"),
                      col_types = cols(
                        Title=col_character(),
                        Title2 = col_character(),
                        Rating = col_double()
                        
                      ))

graphic <- graphic %>% mutate(  rank  = as.numeric(gsub("([0-9]+).*$", "\\1", Title2)),
                                year = as.numeric(str_sub(Title2, -5,-2)),
                                type = "graphic", 
                                TitleBook = ifelse(str_trim(Title) == "X-men: Days of Future Past", 
                                                   "Uncanny X-men", Title))
#movies based on graphic novels released between 2005-2017, 
# as based on the popularity ranking (different from imdb ranking)
#need month of release from tmbd


imdb_int <- read_delim("./data/CSV/movies based on novel imdb.csv",
                    delim = "\t", 
                   col_names = c("Title", "Title2","Rating"),
                   col_types = cols(
                     Title=col_character(),
                     Title2 = col_character(),
                     Rating = col_double()
                     
                   ))



imdb_int2 <- imdb_int %>% mutate(
  type = "Novel")



#warcraft, Marie Antoinette, based on a true story (based on a french novel)  needs to come out
# harry potter and the deathly hallows parts 1 and 2 need to be simplified
# same with Twilight Saga: Beaking dawn parts 1 and 2
# t2 trainspotting could be either trainspotting or porno for book


# add in titles from series

series_int <- read_csv("./data/CSV/book_series.csv") %>% mutate (
  type = "Novel",
  Title2 = Title, 
  year = NA, 
  Rating = NA, 
  rank= NA
  ) %>% select(-notes, -series)
series_movies <- series_int[!is.na(series_int$Title),]
series_books <- series_int[!is.na(series_int$TitleBook),]

# add in alternative book titles
# these will have to be added in ie can read either lion witch and wardrobe or chronicles of narnia
alternative <- read_csv("./data/CSV/alternative_titles.csv") %>% mutate (
  type = "Novel",
  Title2 = Title, 
  Rating = NA, 
  rank = NA
) 

# read in 1:1 substitions from a data set
subs <- read_csv("./data/CSV/one_to_one_subs.csv")

i_text <- " "
i_loop <- function(){
  for(i in 1:nrow(subs)){
    i_text[i] <- sprintf(paste0( 'ifelse(str_trim(str_to_lower(imdb_int2$Title)) == "', 
                                 str_trim(str_to_lower(subs$Title[i])),'" , "',
                                 str_trim(str_to_lower(subs$TitleBook[i])),'",'))
    
  }
  
  i_text[nrow(subs)+1] <- paste0("imdb_int2$Title")
  return(i_text)
}

ie <- function(x) {
  
  args <- x
  #View(args)
  for (i in 1:(length(args) - 1) ) {
    if (substr(args[[i]], 1, 6) != "ifelse") {
      stop("All but the last argument, need to be i functions.", call. = FALSE)
    }
  }
  if (substr(args[[length(args)]], 1, 6) == "ifelse"){
    stop("Last argument needs to be an e function.", call. = FALSE)
  }
  args$final <- paste(rep(')', length(args) - 1), collapse = '')
  eval_string <- do.call('paste', args)
#  View(eval_string)
  eval(parse(text = eval_string))
}

imdb_int3 <- imdb_int2 %>% mutate(
  rank  = as.numeric(gsub("([0-9]+).*$", "\\1", Title2)),
  year = as.numeric(str_sub(Title2, -5,-2)),
  #TitleBook = ifelse(str_trim(Title) == "1922", "Full dark, no stars", NA)
   TitleBook = ie(
     i_loop()
   )
  
)



# since series has some missingness for book and movies (when not 1:1), one list has complete movies
# other list has complete books

# has some duplicates, can simplify by simpleTitle for use with tmdb

imdb_movies <- rbind(imdb_int3, graphic, alternative, series_movies) %>% mutate(
  simpleTitleMovie = str_trim(str_to_lower(Title)),
  simpleTitleBook = str_trim(str_to_lower(TitleBook))
)

# imdb_books will have some duplicates but that's okay because we will be using this to get the book titles

imdb_books <- rbind(imdb_int3, graphic, alternative, series_books) %>% mutate(
  simpleTitleBook = str_trim(str_to_lower(TitleBook)),
  simpleTitleMovie = str_trim(str_to_lower(Title))
)

