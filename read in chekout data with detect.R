#read in library data;
# borrowing time is 21 days and renewals are NOT counted as a new checkout
#setwd("/home/yichijin/Desktop/Diana/DataIncubator/")


#source("read in movies based on novel imdb.R")
#first, get a list of all movies based on abook (tmdb and imdb)
#by year, go through library data and get the list of titles that match the year
#library(dplyr)
#library(tidyverse)
#library(stringr)

read_books <- function(i){
  start <- 10000*as.numeric(i) +1
  
  print(paste0("Currently reading using n = ", i))
  temp <- read_csv("./data/CSV/Checkouts_by_Title.csv", 
                   skip = start,  n_max = 10000, col_names = header, 
                   col_types = cols(
                     UsageClass = col_character(),
                     CheckoutType = col_character(),
                     MaterialType = col_character(),
                     CheckoutYear = col_integer(),
                     CheckoutMonth = col_integer(),
                     Checkouts = col_integer(),
                     Title = col_character(),
                     Creator = col_character(),
                     Subjects = col_character(),
                     Publisher = col_character(),
                     PublicationYear = col_character()
                   ))
  
  books <- temp %>% mutate(
    cleanTitle = vapply(str_split(Title, "/"), '[',1, FUN.VALUE = character(1))
  ) %>% filter(
    str_detect(str_trim(str_to_lower(cleanTitle)),  str_trim(str_to_lower(imdb_books$TitleBook))),
    MaterialType %in% c("BOOK", "EBOOK")
  )
  
  return(books)
}


# first 10000 -------------------------------------------------------------

#initialize with the first 10000 checkouts which have been filtered
#need first 10000 for header information


spl0 <- read_csv("./data/CSV/Checkouts_by_Title.csv", 
                 skip = 0, n_max = 10000, col_names = TRUE, 
                 col_types = cols(
                   UsageClass = col_character(),
                   CheckoutType = col_character(),
                   MaterialType = col_character(),
                   CheckoutYear = col_integer(),
                   CheckoutMonth = col_integer(),
                   Checkouts = col_integer(),
                   Title = col_character(),
                   Creator = col_character(),
                   Subjects = col_character(),
                   Publisher = col_character(),
                   PublicationYear = col_character()
                 ))
header <- colnames(spl0)

testbooks <- alpha <- spl0 %>% mutate(
  cleanTitle = vapply(str_split(Title, "/"), '[',1, FUN.VALUE = character(1))
) %>% filter(
  str_detect(str_trim(str_to_lower(cleanTitle)),  str_trim(str_to_lower(imdb_books$TitleBook))),
  MaterialType %in% c("BOOK", "EBOOK")
)

for (i in 1:3077) {
  
  temp <- read_books(i)
  testbooks <- rbind(testbooks, temp)
}

full <- testbooks

write_csv(full, "checkedout_more_titles_detect.csv")


