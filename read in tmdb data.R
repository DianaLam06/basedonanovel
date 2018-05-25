#setwd("/home/yichijin/Desktop/Diana/DataIncubator/library")

source("read in movies based on novel imdb.R")
library(dplyr)
library(stringr)
library(jsonlite)
library(httr)

#read in all possible tmdb movies;

tmdb <- stream_in(file("movie_ids_12_31_2017.json",open="r")) 

tmdb_restricted <- tmdb%>% filter(
  str_trim(str_to_lower(original_title)) %in% str_trim(str_to_lower(imdb$Title )),
  adult == FALSE
)

write_csv(tmdb_restricted, "tmdb_list.csv")
