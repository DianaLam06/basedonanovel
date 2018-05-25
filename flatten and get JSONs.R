setwd("/home/yichijin/Desktop/Diana/DataIncubator/library")

source("read in movies based on novel imdb.R")
# source("read in tmdb data.R") # creates tmdb_list.csv 

library(jsonlite)
library(httr)
library(purrr)

#read in all possible tmdb movies;

tmdb_restricted <-read_csv("./data/CSV/tmdb_list.csv")


tmdb_list <- tmdb_restricted 
#%>%   filter (title %in%  c("Atonement", "Bridge to Terabithia", "Brideshead Revisited", "Gone Girl"))


# see if you can combine lists by making them flat, and then into tibbles
# genres, keywords, and production_companies need to be flattened

# want to loop flattening over all tmdb_restricted titles

df.names <- " "
for(i in 1: nrow(tmdb_list)){
  df.names[i] <- paste0("movie",i)
}

#rm(list= ls(pattern = "movie"))
#github_api comes from read in tmdb api loop.R


github_api <- function(path) {
  url <- modify_url(str_c("https://api.themoviedb.org/3/movie/", as.character(path),
                          "?api_key=900a489ed1a09a120f925244bffb3f34&language=en-US&append_to_response=details,keywords,release_dates"))
  temp <- GET(url)
  json <- fromJSON(content(temp, "text"), simplifyVector = FALSE)
  return(json)
}

test <- github_api(tmdb_list$id[1]) #run oe to get col_names
col_names <- as.tibble(names(test)) %>% filter(
  !(value %in% c("genres", "keywords", "production_countries", "production_companies", 
                 "backdrop_path","spoken_languages", "belongs_to_collection", "homepage", "release_dates"))
)

# rm(list = ls(pattern = "movie"))

tmdb <- " "

movie_num <-  0



for(i in 1:nrow(tmdb_list)){

  test <- github_api(tmdb_list$id[i])
  
  if(!is.null(test$status_code) ||
  parse_date(test$release_date) < parse_date("2005-01-01") ||
  is.na(parse_date(test$release_date))) next
  
  
  test2 <- unlist(test$genres) %>% as.tibble() 
  test2_1 <- unlist(test$keywords) %>% as.tibble()
  
  if(length(test2) > 0){
  if(row.names(test2)[1] == "1"){
  test3 <- test2 %>% mutate( 
    count   = as.numeric(row.names(test2)),
    numeric = ifelse(count %in% grep("[0-9]", test2$value), "genre_id", "genre_name")
  ) %>% group_by(numeric) %>% summarize(
    genre_value = paste0(value, collapse = ", ")
  ) 
  } else if(row.names(test2)[1] == "id"){
    test3 <- test2 %>% mutate( 
      numeric = ifelse(row.names(test2) == "id", "genre_id", "genre_name")
    ) %>% group_by(numeric) %>% summarize(
      genre_value = paste0(value, collapse = ", ")
    ) 
  }
    test4 <- spread(test3, key = numeric, value = genre_value) %>% mutate(
      title = test$title
    )
  } else{
    
    test4 <- tibble(
      genre_id = " ", genre_name =" ", title = test$title
    )
    
  }

if (length(test2_1) >0 ){
  if(row.names(test2_1)[1] == "1"){
  test3_1 <- test2_1 %>% mutate( 
    count   = as.numeric(row.names(test2_1)),
    numeric = ifelse(count %in% grep("[0-9]", test2_1$value), "keyword_id", "keyword_name")
  ) %>% group_by(numeric) %>% summarize(
    keyword_value = paste0(value, collapse = ", ")
  ) 
  } else if(row.names(test2_1)[1] == "id"){
    test3_1 <- test2_1 %>% mutate( 
      numeric = ifelse(row.names(test2_1) == "id", "keyword_id", "keyword_name")
    ) %>% group_by(numeric) %>% summarize(
      keyword_value = paste0(value, collapse = ", ")
    ) 
  }
  
  test4_1 <- spread(test3_1, key = numeric, value = keyword_value) %>% mutate(
    title = test$title
  )
  
} else {
  
  test4_1 <- tibble(
    keyword_id = " ", keyword_name =" ", title = test$title
  )
 
}

      # production companies
    
    production_names <- " "
    production_ids <- " "
    
   
    
    if (length(test$production_companies) >0){
    for(prod_index in 1 : length(test$production_companies)){
      
      name <- str_trim(str_to_lower(test$production_companies[[prod_index]]$name)) 
      id <- test$production_companies[[prod_index]]$id
      production_names <- str_c(production_names,", ", name)
      production_ids <-  str_c(production_ids,", ", id)
     # print(paste0(production_names, production_ids))
    }
}
  # create data set with the rest of the info
  
     test5 <- test4
   
   for(j in 1:nrow(col_names)){
     value <- paste0("test$",col_names$value[j])
     if(!is.null(eval(parse(text = value)))){
       test5[[col_names$value[j]]] <- eval(parse(text = value))
     } else {
       test5[[col_names$value[j]]] <- " "
     }
   }
   
   
     
     country <- " "
     index <- 1
     while(country != "us"){
       if(index <= length(test$release_dates$results)){
         country <- str_trim(str_to_lower(test$release_dates$results[[index]]$iso_3166_1)) 
         index <- index + 1
       }
       # this is for if a movie was not released in the US
       else if(index >= length(test$release_dates$results) +1) {
         country <- "us"
         index <- 9999
       }
     }
     
     if(index-1 <= length(test$release_dates$results)){
       test5[["certification "]] <- test$release_dates$results[[index-1]]$release_dates[[1]]$certification
     } else{
       test5[["certification "]] <- "no us release"
     }
     
   test5[["production_names"]] <- production_names
   test5[["production_ids"]] <- production_ids
   
  
  # test_all <- join_all(list(test4, test4_1, test5), by = "title" )
  # join_all comes from plyr which is causing spread not to work
   
  test_all <- inner_join(test5, test4_1, by = "title")
  
  tmdb <- rbind(tmdb, test_all)
  movie_num <-  movie_num + 1
  print(paste0("movie", movie_num, " request", i))
  #testit(3)
 # assign(df.names[i], test_all)
}

write_csv(tmdb, "./data/CSV/tmdb_movie_details.csv")

# see if there are any duplicates in tmdb data set

tmdb <- read_csv( "./data/CSV/tmdb_movie_details.csv")
spec(tmdb)

tmdb <- tmdb %>% mutate(
  runtime = as.numeric(runtime),
  simple_title = str_trim(str_to_lower(title))
)

tmdb_dup_list <- tmdb %>% filter((runtime >60) | is.na(runtime)) %>% # want full length feature only 
  group_by (title) %>% summarize(count = n()) %>% filter(count > 1)

tmdb_dup <- tmdb %>% filter (
  title %in% tmdb_dup_list$title,
  (runtime > 60 ) | (is.na(runtime))
  ) %>% arrange( title) %>% mutate(
    based_on_novel = str_detect(keyword_id, "818")
  )

# 818 is based on a novel keyword_id

tmdb_dup2 <- tmdb_dup %>% group_by(title) %>% summarize(based_count = sum(based_on_novel))

tmdb_full <- tmdb %>% filter((runtime>60 ) | is.na(runtime))
#assumes a full length fill is longer than 60 min

# check if graphic novels have their own designation

wanted <- tmdb %>% filter (str_trim(str_to_lower(title)) == "wanted")

wanted$keyword_name

# see if there's any indication that movies are sequels/ part of a series

maze <- tmdb %>% filter (str_detect(simple_title, "maze runner"))

potter <- tmdb %>% filter (str_detect(simple_title, "harry potter"))
