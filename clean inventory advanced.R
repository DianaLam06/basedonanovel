
setwd("/home/yichijin/Desktop/Diana/DataIncubator/library/")

source("read in movies based on novel imdb.R")

full_inventory <- read_csv("./data/CSV/full_inventory_2018_01_01.csv")
full_inventory <- full_inventory %>% mutate(
  cleanTitle = vapply(str_split(Title, "/"), '[',1, FUN.VALUE = character(1)),
  simpleTitle = str_trim(str_to_lower(cleanTitle))
)
\

#will need to go through this and clean up multiples with the same title
#can you find out whether it's in english ?

# doesn't like the apostraphe in alice's adventures in wonderland


catalog_copies_with_dup <- full_inventory  %>% filter(
  (simpleTitle %in% imdb_books$simpleTitleBook) |
    ( str_detect(simpleTitle , "adventures in wonderland")) |
    (str_detect(simpleTitle,  "good people : a novel")) |
  (simpleTitle %in% str_c(imdb_books$simpleTitleBook, " : a novel")) |
   (simpleTitle %in% str_c("the ", imdb_books$simpleTitleBook))
) %>%arrange(simpleTitle)



# bibnum will indicate same title, but different editions of the book might be under different bibnum

#this is to find different versions of the same title
catalog_copies_count <-  catalog_copies_with_dup %>%
  group_by( simpleTitle, Author) %>% summarize(
    count = n()
  ) %>% group_by(simpleTitle) %>% summarize(
    count_title = n()
  )


diff_titles_multiple_authors <-  catalog_copies_with_dup %>%
  group_by( simpleTitle, Author) %>% summarize(
    num_copies = sum(ItemCount)
  ) %>% group_by(simpleTitle) %>% summarize(
    count_title = n()
  ) %>% filter (count_title>1)

diff_authors_count <-  catalog_copies_with_dup %>%
  group_by( simpleTitle, Author) %>% summarize(
    num_copies = sum(ItemCount)
  ) %>% filter(
    simpleTitle %in% diff_titles_multiple_authors$simpleTitle
  )

# artificially inflate the copies that actually have less

# make data set with the title/author combos that have the most
diff_authors_max <- diff_authors_count %>% group_by (simpleTitle) %>%
  arrange(simpleTitle, desc(num_copies)) %>%
  slice(1)

