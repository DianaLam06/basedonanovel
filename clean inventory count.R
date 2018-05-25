# clean up inventory data -------------------------------------------------

#need to clean up duplicates ie Twilight needs to be cleaned up

#to find the number of copies, you need to sum ItemCount over BibNum

setwd("/home/yichijin/Desktop/Diana/DataIncubator/library/")

source("read in movies based on novel imdb.R")

full_inventory <- read_csv("./data/CSV/full_inventory_2018_01_01.csv")
full_inventory <- full_inventory %>% mutate(
  cleanTitle = vapply(str_split(Title, "/"), '[',1, FUN.VALUE = character(1)),
  simpleTitle = str_trim(str_to_lower(cleanTitle))
)


#will need to go through this and clean up multiples with the same title
#can you find out whether it's in english ?

# doesn't like the apostraphe in alice's adventures in wonderland


catalog_copies_with_dup <- full_inventory  %>% filter(
 (simpleTitle %in% imdb_books$simpleTitleBook) |
   ( str_detect(simpleTitle , c("adventures in wonderland", "good people : a novel"))) |
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

lasky <- full_inventory %>% filter(str_detect(Author, "Lasky, Kathryn"))

# find out how many have more than 2 authors with the same title
diff_authors <- catalog_copies_count %>% filter( count_title >1)

# see if there's an easier way to differentiate, larger item count
sum(is.na(catalog_copies_with_dup$ItemCount))


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

# take out alone based on french graphic, blood ties based on a french novel, broken don't have clay version
# take out , hercules (don't have the specific comic), hostage don't have book, 
# take out the burning don't have book, the capture not released, the death of stalin based on a french novel
# take out (the) sacrifice not released, sarhara don't have book, the fallen can't find, 
# take out the interpreter not based on book
# take out warcraft, Marie Antoinette, based on a true story (based on a french novel) not based on novels
# take out the girl next door don't have book,  the silence (only want silence)
# take out the wedding party, unbroken don't have right book, whiteout,,  don't have right book


# addicted want author Zane but has less
# cell want king but has less
# drive want sallis
# eclipse want meyer
# every secret thing want lippman
# if generic title and Lasky, Kathryn is author then we want it for gahoole series
# the snowman want nesb (really nesbo with special character)
# wetlands want roche
# wilson want clowes

# frankenstein want shelley and may be NA (but that might be movie)


# Good people : a novel didn't enter filter for some reason want sakey
# it want king
# left behind want lahaye
# red want ellis
# the dressmaker want ham
# the ghost want harris
# the headhunters actually want none, want headhunters
# the lion, the witch and the wardrobe want lewis
# the mist, want king
# the paperboy want dexter
# the reader want schlink


# a single man is by the same author but differ due to a comma
# the girl who played by fire also has comma issue with author
# the junglebook comma issue
# the winter's tale comma issue

# the narnia series will need the chronicles of narnia added to # of copies 
# consider taking out beauty and the beast and hercules (don't have the specific comic)
# count_title counts how many books of the same title have different authors
# will need to go through and check how many have more than 1
# still missing a lot of titles;

# get a list of the titles that are in imdb_books ut not in catalog_copies_count

# note that imdb_books has some duplicates

imdb_books_no_dup <- imdb_books %>% mutate(
  simpleTitleBook = str_trim(str_to_lower(TitleBook))
) %>% group_by (simpleTitleBook) %>% summarize( n()) %>% na.omit()
# 618 book titles we need to find in inventory

not_in_inventory <- imdb_books_no_dup %>% filter(
  !(simpleTitleBook %in% catalog_copies_count$simpleTitle)
)

# find titles in full_inventory that are in not_in_inventory

difficult_titles <- full_inventory %>% filter(
  str_detect(simpleTitle, "city of")
  
)


difficult_titles <- full_inventory %>% filter(
  str_detect(simpleTitle, "alice's adventures in wonderland")
  
)

?str_
