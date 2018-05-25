setwd("/home/dianalam06/Desktop/DataIncubator/library")
# borrowing time is 21 days and renewals are NOT counted as a new checkout
# presumably can't renew if there's a hold on it

#graphic novels are included in books, but borrowing times might be different

source("read in movies based on novel imdb.R") # reads in libraries
# source("read in checkout data with book not movie titles.R") # creates checkedout_more_titles.csv
# source("read in inventory limiting to report date") # creates full_inventory_2018_01_01.csv
# source("flatten and get JSONS.R") #creates tmdb_movie_details.csv


checkedout_int <- read_csv("./data/CSV/checkedout_more_titles.csv")
full_inventory <- read_csv("./data/CSV/full_inventory_2018_01_01.csv")
full_inventory <- full_inventory %>% mutate(
  cleanTitle = vapply(str_split(Title, "/"), '[',1, FUN.VALUE = character(1)),
  simpleTitle = str_trim(str_to_lower(cleanTitle))
)

tmdb <- read_csv( "./data/CSV/tmdb_movie_details.csv")

tmdb <- tmdb %>% mutate(
  runtime = as.numeric(runtime),
  simple_title = str_trim(str_to_lower(title))
)


tmdb_full <- tmdb %>% filter((runtime>60 ) | is.na(runtime))
#assumes a full length fill is longer than 60 min

# sum checkouts over month
# might want to check regprint in larger data set

book_movie <- read_csv("./data/CSV/book_movie.csv")

# things you need to clean up later ----------------------------------------------------------------------

#find titles with duplicates;

#beauty and the beast by Willard, Nancy, 
#the paperboy (not by pete dexter), 
#twilight (not by Stephenie Meyer) want: Twilight / Stephenie Meyer
# Wetlands whose subject is Experiments, Ecology Experiments,
# Up in the Air subject Experiments
# On the road subject dogs fiction
# scacrifice want Sharon Bolton author not Shanower, Eric
# slience want Shūsaku Endō not Perry, Thomas
# Labor day want Joyce Maynard not Schuh, Mari C.
#are the wrong kind 


# end things you need to clean up -----------------------------------------


checkedout <- checkedout_int %>% mutate(
  simpleTitle =  str_trim(str_to_lower(cleanTitle)) ,
  time = parse_date(str_c(CheckoutMonth,"/",CheckoutYear ), "%m/%Y")
)


checkedout_summary <-  checkedout %>%
  group_by(simpleTitle, CheckoutYear, CheckoutMonth, time) %>% summarize(
    checkout_sum = sum(Checkouts)
  )


# clean up inventory data -------------------------------------------------

#need to clean up duplicates ie Twilight needs to be cleaned up

#to find the number of copies, you need to sum ItemCount over BibNum


#will need to go through this and clean up multiples with the same title
#can you find out whether it's in english ?

#why are there so many copies of coraline? one's a graphic novel
catalog_copies_with_dup <- full_inventory  %>% filter(
  ReportDate == "01/01/2018"
)  %>% arrange(simpleTitle)

catalog_copies <-  full_inventory  %>% filter(
  ReportDate == "01/01/2018"
)  %>%
  group_by( BibNum, simpleTitle) %>% 
  summarize(
    copies = sum(ItemCount)
  )
