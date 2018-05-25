
#process library inventory data
# number of lines in Library iventory = 10765336
# only need books with ReportDate == "01/01/2018", otherwise double counting
# m needs to be 1076
catalog_int0<- read_csv("./data/CSV/Library_Collection_Inventory.csv",
                        skip = 0, n_max = 10000, col_names = TRUE)

header_inv <- colnames(catalog_int0)

read_inventory <-  function(i){
  print(paste0("currently printing ",i))
  
  temp <- read_csv("./data/CSV/Library_Collection_Inventory.csv",
                   skip = 10000*i+1, n_max = 10000, col_names = header_inv,
                   col_types = cols(
                     BibNum = col_integer(),
                     Title = col_character(),
                     Author = col_character(),
                     ISBN = col_character(),
                     PublicationYear = col_character(),
                     Publisher = col_character(),
                     Subjects = col_character(),
                     ItemType = col_character(),
                     ItemCollection = col_character(),
                     FloatingItem = col_character(),
                     ItemLocation = col_character(),
                     ReportDate = col_character(),
                     ItemCount = col_integer()
                   ))
  
  
  inventory <-  temp %>%
    filter(
      as.numeric(str_sub(ItemType, -2,-1) %in% c( "bk","nh") ) == 1,
      ReportDate == "01/01/2018"
    )
  return(inventory)
  
}



beta <-  catalog_int0 %>%
  filter(
    as.numeric(str_sub(ItemType, -2,-1) %in% c( "bk","nh") ) == 1, 
    ReportDate == "01/01/2018"
  )


m <- 1076
#m <- 2

testinventory <- beta
for (i in 1:m){
  
  temp2 <- read_inventory(i)
  
  testinventory <- rbind(testinventory, temp2)
}

full_inventory <- testinventory
write_csv(full_inventory, "./data/CSV/full_inventory_2018_01_01.csv")
#need to clean up duplicates

#to find the number of copies, you need to sum ItemCount over BibNum

# 
# catalog_copies <-  beta  %>%   
#   group_by( BibNum, simpleTitle) %>% 
#   summarize(
#     copies = sum(ItemCount)
#   )
# 
# #will need to go through this and clean up multiples with the same title
# #can you find out whether it's in english ?
# 
# #why are there so many copies of coraline? one's a graphic novel
# 
# coraline <- beta %>% filter (simpleTitle == "coraline")


#unique(catalog_int$ItemType)

#what is arper? research periodicals
#what is pkbknh?  May be books
#arper <- catalog_int %>% filter(ItemType == "arper")
#pkbknh <- catalog_int %>% filter(ItemType == "pkbknh")
