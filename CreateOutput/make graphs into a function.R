
# make graphs into a function 

setwd("/home/yichijin/Desktop/Diana/DataIncubator/")
#source("read all data.R")

# test loop ---------------------------------------------------------------
#### dates will have to be automated, but otherwise the string works

make_graph <- function(movie){
  
  book <- checkedout_summary %>% filter( simpleTitle == str_trim(str_to_lower((movie))))
  book_copies <- catalog_copies %>% filter (simpleTitle == str_trim(str_to_lower((movie))))
  book_copies_num <- sum(book_copies$copies)
  
  # 5 copies of brideshead revisited, released August 2008
  graph <-  ggplot(data = book)+
    geom_col(mapping = aes(x=time, y=checkout_sum), position = "identity") + 
    labs(y = "# of checkouts per month", title = str_to_title(movie)) + 
    geom_hline(yintercept = book_copies_num) +
    geom_vline(xintercept = as.numeric(as.Date("2008-08-01")) , color = "blue" , linetype = 4) +
    scale_x_date(  limits = as.Date(c('2006-08-01','2010-08-01')))
  
 
  return(graph)
 
}

make_graph("brideshead revisited")
