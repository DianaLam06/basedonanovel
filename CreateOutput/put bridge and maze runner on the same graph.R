
library(ggplot2)
# the maze runner -----------------------------------------------------



make_graph <- function(movie){
  
  book <- checkedout_summary %>% filter( simpleTitle == str_trim(str_to_lower((movie))))
  book_copies <- catalog_copies %>% filter (simpleTitle == str_trim(str_to_lower((movie))))
  book_copies_num <- sum(book_copies$copies)
  
  # released  19 September 2014
  maze <-  ggplot(data = book)+
    geom_col(mapping = aes(x=time, y=checkout_sum), position = "identity") + 
    labs(y = "# of checkouts per month", title = str_to_title(movie), x= " ") + 
    geom_hline(aes( color = "# of copies", yintercept = book_copies_num ) ) +
    geom_vline(aes( color = "Release date" , xintercept = as.numeric(as.Date("2014-09-19"))) ) +
    scale_x_date(  limits = as.Date(c('2012-09-01','2016-09-01')))+
    scale_color_manual( name = "Reference line",  values = c("palevioletred2","lightseagreen"))
  

  return(graph)
  
  
}

make_graph("the maze runner")


# Bridge to Terabithia example --------------------------------------------



make_graph <- function(movie){
  
  book <- checkedout_summary %>% filter( simpleTitle == str_trim(str_to_lower((movie))))
  book_copies <- catalog_copies %>% filter (simpleTitle == str_trim(str_to_lower((movie))))
  book_copies_num <- sum(book_copies$copies)
  
  # released  19 September 2014
  bridge <-  ggplot(data = book)+
    geom_col(mapping = aes(x=time, y=checkout_sum), position = "identity") + 
    labs(y = "# of checkouts per month", title = str_to_title(movie), x= " ") + 
    geom_hline(aes( color = "# of copies", yintercept = book_copies_num ) ) +
    geom_vline(aes(xintercept = as.numeric(as.Date("2007-02-01")), color = "Release date" )) +
    scale_x_date(  limits = as.Date(c('2006-02-01','2008-02-01'))) +
    scale_color_manual( name = "Reference line",  values = c("palevioletred2","lightseagreen"))
  
  
  return(bridge)
  
  
}

make_graph("bridge to terabithia")

make_graph <- function(movie){
  
  book <- checkedout_summary %>% filter( simpleTitle == str_trim(str_to_lower((movie))))
  book_copies <- catalog_copies %>% filter (simpleTitle == str_trim(str_to_lower((movie))))
  book_copies_num <- sum(book_copies$copies)
  
  # released Jan 11 2008
  atonement <-  ggplot(data = book)+
    geom_col(mapping = aes(x=time, y=checkout_sum), position = "identity") + 
    labs(y = "# of checkouts per month", title = str_to_title(movie)) + 
    geom_hline(aes( color = "# of copies", yintercept = book_copies_num ) ) +
    geom_vline(aes(xintercept = as.numeric(as.Date("2008-01-11")), color = "Release date" )) +
    scale_x_date(  limits = as.Date(c('2007-01-01','2009-01-01'))) +
    scale_color_manual( name = "Reference line",  values = c("palevioletred2","lightseagreen"))
  
  
  return(atonement)
  
}

make_graph("atonement")

par(mfrow=c(2,1))
make_graph("atonement")
make_graph("atonement")
