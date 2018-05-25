
# Bridge to Terabithia example --------------------------------------------



  book <- checkedout_summary %>% filter( simpleTitle == str_trim(str_to_lower(("bridge to terabithia"))))
  book_copies <- catalog_copies %>% filter (simpleTitle == str_trim(str_to_lower(("bridge to terabithia"))))
  book_copies_num <- sum(book_copies$copies)
  
  # released  19 September 2014
  bridge <-  ggplot(data = book)+
    geom_col(mapping = aes(x=time, y=checkout_sum), position = "identity") + 
    labs(y = "# of checkouts per month", title = str_to_title("bridge to terabithia"), x= " ") + 
    geom_hline(aes( color = "# of copies", yintercept = book_copies_num ) ) +
    geom_vline(aes(xintercept = as.numeric(as.Date("2007-02-01")), color = "Release date" )) +
    scale_x_date(  limits = as.Date(c('2006-02-01','2008-02-01'))) +
    scale_color_manual( name = "Reference line",  values = c("palevioletred2","lightseagreen"))
  
  
  
  book2 <- checkedout_summary %>% filter( simpleTitle == str_trim(str_to_lower(("atonement"))))
  book2_copies <- catalog_copies %>% filter (simpleTitle == str_trim(str_to_lower(("atonement"))))
  book2_copies_num <- sum(book2_copies$copies)
  
  # released Jan 11 2008
  atonement <-  ggplot(data = book2)+
    geom_col(mapping = aes(x=time, y=checkout_sum), position = "identity") + 
    labs(y = "# of checkouts per month", title = str_to_title("atonement")) + 
    geom_hline(aes( color = "# of copies", yintercept = book2_copies_num ) ) +
    geom_vline(aes(xintercept = as.numeric(as.Date("2008-01-11")), color = "Release date" )) +
    scale_x_date(  limits = as.Date(c('2007-01-01','2009-01-01'))) +
    scale_color_manual( name = "Reference line",  values = c("palevioletred2","lightseagreen"))
  
  atonement
  ggsave("./output/Atonement2.pdf", width = 6)


  pdf("Bridge_Atonement.pdf", width = 6)
multiplot(bridge, atonement, rows=2)
dev.off()