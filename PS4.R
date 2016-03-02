library(rvest) 
library(XML)
wikiURL <- 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'

## Grab the tables from the page and use the html_table function to extract the tables.
## You need to subset temp to find the data you're interested in (HINT: html_table())



table <- wikiURL %>%  
  read_html %>% 
  html_nodes("table")  %>% 
  .[[2]] %>% 
  html_table(trim=TRUE)


#fix column names 
colnames(table)[6] <- "percmargin"
colnames(table)[8] <- "votemargin"

#remove first two rows (only with names)
table <- table[-c(1,2),]

#fix margin variables
table$percmargin <- gsub(,table$percmargin)