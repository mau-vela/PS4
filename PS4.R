library(rvest) 
library(XML)
library(gridExtra)
library(ggplot2)
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
colnames(table)[3] <- "president"
colnames(table)[9] <- "runner_up"
colnames(table)[10] <- "party_runner_up"

#remove first two rows (only with names)
table <- table[-c(1,2),]

#fix margin variables

html <- wikiURL %>%  
  read_html()

table$percmargin <-  html %>%
  html_nodes(xpath = '//table[2]') %>%       # get table 2
  html_nodes(xpath = '//td[6]/text()')    # get column 6 using text()

table$votemargin <-  html %>%
  html_nodes(xpath = '//table[2]') %>%       # get table 2
  html_nodes(xpath = '//td[8]/text()')   # get column 8 using text()


#put as numeric 
putnumeric <- function(dat, want) {
  dat[, want] <- sapply(dat[, want], function(x) gsub("â^'", "-", x))
  dat[, want] <- sapply(dat[, want], function(x) gsub("%", "", x))
  dat[, want] <- sapply(dat[, want], function(x) gsub(",", "", x))
  dat[, want] <- sapply(dat[, want], as.numeric)
  dat
}
table <- putnumeric(table, c(5:8,11))

#fix president names
table$president <-  html %>%
  html_nodes(xpath = '//table[2]') %>% # get table 2
  html_nodes(xpath = '//td[3]/a/text()') %>%  # get column 3 using text()
  html_text()

#Fix runner up
table$runner_up <-  html %>%
  html_nodes(xpath = '//table[2]') %>% # get table 2
  html_nodes(xpath = '//td[9]//a//text()') %>%  # get column 9 using text()
  html_text()

#Order by year
table <- table[order(table$Year), ]

#Plot
#To define colors
table$color <- ifelse(table$Party=="Dem.", "blue", ifelse(table$Party=="Rep.", "red", ifelse(table$Party=="Whig", "yellow", "green")))

#plot % vote for winner and color according to the party
plot1 <- ggplot(table, aes(x=Year, y=`Popular vote (%)`)) +
  labs(title="Presidential Winner", x="Year", y="Popular vote %") +
  geom_line(aes(colour=color, group=1)) + 
  scale_colour_identity() 

#the same plot but using turnout
plot2 <- ggplot(table, aes(x=Year, y=Turnout)) +
  labs(title="Turnout", x="Year", y="Turnout %") +
  geom_line(aes(colour=color, group=1)) + 
  scale_colour_identity(guide="legend", labels=c("Dem", "Dem-Rep", "Rep", "Whig")) 

#Put two figures together and save to pdf
pdf("R/Clase/PS4/Plots.pdf", height=15, width=15)
grid.arrange(plot1, plot2, ncol=2)


#Get data from electoral college votes

wikiURL <- 'https://en.wikipedia.org/wiki/United_States_presidential_election'

#Create table from wikipedia
table2 <- wikiURL %>% 
  read_html %>% 
  html_nodes("table") %>% 
  .[[3]] %>% 
  html_table()

#take out some symbols
table2[,] <- sapply(table2[, ], function(x) gsub("â???"", "-", x))  #minus sign
table2[,] <- sapply(table2[, ], function(x) gsub("[*]+","", x)) #asterisk
table2[,] <- sapply(table2[, ], function(x) gsub("*â???", "", x))  #other


#Get votes for winner
#Find regular expression for number after "-"
m <- regexpr(".\\s?[0-9]+", table2$Winner)
#Create variable with votes from electoral college
table2$votes_winner <- regmatches(table2$Winner, m)

#Get votes for runner-up
#Find regular expression for number after "-"
m <- regexpr("(.\\s?[0-9]+)", table2$`Other major candidates[27]`) 
#Create variable with votes from electoral college
table2$votes_runner_up <- regmatches(table2$`Other major candidates[27]`, m)

#convert to numeric
table2[,c(5,6)] <- sapply(table2[,c(5,6)], function(x) as.numeric(trimws(gsub("-", "", x))))

#Create Year
table2$Year <- as.numeric(substring(table2$`Election year`, 1, 4))
#Keep only electoral college votes and year
table2 <- table2[,c(5:7)]

#Merge table with table2 
finaltable <- merge(table, table2, by="Year")
