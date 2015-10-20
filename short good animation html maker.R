## This script will build an HTML  page of the best short animations per year

library(ggplot2)
library(httr)
library(XML)
library(dplyr)
library(tidyr)
library(stringr)
library(stringdist)

## Summarize ggplot2::movies to only animations 10 minutes or less
## then arrange by year and descending rating, and then select only
## 1 best animation per year. Only keep the title and year.
ggplot2::movies %>% 
  filter(Animation==1, length<=10) %>% 
  arrange(year,desc(rating)) %>% 
  select(title,year,rating) %>% 
  group_by(year) %>% top_n(3) -> a
  # filter(rating == max(rating)) -> a

a$forVideo <- paste('Cartoon "',
                    a$title,'" (',a$year,')',sep="")
a$surch <- paste(a$title,'</strong> (<strong>',a$year,'</strong>)',sep="")

## OK, now feed each of these 'forVideo' strings into Bing to get
## the search results

## dim the list that will get the bing results
# hits <- as.list(rep("0",length(L)))
hits <- data.frame(SearchTitle=0,Link=0,Title=0,dist=0)

## Sometimes this loop breaks for an unknown reason. If it does, just get value of i
## and replace "1" in for with current value of i.
for (i in 1:length(a$forVideo)){
  d <- GET("http://www.bing.com/", 
           path = "search", 
           query = list(q = a$forVideo[i]),as="text")
  e <- content(d)
  
  ## OK, I have a problem... in theory, I should be xpathing this: //div[@class='b_title']
  ## but for some strange reason it doesn't capture all the links... so go one element above
  ## and then clean more. :-\
  
  stuff <- xpathSApply(e,"//li[@class='b_algo']",saveXML)
  # stuff <- gsub('.+"http://','',stuff)
  
  data.frame(Link = gsub(".+www\\.|.+//","",gsub('h=\\\".+','',stuff)),
             Title = gsub('.+>','',gsub('</strong.+','',stuff))) %>%
    filter(grepl("youtube.com",Link)) %>% head(1) -> winner
  
  hits[i,1] <- a$title[i]
  hits[i,2] <- as.character(winner[1,1])
  hits[i,3] <- as.character(winner[1,2])
  hits[i,4] <- stringdist(tolower(hits[i,1]),tolower(hits[i,3]),method="qgram")
  cat(paste(i,";",sep="")) ## for long waits
}

hits$Year <- a$year

hits %>%
  filter(!is.na(Link)) %>% 
  mutate(Per=dist/nchar(SearchTitle)*100) %>% 
  filter(dist<=8,Per<100) %>% 
#   View
#   select(Year,SearchTitle,dist,Per) %>%
#   arrange(Year) %>%
  
  select(Year,SearchTitle,Link,Per) %>%
  arrange(Year) %>%
  mutate(code=paste(Year,
                    ' - <a target="_blank" href="http://',
                    Link,
                    '">',
                    SearchTitle,
                    '</a><br><br>',sep="")) %>%
select(code)-> code


cat("<html><body><br><h1>Cool short movies</h1><p>",
    code[,1],
    "</p></body></html>",
    file="Try 3.html")


## try 1 used: paste('Cartoon "',a$title,'" (',a$year,')',sep="")
## try 2 used better xpath
## Try 3 Try with 3 top movies for each year, but only go for perfect hits
##   which turned out to be stringdist <8 and Perc <100









