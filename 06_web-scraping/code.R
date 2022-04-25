library(rvest)
library(tidyverse)
library(lubridate)

url <- "https://www.the-numbers.com/box-office-chart/daily/2022/03/20"
html <- read_html(url)
tables <- html %>% html_table(fill=TRUE)

length(tables)

A = tables[[2]]
names(A)[1 : 2] = c("Rank", "Rank.Last.Week")
str(A)

A = A %>% mutate(TotalGross = parse_number(TotalGross),
                 PerTheater = parse_number(PerTheater),
                 Theaters = parse_number(Theaters),
                 Gross = parse_number(Gross))

url0 = "https://www.the-numbers.com/box-office-chart/daily/"

url.date = "2022/03/20"

paste0(url0, url.date)


date0 = "2022/01/01"
data0.time = ymd(date0)
Result = c()
for (i in 1 : 10){
  data.temp.time = data0.time + i
  data.temp = as.character(data.temp.time)
  data.temp.final = gsub("-", "/", data.temp)
  link = paste0(url0, data.temp.final)
  #print(link)
  
  html = read_html(link)
  tables = html %>% html_table(fill=TRUE)
  A = tables[[2]]
  names(A)[1 : 2] = c("Rank", "Rank.Last.Week")
  A = A %>% mutate(TotalGross = parse_number(TotalGross),
                   PerTheater = parse_number(PerTheater),
                   Theaters = parse_number(Theaters),
                   Gross = parse_number(Gross),
                   date = data.temp.time)
  Result = rbind(Result, A)
}


Result1 = Result %>% filter(Theaters > 300)
sum(is.na(Result1$`Movie Title`))

Result2 = Result1 %>% group_by(`Movie Title`) %>% 
  summarise(Max_Gross = max(TotalGross))

Result1 %>% ggplot(aes(x = Date, 
                       y = log(TotalGross), 
                       group = `Movie Title`)) + 
  geom_line() + 
  geom_text(data = Result2, 
            aes(x = ymd("2022/01/20"), 
                y = log(Max_Gross), 
                label = `Movie Title`))