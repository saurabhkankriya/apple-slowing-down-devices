#.libPaths()
devtools::install_github('hadley/ggplot2')

#The essential libraries
install.packages("rvest", dependencies = T)
install.packages("stringr", dependencies = T)
install.packages("lubridate", dependencies = T)
install.packages("zoo", dependencies = T)
install.packages("ggplot2", dependencies = T)
install.packages("plotly", dependencies = T)
library(rvest)
library(stringr)
library(lubridate)
library(zoo)
library(ggplot2)
library(devtools)
library(plotly)

remove.packages("ggplot2")
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('data.table', dependencies = TRUE)

#reading google trends data
google_trends_data <- read.csv("multiTimeline.csv")
View(google_trends_data)
sapply(google_trends_data,class)


#Converting datatype of Month
google_trends_data$YearMonth <- as.yearmon(google_trends_data$Month)
google_trends_data$Month <- as.Date(as.yearmon(google_trends_data$Month))

#Subsetting trends data on the basis of date
google_trends <- subset(google_trends_data, google_trends_data$Month > "2007-01-01")
View(google_trends)

#Changing column name
colnames(google_trends)[2] <- "Hits"

#iPhone wikipedia table scrapping

url <- "https://en.wikipedia.org/wiki/IPhone"

iphone_release_table <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>%
  html_table(fill = T)

iphone_release_table <- iphone_release_table[[1]]

head(iphone_release_table)
View(iphone_release_table)

#datatype of columns
sapply(iphone_release_table, class)

#removing last 2 rows
iphone_release_table <- iphone_release_table[1:14,]
View(iphone_release_table)

#Removing ONLY those columns that contain NAs entirely
iphone_release_table <- iphone_release_table[ , ! apply( iphone_release_table , 2 , function(x) all(is.na(x)) ) ]


#Renaming the 'iPhone's first release' column
colnames(iphone_release_table)[3] <- "DateOfRelease"

#Regular expression for extracting date between parenthesis
#Explanation: Replace the matched pattern by the 1st group captured with (.*)
iphone_release_table$new_DateOfRelease <- sapply(iphone_release_table$DateOfRelease,
                                                 function(x) sub(".*\\((.*)\\).*", "\\1",x))



#Changing datatype of date column
class(iphone_release_table$new_DateOfRelease)
iphone_release_table$new_DateOfRelease <- ymd(iphone_release_table$new_DateOfRelease)
View(iphone_release_table)
iphone_release_table$Year <- year(as.Date(iphone_release_table$new_DateOfRelease, '%Y%m%d'))
iphone_release_table$datee <- format(iphone_release_table$new_DateOfRelease, '%d-%m-%Y')


#Storing data in time series form: #Not sure how to proceed. Keep in comments
# google_trends_ts <- as.ts(google_trends)
# iphone_release_table_ts <- as.ts(iphone_release_table)
# 
# plot.ts(google_trends_ts)
# plot.ts(iphone_release_table_ts)

#Scaling dates
(iphone_release_table_date <- iphone_release_table)
(google_trends_date <- google_trends)

iphone_release_table_date$new_DateOfRelease <- iphone_release_table_date$new_DateOfRelease
google_trends_date$Month <- google_trends_date$Month

offset <- min(df2$Date) - min(df1$Date)   # this would make them start at the same place

offset <-  min(iphone_release_table_date$new_DateOfRelease) - min(google_trends_date$Month) 
offset

df2.1 <- df2
iphone_release_table_date$new_DateOfRelease <- iphone_release_table_date$new_DateOfRelease - offset

plot(df1, xlim=range(c(df1$Date,df2.1$Date)),ylim=range(c(df1$Visits,df2$Visits)), type='l',col=2)
lines(df2.1,col=4)
plot(google_trends_date, xlim=range(c(google_trends_date$Month,iphone_release_table_date$new_DateOfRelease)),ylim=range(c(google_trends_date$Hits,iphone_release_table_date$iPhone)), type='l',col=2)
lines(iphone_release_table_date,col=4)
#Plotting
(p <- ggplot() + 
  geom_line(data=google_trends, aes(x=Month, y=Hits), color='red') )
#  geom_line(data=iphone_release_table, aes(x=as.Date(datee), y= as.numeric(iPhone)), color='blue')

(q <- ggplot() + 
  geom_point(data=iphone_release_table, aes(x=datee, y= as.factor(iPhone)), color='blue'))


#Making the plot interactive (tooltip)
(p <- ggplotly(p))

