### Spurious Correlations with UNC Basketball Data
### In the style of Tyler Vigen's book "Spurious Correlations," this is a script 
### for collecting, cleaning, and visualizing data to hopefully find some false correlations
### of our own. This project is being done for the UNC-Chapel Hill Science Expo on April 14, 2018

# library for webscraping
library(rvest)
library(tidyverse)
library(stringr)
library(ggplot2)
library(readr)

setwd('C:/Users/cbreen/Documents/R/Spurious Correlations UNC Basketball')


################################## SCRAPE UNC BASKETBALL WINS ################################## 

# Scrape data from an online source containing UNC annual basketball statistics
unc_basketball_url <- "https://www.sports-reference.com/cbb/schools/north-carolina/"
unc_data = read_html(unc_basketball_url)

# Overall win percentage per season, where the season is paired with the second year
# e.g. the 2017-2018 season is paired with "2018-01-10"
win_percent <- unc_data %>%
  html_nodes(".right:nth-child(6)") %>%
  html_text() %>%
  as.numeric()
win_percent <- rev(win_percent)

# Overall wins per season
raw_wins <- unc_data %>%
  html_nodes(".left+ .right") %>%
  html_text() %>%
  as.numeric()
raw_wins <- rev(raw_wins)[-1]

# Conference win percentage
conference_win_percent <- unc_data %>%
  html_nodes(".right:nth-child(9)") %>%
  html_text() %>%
  as.numeric()
conference_win_percent <- rev(win_percent)

# Manually set years data because there is no easy scraping for this
years <-seq(as.Date("1911/1/1"), as.Date("2018/1/1"), "years")


df <- data.frame(years, raw_wins, win_percent, conference_win_percent)

library(ggplot2)
# Basic line plot with points
ggplot(data=df, aes(x=years, y=win_percent, group=1)) +
  geom_line()+
  geom_point()

################################## SCRAPE ANNUAL DEFICIT AS A PERCENT OF GDP ################################## 


# Deficit data courtesy of https://fred.stlouisfed.org/series/FYFSGDA188S
# BE sure to have the correct folder with all of the excel files 
deficit_data <- read_csv("FYFSGDA188S.csv", 
                col_types = cols(DATE = col_date(format = "%m/%d/%Y")))
deficit_years  <- deficit_data[,1]
deficit_years  <- do.call(c, deficit_years)
annual_deficit <- deficit_data[,2]
annual_deficit <-  unlist(annual_deficit, use.names=FALSE)
head(annual_deficit)

# Make the dimensions match by only taking
rowEqualizer <- function(x1, x2){
  number_rows_1 = NROW(x1)
  number_rows_2 = NROW(x2)
  
  left_bound = -1
  right_bound = -1*abs(NROW(x1) - NROW(x2))
  
  # Prune out the last entries (thought of here as the oldest data values (e.g. 1920))
  if (NROW(x1) < NROW(x2)){
    x2 <- x2[left_bound:right_bound]
  } else if (NROW(x2) < NROW(x1)){
    x1 <- x1[left_bound:right_bound]
  }
  
  # Create a data frame to return to user for graphing
  df <- data.frame(x1, x2)
  return(df)
  
}

# Equalize the data 
deficit_win_percent_df <- rowEqualizer(win_percent, annual_deficit)
# Equalize the years
deficit_win_percent_years <- rowEqualizer(years, deficit_years)[2]
# Combine data and years into one dataframe
df <- data.frame(deficit_win_percent_years, deficit_win_percent_df)


# Annual church membership rate of change (LDS)
# mormon_url <- "https://en.wikipedia.org/wiki/The_Church_of_Jesus_Christ_of_Latter-day_Saints_membership_history#Table_for_recent_growth"
# mormon_data <- read_html(mormon_url) %>%
# 
# mormon_roc <- mormon_data %>%
#   html_nodes("p+ .jquery-tablesorter td:nth-child(2)") %>%
#   html_text() %>%
#   as.numeric()
# head(mormon_roc)
# 
# # NC Population
# nc_pop_url <- "https://en.wikipedia.org/wiki/Demographics_of_North_Carolina"
# nc_pop_data <- read_html(nc_pop_url) %>%
#   html_nodes(".toccolours td~ td+ td") %>%
#   html_text() %>%
#   as.numeric() %>%
# head(nc_pop_data)

# IBM Stock price
# This data set contains the date, open, high, low, close and volume (int that order)
# of IBM stock (measured daily) since 1962

library(readr)
IBM <- read_csv("MacroTrends_Data_Download_IBM.csv", 
                                          col_types = cols(date = col_datetime(format = "%m/%d/%Y")), 
                                          skip = 10)
IBM_dates <- do.call(c, IBM[,1])
IBM_open  <- unlist(IBM[,2], use.names=FALSE)
IBM_high  <- unlist(IBM[,3], use.names=FALSE)
IBM_low   <- unlist(IBM[,4], use.names=FALSE)
IBM_close <- unlist(IBM[,5], use.names=FALSE)
IBM_ratio <- (IBM_low / IBM_close)

IBM_df <- data.frame(IBM_dates, IBM_open, IBM_high, IBM_low, IBM_close, IBM_ratio)
ggplot(data=IBM_df, aes(x=IBM_dates, y=IBM_ratio, group=1)) +
  geom_line()+
  geom_point()

plot( years, win_percent, type="l", col="red")
par(new=TRUE)
plot(IBM_dates, IBM_ratio, type="l", col="green")


# Next step: take the mean per year or take the minimum stock price or something like that
# may need to convert to string in order to make yearly calculations
# See: toString()
# See: substring(x, a, b)

# To avoid scaling issues, take min/max

# TimeWarner Stock Price
TWX <- read_csv("MacroTrends_Data_Download_TWX.csv", 
       col_types = cols(date = col_datetime(format = "%m/%d/%Y")), 
       skip = 10)
TWX_dates <- do.call(c, TWX[,1])
TWX_open  <- unlist(TWX[,2], use.names=FALSE)
TWX_high  <- unlist(TWX[,3], use.names=FALSE)
TWX_low   <- unlist(TWX[,4], use.names=FALSE)
TWX_close <- unlist(TWX[,5], use.names=FALSE)
TWX_ratio <- (TWX_low / TWX_close)

TWX_df <- data.frame(TWX_dates, TWX_ratio)
ggplot(data=TWX_df, aes(x=TWX_dates, y=TWX_ratio, group=1)) +
  geom_line()+
  geom_point()


# 

# Deaths by different things
# Terrorism data
# Military spending
# Google searches for Iraq or other things
# Tech stock prices
# Wins of other teams, like Duke Basketball
# Make a function that will take in two 
