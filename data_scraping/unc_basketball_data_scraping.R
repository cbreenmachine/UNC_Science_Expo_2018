# UNC basketball only scraping
library(rvest)
library(tidyverse)
library(stringr)
library(readr)

# URL to scrape
url <- "https://www.sports-reference.com/cbb/schools/north-carolina/"
unc_data = read_html(url)

# Overall wins per season
wins <- unc_data %>%
  html_nodes(".left+ .right") %>%
  html_text() %>%
  as.numeric()
wins <- rev(wins)
wins <- wins[2:length(wins)]

# Win percent
win_percent <- unc_data %>%
  html_nodes(".right:nth-child(6)") %>%
  html_text() %>%
  as.numeric()
win_percent <- rev(win_percent)

# Conference wins
conference_wins <- unc_data %>%
  html_nodes(".right:nth-child(7)") %>%
  html_text() %>%
  as.numeric()
conference_wins <- rev(conference_wins)

# Conference win percent
conference_win_percent <- unc_data %>%
  html_nodes(".right:nth-child(9)") %>%
  html_text() %>%
  as.numeric()
conference_win_percent <- rev(conference_win_percent)

# PPG
ppg <- unc_data %>%
    html_nodes(".right:nth-child(12)") %>%
    html_text() %>%
    as.numeric() 
ppg <- rev(ppg)

# Opponent PPG
opponent_ppg <- unc_data %>%
  html_nodes(".right:nth-child(13)") %>%
  html_text() %>%
  as.numeric() 
opponent_ppg <- rev(opponent_ppg)


# Pre season ranking
pre_season_rank <- unc_data %>%
  html_nodes(".right+ .center") %>%
  html_text() %>%
  as.numeric()
pre_season_rank <- rev(pre_season_rank)


# Final ranking
final_rank <- unc_data %>%
  html_nodes(".valign_top .center~ .center+ .center") %>%
  html_text() %>%
  as.numeric() 
final_rank <- rev(final_rank)


# Highest ranking
highest_rank <- unc_data %>%
  html_nodes(".valign_top .center:nth-child(15)") %>%
  html_text() %>%
  as.numeric() 
highest_rank <- rev(highest_rank)


# Year sequence
year <- seq(from=1911, to=2018, by=1)


# Final data frame to export
unc_bb <- data.frame(year, wins, win_percent, conference_wins, conference_win_percent,
                     ppg, opponent_ppg, pre_season_rank, highest_rank, final_rank)

# Show the data frame
head(unc_bb)
tail(unc_bb)

# Write the file
write.csv(unc_bb, file = "unc_bb.csv")
