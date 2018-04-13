library(rvest)
library(tidyverse)
library(stringr)
library(janitor)


# scrape stock data frome http://www.1stock1.com/

scraper <- function(url, name, table_name){
    
    data <- read_html(url) %>% 
        html_nodes(table_name) %>%
        html_table() %>% as.data.frame()
    
    colnames(data) <- data[1, ]
    data <- data[-1, ]
    
    last_col_ind <- dim(data)[2]
    data[, last_col_ind] <- str_replace(data[, last_col_ind], '%', '')
    
    data <- data %>% 
        as_tibble() %>% 
        mutate_all(as.numeric) %>% 
        clean_names()
    
    colnames(data) <-  c('year', str_c(name, '_', colnames(data)[2:length(colnames(data))]))
    
    data
}


# SP 500 ------------------------------------------------------------------

name <- 'SP500'
url <- 'http://www.1stock1.com/1stock1_141.htm'
table_name <- '.F969 table'

sp_data <- scraper(url, name, table_name)
write_csv(sp_data, 'SP500.csv')


# DOW ---------------------------------------------------------------------

name <- 'Dow_Jones'
url <- 'http://www.1stock1.com/1stock1_139.htm'
table_name <- '.F955 table'

dow_data <- scraper(url, name, table_name)
write_csv(dow_data, 'DowJones.csv')



# NASDAQ ------------------------------------------------------------------


name <- 'NASDAQ'
url <- 'http://www.1stock1.com/1stock1_140.htm'
table_name <- '.F962 table'

NASDAQ_data <- scraper(url, name, table_name)
write_csv(NASDAQ_data, 'NASDAQ.csv')
