library(tidyverse)


old_data <- read_csv('old_data.csv')

dow <- read_csv('DowJones.csv')
nasdaq <- read_csv('NASDAQ.csv')
sp500 <- read_csv('SP500.csv')

unc_bb <- read_csv('unc_bb.csv')
duke_bb <- read_csv('duke_bb.csv')


other_data <- old_data %>% 
            full_join(dow, by='year') %>% 
            full_join(nasdaq, by='year') %>% 
            full_join(sp500, by='year') %>%
            full_join(duke_bb,by='year') %>%
            arrange(desc(year)) 


# Show data to make sure it's logical
View(other_data)

# Write CSV
write.csv(other_data, file = "other_data.csv")
