# Test environment for app


# Read in data ------------------------------------------------------------
unc_bb <- read.csv('unc_bb.csv')
other_data <- read.csv('other_data.csv')


# Create data frames and join them ----------------------------------------

# --> Create a data frame with just selected input for unc data and year
year <- unc_bb[,'year']
y1 <- unc_bb[,'wins']

unc_df <- data.frame(year, y1)
head(unc_df)

# --> Create a second data frame with just the selected input for the other and its year
year <- other_data[,'year']
y2 <- other_data[, 'duke_final_rank']

other_df <- data.frame(year, y2)
head(other_df)

# --> Merge the two data frames by year and sort into descending order
both_df <- merge(unc_df, other_df, by='year') # Good job of joining and merging the dfs

head(both_df)
tail(both_df)


# Take out rows with NA ---------------------------------------------------

cull_rows <- vector()
for (i in 1:NROW(both_df)){
    if ((is.na(both_df[i,2])) | (is.na(both_df[i,3]))) {
        cull_rows <- c(cull_rows, i)
    }
}

both_df <- both_df[-cull_rows,]

head(both_df)
tail(both_df)
