## Shiny App to visualize spurious correlations with UNC basketball
#  This script sets up the user interface for correlating UNC basketball wins/win %
#  with other phenomena. To start, it will have two dropdown menus--one for each
#  data set. Additionally, it will show the R^2 value of a simple linear regression


# Set up environment ------------------------------------------------------

library(shiny)
library(tidyverse)
library(stringr)
library(dplyr)
library(Hmisc)


# Functions ---------------------------------------------------------------

label_cleaner <- function(label){
    label %>% 
        str_replace_all(pattern = '_', replacement = ' ') %>% 
        str_replace_all(pattern = 'unc', replacement = 'UNC') %>%
        str_replace_all(pattern = 'duke', replacement = 'Duke') %>%
        str_replace_all(pattern = 'ibm', replacement = 'IBM') %>%
        str_replace_all(pattern = 'unc', replacement = 'UNC') %>%
        str_replace_all(pattern = 'gdp', replacement = 'GDP') %>%
        str_replace_all(pattern = 'ppg', replacement = 'points per game') %>%
        capitalize()
    
}

# Import Data -------------------------------------------------------------

unc_bb <- read.csv('unc_bb.csv')
other_data <- read.csv('other_data.csv')

# Create option choices for user ------------------------------------------

# Get rid of the first and second columns (index col and year col)
y1_options <- colnames(unc_bb)[-2:-1] 
y2_options <- colnames(other_data)[-2:-1]

# Cheater options
cheat_options <- c("UNC win percentage vs. IBM ratio",
                   "Points scored on UNC vs. Deaths by lightning",
                   "Pre-season rank vs. Deaths by lightning",
                   "Points scored on UNC vs. Duke wins",
                   "Points scored on UNC vs. Honey production",
                   "UNC wins vs. Sour cream consumption")

# User interface ----------------------------------------------------------


ui <- fluidPage(

  title = "Correlations with UNC Basketball Data",
  
  plotOutput(outputId = "time_series", width = "100%"),
  
  hr(),
  
  fluidRow(
    column(width=4,
           h2("Test out your own variables..."),
           selectInput("y1", "Pick a UNC men's basketball variable:", choice=y1_options, selected=y1_options[1]),
           selectInput("y2", "Pick another variable:", choice=y2_options, selected=y2_options[1]),
           sliderInput("year_span", label="Pick a range of years:", min=1905, max=2018, value=c(1950, 2018), sep=""),
           br()
  ),
  
  column(width=4, offset=0,
         h2("Or pick from some of our favorites!"),
         selectInput('cheat', 'Our favorite plots:', cheat_options,selected=NULL)
  ),
  
  column(width=2, offset=0,
         h2("Funding for this project provided by:"),
         h4("National Science Foundation"), 
         h4("UNC Willis-Hanes Fund"),
         h4("The Vicki and David Craver Fund for Faculty Leadership")
    )
        
        
)
)

  
# Server of app -----------------------------------------------------------

server <- function(input, output, session){
    
    # cheater updates
    observe({
        cheat <- input$cheat

        if(cheat == "UNC win percentage vs. IBM ratio"){
            y1_selection <- 'win_percent'
            y2_selection <- 'ibm_ratio'
            year_selection <- c(2000, 2008)
        } else if (cheat == "Points scored on UNC vs. Deaths by lightning"){
            y1_selection <- 'opponent_ppg'
            y2_selection <- 'deaths_by_lightning'
            year_selection <- c(2000, 2007)
        } else if (cheat == "Pre-season rank vs. Deaths by lightning"){
            y1_selection <- 'pre_season_rank'
            y2_selection <- 'deaths_by_lightning'
            year_selection <- c(2000, 2009)
        } else if (cheat == "Points scored on UNC vs. Duke wins"){
            y1_selection <- 'opponent_ppg'
            y2_selection <- 'duke_wins'
            year_selection <- c(1979, 1994)
            # Don't remove this line. Fixes an inexplainable bug
            updateSliderInput(session, "year_span", min=1970, max=2018)
        } else if (cheat == "Points scored on UNC vs. Honey production"){
            y1_selection <- 'opponent_ppg'
            y2_selection <- 'honey_produced_per_colony_in_lbs'
            year_selection <- c(2004, 2008)
        } else if (cheat == "UNC wins vs. Sour cream consumption"){
            y1_selection <- 'wins'
            y2_selection <- 'sour_cream_consumption'
            year_selection <- c(2002, 2009)
        }

        # update the "pick your own variables" selector
        updateSelectInput(session, "y1", selected = y1_selection)
        updateSelectInput(session, "y2", selected = y2_selection)
        updateSliderInput(session, "year_span", value = year_selection)
    })


  output$time_series <- renderPlot({
      

    # --> Create a data frame with just selected input for unc data and year
    year <- unc_bb[,'year']
    y1 <- unc_bb[,input$y1]
    
    new_unc_df <- data.frame(year, y1)

    # --> Create a second data frame with just the selected input for the other and its year
    year <- other_data[,'year']
    y2 <- other_data[,input$y2]
    
    new_other_df <- data.frame(year, y2)

    # --> Merge the two data frames by year and sort into descending order
    both_df <- merge(new_unc_df, new_other_df, by='year')
    
    # --> Take out rows with any NAs

    
    cull_rows <- vector()
    for (i in 1:NROW(both_df)){
        if ((is.na(both_df[i,2])) | (is.na(both_df[i,3]))) {
            cull_rows <- c(cull_rows, i)
        }
    }

    # Take out the rows marked for removal
    if (NROW(cull_rows) > 0){
        both_df <- both_df[-cull_rows,]
    }

    # Extract the time array
    first_t <- both_df[,1]

    # --> Update the year_span slider with the extreme bounds of the 'culled' variables
    
    observe({
        my_min=min(first_t)
        my_max=max(first_t)

        updateSliderInput(session,inputId='year_span', min=my_min, max=my_max)
    })
    
    # --> Run the data through another culling process, this time based on slider input
    
    
    
    beginning_year <- input$year_span[1]
    ending_year <- input$year_span[2]

    cull_rows <- vector()
    for (i in 1:NROW(both_df)){
        if ((both_df[i,1] < beginning_year)
            | (both_df[i,1] > ending_year)){
            cull_rows <- c(cull_rows, i)
        }
    }

    if (NROW(cull_rows)){
        both_df <- both_df[-cull_rows,]
    }
    
    
    t <- both_df[,1]
    y1 <- both_df[,2]
    y2 <- both_df[,3]
        
    correlation <- cor(y1, y2)


    # --> Compute the min and max of the y values for scaling purposes
    minY1 <- min(y1)
    maxY1 <- max(y1)
    
    minY2 <- min(y2)
    maxY2 <- max(y2)
    
    # --> Common properties refactored
    line_width = 2
    c1 <- '#62C6F2'
    c2 <- '#f5243c'
    xsteps <- 10
    ysteps <- 8
    fontSize <- 1.5
    tick_fs <- 1.25
    
    # --> Axes steps
    labelsY1 <- round(seq(minY1, maxY1, length.out = ysteps), digits = 3)
    labelsY2 <- round(seq(minY2, maxY2, length.out = ysteps), digits = 3)
    labelsX  <- seq(min(t), max(t), length.out = 10)

    # --> Plot first variable with time (in years)
    par(mar=c(5.1,5.1,5.1,5.1)) # Bottome, left, top, right based on lines of text
    plot(x=t, y=y1,
         ylim=c(minY1, maxY1),
         col=c1, type='l', main=str_c('Correlation ', format(correlation, digits=3)),
         xlab='Year', ylab=label_cleaner(input$y1),
         xaxt='n', yaxt='n', lwd=line_width,
         cex.lab=fontSize, cex.main=fontSize, cex.sub=fontSize)
    
    # --> Add a subtitle and second y-axis title
    mtext(side=4,label_cleaner(input$y2), padj=3, cex=fontSize)

    # --> First y-axis
    axis(2, labelsY1, col=c1, cex.axis=tick_fs)
    
    # X axis for both plots
    axis(1, pretty(t, xsteps), cex.axis=tick_fs)

    # --> Plot the second variable on the same plot
    par(new=TRUE)
    plot(x=t, y=y2,
         ylim=c(minY2,maxY2),
         col=c2, type='l', lwd=line_width,
         xaxt='n', axes=FALSE, xlab='',ylab='')


    # Second y-axis
    axis(4, labelsY2, col=c2, cex.axis=tick_fs)

    # Legend
    legend("bottomright", inset=.025, legend=c(label_cleaner(input$y1), label_cleaner(input$y2)),
           col=c(c1,c2), lwd=c(3,3))


      
    })
}

shinyApp(ui=ui, server=server)
