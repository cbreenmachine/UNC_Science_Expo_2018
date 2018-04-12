## Shiny App to visualize spurious correlations with UNC basketball
#  This script sets up the user interface for correlating UNC basketball wins/win %
#  with other phenomena. To start, it will have two dropdown menus--one for each
#  data set. Additionally, it will show the R^2 value of a simple linear regression

# Set up environment
library(shiny)
library(readxl)
library(ggplot2)
library(tidyverse)

#setwd("C:/Users/cbreen/Documents/R/Spurious Correlations UNC Basketball/Shiny App/")
source("rowEqualizer.R")

### IMPORT DATA ###

# --> Where the CSV we are interested in lives
address <- 'data.xlsx'
address2<- 'data_labels.xlsx'

# --> The rest of the CSV contains the actual data
data <- read_excel(address, col_names=TRUE,range = cell_rows(1:21))
my_labels <- read_excel(address2, col_names=FALSE)
my_labesl <- unlist(my_labels, use.names=FALSE)

# --> Combine all of this informtion into a dat frame
df <- data.frame(data)


# Define the user interface for our app
ui <- fluidPage(
  
  title = "Correlations with UNC Basketball Data",
  
  plotOutput(outputId = "time_series", width = "95%"),
  
  hr(),
  
  fluidRow(
    column(3,
           h3("Test out your own variables..."),
           selectInput("y1", "Pick a UNC men's basketball variable:", choice=colnames(df[,2:3])),
           selectInput("y2", "Pick another variable:", choice=colnames(df[-1:-3])),
           br()
  ),
  
  column(4, offset=1,
         h3("Or pick from some of our favorites!"),
         selectInput('x', 'Our favorite plots:', choices=c('one','two','three'),selected=NULL)
  )
)
)

  

################### Server of our App. This creates the output and does the plotting. ###################
server <- function(input, output){

  output$time_series <- renderPlot({
    
    ###### Calculate a simple linear model ######
    y1 <- df[,input$y1]
    y2 <- df[,input$y2]
    t  <- df$year

    cleaned_df <- rowEqualizer(t, y1, y2)
    t <- cleaned_df$x1
    y1<- cleaned_df$x2
    y2<- cleaned_df$x3
    
    linear_model <- lm(y2 ~ y1, data=cleaned_df)
    print(summary(linear_model))
    r_squared <- round(summary(linear_model)$r.squared, digits=3)
    subtitle <- paste("Correlation:", toString(r_squared),"%", collapse=" ")
    
    
    ###### Plot the two variables with time ######
    
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
         col=c1, type='l', main='Correlation over time',
         xlab='Year', ylab=input$y1,
         xaxt='n', yaxt='n', lwd=line_width,
         cex.lab=fontSize, cex.main=fontSize, cex.sub=fontSize)
    
    # --> Add a subtitle and second y-axis title
    mtext(subtitle, cex=fontSize)
    mtext(side=4,input$y2, padj=3, cex=fontSize)

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
    legend("bottomright", inset=.025, legend=c(input$y1,input$y2),
           col=c(c1,c2), lwd=c(3,3))
    

      
    })
}

shinyApp(ui=ui, server=server)