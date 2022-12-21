#
# SDS 313 Shiny App Example - Films with variable choice
#

library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(knitr)

college <-read.csv('College_Data.csv')
college1 <-read.csv('DataSet_Admissions.csv')
college$accptrate <- college$Accept/college$Apps
college$yieldrate <- college$Enroll/college$Accept
colnames(college)[1] = "schoolname"
mergedcollege <- merge(college,college1,by="schoolname")
collegefinal <-select(mergedcollege, "Private", "Apps", "Room.Board", "S.F.Ratio", "accptrate", "yieldrate", "Grad.Rate", "UGGPA75", "PhD", "Expend", "Top10perc")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("College Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      #Select box for variable:
      selectInput("selectvar", label = h3("Choose a variable"), 
                  choices=list("Graduation Rate"=1, "Room & Board Cost"=2, "Acceptance Rate"=3, "Private"=4, "Personal Spending"=5), 
                  selected = 1),
      selectInput("colorvar", label = h3("Choose a color"),
                  choices =list("Dark Salmon"= 1, "Orchid"=2, "Turquoise"=3), 
                  selected = 1),
      
      # Slider input for number of bins
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      #option to show mean
      checkboxInput("checkbox1", label="Display mean", value=FALSE),
      
      #option to show table
      checkboxInput("checkbox3", label = "Display frequency table", value = FALSE),
      
      
      #option to show sd
      checkboxInput("checkbox2", label="Display standard deviation", value=FALSE),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(textOutput("College Admissions"),
      plotOutput("distPlot"),
      tags$img(src="collegeadmissions.jpg",height = 400, width = 400, align = "center"),
      tags$figcaption("Multiple factors are taken into consideration when students are applying to college and ultimately where they end up going. There were many factors I had to consider such as finances, distance from home, reputation of my major, etc. This project aims to find associations between variables and college decisions. The first data set used for this project was from [Kaggle](https://www.kaggle.com/datasets/yashgpt/us-college-data). The second data set I used was from [Analytix] (https://analytix.accesslex.org/download-dataset). I used the dropdown to select 2019 as the year for admissions data sets because the first data set is specific to 2019."),
      hr(),
      p('Mean:'),
      fluidRow(column(5, verbatimTextOutput("mean"))),
      p('Standard deviation:'),
      fluidRow(column(5, verbatimTextOutput("sd"))),
      p("Frequency Table:"),
      fluidRow(column(5, verbatimTextOutput("table"))),
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    if(input$selectvar == 1){
      if(input$colorvar ==1){
        hist(collegefinal$Grad.Rate, breaks = input$bins, main='Distribution of University Graduation Rate',xlab='University Graduation Rate',col = 'darksalmon', border = 'black')}
      else if(input$colorvar ==2){
        hist(collegefinal$Grad.Rate, breaks = input$bins, main='Distribution of University Graduation Rate',xlab='University Graduation Rate',col = 'orchid', border = 'black')}
      else if(input$colorvar ==3){
        hist(collegefinal$Grad.Rate, breaks =input$bins, main = 'Distribution of University Graduation Rate', xlab = "University Graduation Rate", col = "turquoise", border = 'black')}
    }
    
    if(input$selectvar == 2){
      if(input$colorvar ==1){
        hist(collegefinal$Room.Board, breaks = input$bins, main='Distribution of Room and Board Cost',xlab='Room and Board Cost',col = 'darksalmon', border = 'black')}
      else if(input$colorvar ==2){
        hist(collegefinal$Room.Board, breaks = input$bins, main='Distribution of Room and Board Cost',xlab='Room and Board Cost',col = 'orchid', border = 'black')}
      else if(input$colorvar ==3){
        hist(collegefinal$Room.Board, breaks =input$bins, main = 'Distribution of Room and Board Cost', xlab = "Room and Board Cost", col = "turquoise", border = 'black')}
        

    }
    if(input$selectvar == 3){
      if(input$colorvar ==1){
        hist(collegefinal$accptrate, breaks = input$bins, main='Distribution of Acceptance Rate',xlab='Acceptance Rate',col = 'darksalmon', border = 'darkgrey')}
      else if(input$colorvar ==2){
        hist(collegefinal$accptrate, breaks = input$bins, main='Distribution of Acceptance Rate',xlab='Acceptance Rate',col = 'orchid', border = 'darkgrey')}
      else if(input$colorvar ==3){
        hist(collegefinal$accptrate, breaks = input$bins, main='Distribution of Acceptance Rate',xlab='Acceptance Rate',col = 'turquoise', border = 'darkgrey')}
 
    }
    
    if(input$selectvar == 4){
      if(input$colorvar ==1){
        barplot(table(collegefinal$Private), main='Distribution of Acceptance Rate',xlab='Acceptance Rate',col = 'darksalmon', border = 'darkgrey')}
      else if(input$colorvar ==2){
        barplot(table(collegefinal$Private), main='Distribution of Acceptance Rate',xlab='Acceptance Rate',col = 'orchid', border = 'darkgrey')}
      else if(input$colorvar ==3){
        barplot(table(collegefinal$Private), main='Distribution of Acceptance Rate',xlab='Acceptance Rate',col = 'turquoise', border = 'darkgrey')}
      
    }
    
    if(input$selectvar == 5){
      if(input$colorvar ==1){
        hist(collegefinal$Expend, main= "Distribution of Personal Spending",xlab = "Personal Spending",col = 'dark salmon',border = "darkgrey")}
      else if(input$colorvar ==2){
        hist(collegefinal$Expend, main= "Distribution of Personal Spending",xlab = "Personal Spending",col = 'orchid',border = "darkgrey")}
      else if(input$colorvar ==3){
        hist(collegefinal$Expend, main= "Distribution of Personal Spending",xlab = "Personal Spending",col = 'turquoise',border = "darkgrey")}
      
    }
      
  })
  
  
  
  #Display mean if selected
  output$mean <- renderPrint({ 
    if(input$checkbox1 == TRUE & input$selectvar == 1){
      mean(collegefinal$Grad.Rate, na.rm=TRUE)}
    else if(input$checkbox1 == TRUE & input$selectvar == 2){
      mean(collegefinal$Room.Board, na.rm=TRUE)}
    else if(input$checkbox1 == TRUE & input$selectvar == 3){
      mean(collegefinal$Room.Board, na.rm=TRUE)}
    else if(input$checkbox1 ==TRUE & input$selectvar == 5){
      mean(collegefinal$Expend, na.rm=TRUE)}

  })
  
  #Display sd if selected
  output$sd <- renderPrint({ 
    if(input$checkbox2 == TRUE & input$selectvar == 1){
      sd(collegefinal$Grad.Rate, na.rm=TRUE)}
    else if(input$checkbox2 == TRUE & input$selectvar == 2){
      sd(collegefinal$Room.Board, na.rm=TRUE)}
    else if(input$checkbox2 == TRUE & input$selectvar == 3){
      sd(collegefinal$Room.Board, na.rm=TRUE)}
    else if(input$checkbox2 ==TRUE & input$selectvar == 5){
      sd(collegefinal$Expend, na.rm=TRUE)}
  })
  
  #Display table if selected
  output$table <- renderPrint({
    if(input$checkbox3 == TRUE & input$selectvar ==4){
      table(collegefinal$Private)}
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
