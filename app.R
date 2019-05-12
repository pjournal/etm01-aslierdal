library(shiny)
library(shinydashboard)
library(rhandsontable)
library(sp)
library(data.table)
library(tidyverse)
library(dplyr)
library(tidyr)

workingdirectory = "/Users/aslierdal/Desktop/etm58D/Homework3"
setwd(workingdirectory)

matches_raw <- readRDS("df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds")

setDT(matches_raw)

matches_raw$date <- as.Date(as.POSIXct(matches_raw$date, origin="1970-01-01"))

matches_raw <- matches_raw[,c("home","away","score","date")]

matches_raw <- matches_raw[complete.cases(matches_raw[, score]),]
matches_raw <- matches_raw[score != 'POSTP.',]
matches_raw <- separate(matches_raw, score, c("score_home", "score_away"), ":", remove = FALSE, convert = TRUE)

matches_raw[home=='manchester-utd',home:='manchester united']
matches_raw[home=='manchester-united',home:='manchester united']
matches_raw[home=='manchester-city',home:='manchester city']
matches_raw[home=='crystal-palace',home:='crystal palace']
matches_raw[home=='west-ham',home:='west ham']
matches_raw[home=='stoke',home:='stoke city']
matches_raw[home=='newcastle',home:='newcastle utd']

matches_raw[away=='manchester-utd',away:='manchester united']
matches_raw[away=='manchester-united',away:='manchester united']
matches_raw[away=='manchester-city',away:='manchester city']
matches_raw[away=='crystal-palace',away:='crystal palace']
matches_raw[away=='west-ham',away:='west ham']
matches_raw[away=='stoke',away:='stoke city']
matches_raw[away=='newcastle',away:='newcastle utd']

for(year in 2010:2019) {
  matches_raw$season[
    matches_raw$date >= paste(as.character(year), "-07-01", sep = "") & 
      matches_raw$date <= paste(as.character(year+1), "-06-30", sep = "")] <- paste(year, year+1, sep = "/")
}

ui <- fluidPage(
  titlePanel("Premier League Matches"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "team",
        label="Team:",
        choices = unique(unlist(matches_raw[, c("home", "away")]))),
      selectInput(
        inputId = "season",
        label="Season:",
        choices = unique(matches_raw$season))
    ),
    mainPanel(
      
      fluidRow(
        column(width = 6,
               dataTableOutput("match_table")
        ),
        column(width = 6,
               plotOutput("match_plot")
        )
      )
    )
  )
)

server <- function(input, output) {
  
  output$match_table <-renderDataTable({
    
    table_to_show=matches_raw[(home==input$team | away==input$team) & season==input$season][order(date)]
    
    table_to_show[,c("home","away","date","score")]
  }, options = list(
    pageLength = 10
  ))
  
  output$match_plot <- renderPlot({
    
    matches_searched <- matches_raw[(home==input$team | away==input$team) & season==input$season,]
    
    if (nrow(matches_searched) == 0)
      return(renderText("No data available in dataset!"))
    
    matches_searched[,c('home_away') := ifelse(home==input$team, "home", "away")]
    
    matches_searched_agg <- aggregate(matches_searched[, c("score_home", "score_away")], list(matches_searched$home_away), mean)
    
    matches_searched_final <- gather(matches_searched_agg, type, avg, score_home:score_away)
    setDT(matches_searched_final)
    
    ggplot(matches_searched_final[,list(type, Group.1, avg)]) + 
      geom_bar(aes(x=Group.1, y=avg, fill=type),stat="identity",position="dodge") +
      xlab("Home/Away")+
      ylab("Average Goal") + 
      labs(title = paste(input$season, input$team, sep = "  -  ")) +
      theme_minimal() +
      theme(legend.position = "left", legend.title = element_blank())
  })
}

shinyApp(ui, server)