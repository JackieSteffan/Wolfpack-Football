#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
football <- read_csv("State_Football.csv")
library(ggplot2)
library(caret)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("NC State Football"),
    
    tabsetPanel(
        #Summary Tab
        tabPanel("Summary",
                 h1("App Summary", style="color:red"),
                 mainPanel(
                     #Explain Data
                     h3("The Data:"),
                     h5("This data shows game level statistics for NC State in the last 8 years, or in the seasons under Dave Doren. Most of this data is offensive statistics, and does not show statistics for the opposite team. Keep that in mind on the modeling page, so the models will not accurately predict wins and losses."),
                     #Explain Purpose
                     h3("App Purpose:"),
                     h5("The purpose of this app is to dig deeper into NC State's football team and their statistics. Some tabs will just look at the data, while others will attempt to model wins and losses."),
                     #Explain Navigation
                     h3("How to Navigate"),
                     h5("At the top you will see different tabs each with different information about the data. In each tab you will have the option to change the visualizations on the lefthand side."),
                     h4("For more information on Wolfpack football, visit", a(href = "https://gopack.com/sports/football", target= "_blank", "this webpage"))
                 )),
        #Data Exploration Tab
        tabPanel("Summary Statistics",
                 sidebarLayout(
                     sidebarPanel(
                         #Select Variable to view
                         selectInput("stats", "Select Statistic to view", selected = "Yards", choices = c("Yards", "Passing Statistics", "Downs", "Turnovers", "Sacks")),
                         conditionalPanel(condition = "input.stats == 'Yards'",
                                          selectInput("YardVar", "More Statistics", choices = c("Total", "Rushing", "Passing")))
                     ,
                         conditionalPanel(condition = "input.stats == 'Passing Statistics'",
                                         selectInput("PassVar", "More Statistics", choices = c("Pass Attempts", "Pass Completions", "Completion Percentage"))),
                         conditionalPanel(condition = "input.stats == 'Downs'",
                                         selectInput("DownVar", "More Statistics", choices = c("First Down", "Third Down Conversion"))),
                         conditionalPanel(condition = "input.stats == 'Turnovers'",
                                         selectInput("TurnVar", "More Statistics", choices = c("Interceptions", "Fumbles (Both lost and recovered)", "Total Turnovers"))),
                         conditionalPanel(condition = "input.stats == 'Sacks'",
                                         selectInput("SackVar", "More Statistics", choices = c("Number of Sacks", "Yards Lost"))),
                     #Against ACC?
                         checkboxInput("ACC", "Against ACC Opponents?", value = F),
                         conditionalPanel(condition = "input.ACC == 1",
                                          checkboxInput("Atlantic", "Against Atlantic Division Opponents?", value = F)),
                     #Download Data
                     downloadButton("downloadPlot", "Download")
                 ),
                                          
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         plotlyOutput('plot'),
                         br(),
                         DT::dataTableOutput("statTab")
                     )
                 )),
        #PCA Tab
        tabPanel("Principal Component Analysis",
                 sidebarLayout(
                 sidebarPanel(
                     sliderInput("PCAIn", "Choose number of variables", min = 3, max = 8, value = 1, step = 1)
                 ),
                 mainPanel(
                     plotOutput("footballPCA")
                 )
                 )),
        #Modeling Tab
        tabPanel("Modeling",
                 sidebarLayout(
                   sidebarPanel(
                       selectInput("numVar1", "Select the # of variables to build our Win/Loss Model", c(1,2,3)),
                       h4("Predict if NC State will win based on the following:"),
                       selectInput("numVar", "Select the # of variables to build our touchdown Model", c(1,2,3)),
                       h4("Predict the number of touchdowns based on the following:"),
                     numericInput("inTotOff", "Total Offense", value=0, min=0),
                     numericInput("inYardsPlay", "Yards per Play", value=0, min=0),
                     numericInput("in3DownConv", "Third Down Conversion Percentage (0-1)", value=0, min=0, max=1)
                   ),
                   mainPanel(
                     h4("Model 1: Predict Wins:"),
                       plotOutput("plotWinTree"),
                     br(),
                     h4("Model 2, which models the number of touchdowns has the form:"),
                     verbatimTextOutput("tdCoeff2"),
                     br(),
                     h4("The number of touchdowns you would expect is:"),
                     textOutput("tdPred")
                   )
                 )),
        tabPanel("Data",
                 sidebarLayout(
                   sidebarPanel(
                     #subset data
                     radioButtons("subData", "Filter Data", choices = c("All Games", "ACC Opponents", "Wins", "Home Games"), selected = NULL),
                     #Download Data
                     downloadButton("downloadData", "Download"),
                     withMathJax(helpText("Pass Completion is calculated by: $$\\ (passes complete) / \\ (pass attempts)$$"))
                   ),
                   mainPanel (
                     DT::dataTableOutput("datTab")
                   )
                 ))
    )
    # Sidebar with a slider input for number of bins
    
))
