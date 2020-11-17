#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  #Filter data
  filtDat <- reactive({
    if (input$ACC){
      if(input$Atlantic){
        filtDat <- football%>% filter(Atlantic==1)
      }
      else {filtDat <- football%>% filter(ACC==1)}
    }
    else {filtDat <- football}
    filtDat
  })
  #y variable
  y <- reactive({
    football <- filtDat()
    #When Yards selected
    if(input$stats == "Yards"){
      if(input$YardVar == "Total"){
        y <- football$Total_offense}
      else if (input$YardVar == "Rushing") {
        y <- football$Rushing_Yards}
      else {
        y <- football$Passing_Yards}}
    #when passing stats selected
    else if (input$stats == "Passing Statistics"){
      if (input$PassVar == "Pass Attempts") {
        y <-football$Pass_Attempts}
      else if (input$PassVar == "Pass Completions") {
        y <- football$Pass_Completions}
      else{
        y <- football$Completion_Percentage}}
    #when downs selected
    else if (input$stats == "Downs"){
      if (input$DownVar == "First Down") {
        y <-football$First_Downs      }
      else{
        y <- football$Third_Down_Conversion    }}
    #when turnovers selected
    else if (input$stats == "Turnovers"){
      if (input$TurnVar == "Interceptions") {
        y <- football$Interceptions      }
      else if (input$TurnVar == "Total Turnovers") {
        y <- football$Turnovers      }
      else{
        y <-football$Fumbles      }}
    #when sacks selected
    else {
      if (input$SackVar == "Number of Sacks") {
        y <- football$Sacks      }
      else{
        y <- football$Sack_Yards      }}
  })
  #x axis
  x <- reactive({
    football <- filtDat()
    x<- football$Year
  })
  
  
  #plotly
  output$plot <- renderPlotly({
    plot1 <- plot_ly(
      x = x(),
      y = y(), 
      type = 'scatter',
      mode = 'markers')
    plot1%>%layout(xaxis = list(title = "Year"), yaxis = list(title = y))

  })
  
  #Download Plot
  # output$downloadPlot <- reactive({
  #   export(output$plot, file = "image.png")
  #   })

    #create summary tab table
    output$statTab <- DT::renderDataTable({
      football <- filtDat()
        #Yards table
        if (input$stats == "Yards"){
           tab <- football%>%select( c(Year, Opponent, Total_offense, Rushing_Yards, Passing_Yards))%>%group_by(Year) %>% 
             summarise(TotalOffense=round(sum(Total_offense),2), 
                       avgOffense = round(mean(Total_offense),2),
                       totalRushYards = round(sum(Rushing_Yards),2), 
                       avgRushYards = round(mean(Rushing_Yards),2),
                       totalPassingYards = round(sum(Passing_Yards),2),
                       avgPassingYards = round(mean(Passing_Yards),2))
           }
        #Passing table
        else if (input$stats == "Passing Statistics"){
            tab <- football%>%select(c(Year, Pass_Attempts, Pass_Completions, Completion_Percentage)) %>% group_by(Year)%>%
              summarise(totalPassAttempts = sum(Pass_Attempts),
                        avgPassAttempts = round(mean(Pass_Attempts),2),
                        totalPassCompletions = sum(Pass_Completions),
                        avgPass_completions = round(mean(Pass_Completions),2),
                        avgCompletionPercentage = round(mean(Completion_Percentage),2))
        }
        #Downs table
        else if (input$stats == "Downs"){
            tab <- football%>%select(c(Year,First_Downs, Third_Down_Conversion))%>%group_by(Year)%>%
              summarise(totalFirstDowns = sum(First_Downs),
                        avgFirstDowns = round(mean(First_Downs),2),
                        avgThirdDownConversion = round(mean(Third_Down_Conversion),2))
        }
        #Turnover Table
        else if (input$stats == "Turnovers"){
            tab <- football%>%select(c(Year, Interceptions, Fumbles, Turnovers))%>%group_by(Year)%>%
              summarise(totalInterceptions=sum(Interceptions),
                        avgInterception = round(mean(Interceptions),2),
                        totalFumbles = sum(Fumbles),
                        avgFumbles = round(mean(Fumbles),2),
                        totalTurnovers = sum(Turnovers),
                        avgTurnovers = round(mean(Turnovers),2))
        }
        #Sacks Table
        else {
            tab <- football%>%select(c(Year, Sacks, Sack_Yards))%>%group_by(Year)%>%
              summarise(totalSacks = sum(Sacks),
                        avgSacks = round(mean(Sacks),2),
                        totalSackYards = sum(Sack_Yards),
                        avgSackYards = round(mean(Sack_Yards),2))
        }
        tab
    })
    

    #PCA
    output$footballPCA <- renderPlot({
        PCAVars <- football[,7:((input$PCAIn)+6)]
        PCs <- prcomp(PCAVars, center = TRUE, scale = TRUE)
        biplot(PCs, xlabs = rep(".", nrow(football)), cex = 1.2)
    })
    
    #Model 1: W/L classification
    colnames(football) <- make.names(colnames(football))
    winTree <- reactive({
      set.seed(91)
      treeFootball <- select(football, -Opponent)
      train <- sample(1:nrow(treeFootball), size = nrow(treeFootball)*0.8)
      test <- dplyr::setdiff(1:nrow(treeFootball), train)
      footballTrain <- treeFootball[train, ]
      footballTest <- treeFootball[test, ]
      classTree <- train(Win_Loss ~ ., data = footballTrain, method = "rpart",
                         trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5),
                         preProcess = c("center", "scale"))
      winTree <- classTree$finalModel
      })
    output$plotWinTree <- renderPlot({
      set.seed(91)
      winTree <- winTree()
      plot(winTree, uniform=TRUE,
           main="Win/ Loss Classification Tree")
      text(winTree, all=TRUE, cex=.8)
    })
    
    #Model 2 Predict TD
    touchdownMod <- reactive({
      if (input$numVar == 3){
      touchdownMod <- lm(Touchdowns ~ Total_offense + Yards.Play + Third_Down_Conversion, data = football)}
      else if (input$numVar == 2){
        touchdownMod <- lm(Touchdowns ~ Total_offense + Yards.Play, data = football)
      }
      else {touchdownMod <- lm(Touchdowns ~ Total_offense, data = football)}
    })
    #Print Model 
    output$tdCoeff2 <- renderPrint({
      touchdownMod <- touchdownMod()
      touchdownMod$coefficients
    })
    
    #Predictions
    output$tdPred <- renderText({
      touchdownMod<- touchdownMod()
      #td <- touchdownMod$coefficients[[1]] + touchdownMod$coefficients[[2]]*input$inTotOff + touchdownMod$coefficients[[3]]*input$inYardsPlay+ touchdownMod$coefficients[[4]]*input$in3DownConv
      td<- predict(touchdownMod, data.frame(Total_offense = c(input$inTotOff), Yards.Play = c(input$inYardsPlay), Third_Down_Conversion = c(input$in3DownConv)))
      round(td,0)
    })
    
    # Data table for data tab
    output$datTab <- DT::renderDataTable({
      if (input$subData == "ACC Opponents") {
        football %>% filter(ACC == 1)
      }
      else if (input$subData == "Wins"){
        football %>% filter(Win_Loss == "Win")
      }
      else if (input$subData == "Home Games"){
        football %>% filter(Home_Away == "Home")
      }
      else football
    })
    
    #Download Data
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$dataset, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(datasetInput(), file, row.names = FALSE)
      }
    )
})
