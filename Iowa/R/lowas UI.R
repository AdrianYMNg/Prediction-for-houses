#install.packages("shiny")
#install.packages("Hmisc")
library(shiny)
library("Hmisc")

options(scipen = 9999)
ui <- fluidPage(
  titlePanel("Prediction Value of House"),

  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "LotAreaValue", label = "Lot Area", value = 50, min = 50, step = 1),
      sliderInput(inputId = "OverallConditionValue", label = "Overall Condition", min = 0, value = 0, step = 1, max = 10),
      numericInput(inputId = "YearBuiltValue", label = "Year Built", min = 1750, value = 1750, step = 1, max = 2019),
      selectInput(inputId = "CentralAirValue", label = "Central Air-conditioning", choices = c("Yes","No")),
      selectInput(inputId = "KitchenQualityValue", label = "Kitchen Quality", choices = c("Excellent","Good","Average","Fair","Poor")),
      sliderInput(inputId = "FirePlaceValue", label = "Fire Place", min = 0, value = 0, step = 1, max = 3)
    ),
    mainPanel(
      textOutput(outputId = "Predict")
    )
  )
)

#' Turning input data into a dataframe
#'
#' The function will obatain the input from the front-end UI insert them into a dataframe
#' It tranforms the values of the dataframe to a useble format that matches the training data
#' @return The dataframe containing the inputs will be returned
#' @param input
#' A selection of valus that matches a dataframe format should be checked
#' @export
Logic <- function(input) {
  SalePrice = 0
  LotArea <- (as.numeric(input$LotAreaValue))
  OverallCond <- (as.numeric(input$OverallConditionValue))
  YearBuilt <- (as.numeric(input$YearBuiltValue))
  CentralAir <- input$CentralAirValue
  if(CentralAir == "Yes"){
    CentralAir <- "Y"
  }else if(CentralAir == "No"){
    CentralAir <- "N"
  }
  KitchenQual <- input$KitchenQualityValue
  if(KitchenQual == "Excellent"){
    KitchenQual <- "Ex"
  }else if(KitchenQual == "Good"){
    KitchenQual <- "Gd"
  }else if(KitchenQual == "Average"){
    KitchenQual <- "TA"
  }else if(KitchenQual == "Fair"){
    KitchenQual <- "Fa"
  }else if(KitchenQual == "Poor"){
    KitchenQual <- "Po"
  }
  Fireplaces <- (as.numeric(input$FirePlaceValue))
  datavalues <- data.frame(SalePrice,LotArea,OverallCond,YearBuilt,CentralAir,KitchenQual,Fireplaces)
  input_data <<- datavalues

  setwd("C:\\Users\\Admin\\Documents\\Project1")#-------------change when upload to cloud
  source("lowas predictions.R")
  Resultdata <- round((as.numeric(Result)),digits = 2)
  print(Resultdata)
  return(Resultdata)
}

server <- function(input, output) {
  output$Predict <- renderText({
    paste("Predicted Value of £", Logic(input))
  })
}
shinyApp(ui = ui, server = server)
