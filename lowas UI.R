library(shiny)
library("Hmisc")


setwd("C:\\Users\\Admin\\Documents\\Test_Data")
my_data <- read.csv("lowa\\lowatrain.csv", sep = ",")


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
  
  setwd("C:\\Users\\Admin\\Documents\\Project1")
  write.csv(datavalues, file = "input_data.csv")
  
  source("lowas predictions.R")
  Result <- read.table(file="Result_data.csv", header=TRUE, sep=",")
  Resultdata <- round((as.numeric(Result[1,2])),digits = 2)
  print(Resultdata)
  return(Resultdata)
}

server <- function(input, output) {
  output$Predict <- renderText({
    paste("Predicted Value of £", Logic(input))
  })
}
shinyApp(ui = ui, server = server)