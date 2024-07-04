library(shiny)
library(DT)
library(caret)
data("GermanCredit")


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Summaries for US Treasury Data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h3("This data set comes from the API of US Treasury data."),
      br(),
      h4("You can select a dataset using the buttons below."),
      radioButtons("plot", "Select the Plot Type", choices = list("Just Classification" = "bar", "Classification and Unemployed" = "sideUmemploy", "Classification and Foreign" = "sideForeign"), selected = "bar"),
      checkboxGroupInput("GRFilter", "Filter By Date", choices = c("2012","2013"), selected = c("2012", "2013")),
      br(),
      h4("You can find the", strong("sample mean"), " for a few variables below:"),
      selectInput("var", label = "Dataset to choose", 
                  choices = c("Gold Reserve", "Balance Sheets", "Interest Rates", "Electronic Securities", "Public Debt", "Bond Issues"),
                  selected = "Gold Reserve"),
      numericInput("round", "Select the number of digits for rounding", value = 2, min = 0, max = 5)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("barPlot"),
      br(),
      dataTableOutput("summary")
    )
  )
))
