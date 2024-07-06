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
      br(),
      h4("You can find the", strong("sample mean"), " for a few variables below:"),
      selectInput("var", label = "Dataset to choose", 
                  choices = c("Gold Reserve", "Balance Sheets", "Interest Rates", "Electronic Securities", "Public Debt", "Bond Issues"),
                  selected = "Gold Reserve"),
      conditionalPanel(
        "input.var == 'Gold Reserve'",
        radioButtons("GRFilter", "Filter By Date", choices = c("2012","2013", "both"), selected = "both"),
        checkboxGroupInput("GRColumns", "Select desired columns", choices = 
                            c("record_date", "facility_desc", "form_desc", "location_desc", "fine_troy_ounce_qty", "book_value_amt", "record_calendar_year"), 
                            selected = c("record_date", "facility_desc", "form_desc", "location_desc", "fine_troy_ounce_qty", "book_value_amt", "record_calendar_year"))),
    
      conditionalPanel(
        "input.var == 'Balance Sheets'",
        radioButtons("BSFilter", "Filter By Date", choices = c("1995", "1996", "1997", "all"), selected = "all")),
      conditionalPanel(
        "input.var == 'Electronic Securities'",
        radioButtons("ESFilter", "Filter By Date", choices = c("2002", "2003", "both"), selected = "both"),
        checkboxGroupInput("ESColumns", "Select desired columns", choices = 
                             c("record_date", "security_type_desc", "security_class_desc", "securities_redeemed_cnt", "securities_redeemed_amt", "record_calendar_year"), 
                           selected = c("record_date", "security_type_desc", "security_class_desc", "securities_redeemed_cnt", "securities_redeemed_amt", "record_calendar_year"))),
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
