library(shiny)
library(jsonlite)
library(DT)
library(caret)
library(httr)
library(dplyr)
library(ggbeeswarm)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Summaries for US Treasury Data"),
  tabsetPanel(
  
  tabPanel("About",
           mainPanel(
             h3("Purpose of the App"),
             p("The purpose of this app is to allow the user to explore different datasets given by the US
               Treasury API. The datasets include the balance sheets of the treasury, the interest rates set,
               the public debt amounts, and the amount stored in gold reserves at certain time periods. The
               user can view the datasets, subset which columns they want, filter certain years, and then download
               whatever subset of the datasets they would like. The user can then choose which variables they would
               like to be summarized and which graphs they want within the dataset."),
             h3("Source of the Data"),
             p("This data comes from an official website of the US government, FiscalData. The link for more
               information on the API and documentation can be found here:", a(href =  "https://fiscaldata.treasury.gov/api-documentation/", "US Treasury API")),
             h3("Purpose of each Tab"),
             p("The purpose of the 'About' tab is to give the user an overview of this app, the API that the data sets
               come from, and a link to this API. The purpose of the 'Data Download' tab is to allow the user to query to
               a certain API endpoint, filter the data set by year, and select certain columns of it. The user is then able
               to download whatever subset of the data set they would like. Lastly, the purpose of the 'Data Exploration' tab
               is to allow the user to access different numerical summaries of certain variables within the data set. Also,
               in this tab the user can select from options of graphical summaries to present the chosen parameters in the data."),
           ),
           sidebarPanel(tags$img(src = 'us-treasury.png', align = "center", width = "265px", length = "265px"))
           ),
  
  
  # Data Download tab panel
  tabPanel("Data Download",
    sidebarLayout(
    sidebarPanel(
      h3("This data set comes from the API of US Treasury data."),
      br(),
      
      #Allowing user to select which data set they want out of 6 endpoints
      selectInput("var", label = "Dataset to choose", 
                  choices = c("Gold Reserve", "Balance Sheets", "Interest Rates", "Electronic Securities", "Public Debt", "Bond Issues"),
                  selected = "Gold Reserve"),
      
      # Conditional panel for when user selects Gold Reserve data set
      conditionalPanel(
        "input.var == 'Gold Reserve'",
        radioButtons("GRFilter", "Filter By Year", choices = c("2012","2013", "both"), selected = "both"),
        checkboxGroupInput("GRColumns", "Select desired columns", choices = 
                            list("Record Date" = "record_date","Facility" = "facility_desc","Form of Gold" = "form_desc","Location" = "location_desc", 
                              "Gold Quantity (Ounces)" = "fine_troy_ounce_qty","Book Value" = "book_value_amt"), 
                            selected = list("record_date", "facility_desc", "form_desc", "location_desc", "fine_troy_ounce_qty", "book_value_amt"))),
      
      # Conditional panel for when user selects Balance Sheet data set
      conditionalPanel(
        "input.var == 'Balance Sheets'",
        radioButtons("BSFilter", "Filter By Year", choices = c("1995", "1996", "1997", "all"), selected = "all"),
        checkboxGroupInput("BSColumns", "Select desired columns", choices = 
                             list("Record Date" = "record_date","Reocurring Charge?" = "restmt_flag","Account" = "account_desc", 
                              "Line Item" = "line_item_desc","Bill Amount" = "position_bil_amt"),
                           selected = list("Record Date" = "record_date","Restatement Variable" = "restmt_flag","Account" = "account_desc", 
                                           "Line Item" = "line_item_desc","Bill Amount" = "position_bil_amt"))),
      
      # Conditional panel for when user selects Interest Rates data set
      conditionalPanel(
        "input.var == 'Interest Rates'",
        checkboxGroupInput("IRColumns", "Select desired columns", choices =
                             list("Record Date" = "record_date","Security Type" = "security_type_desc","Specific Security" = "security_desc", 
                                  "Average Interest Rate" = "avg_interest_rate_amt"),
                           selected = list("Record Date" = "record_date","Security Type" = "security_type_desc","Specific Security" = "security_desc", 
                                           "Average Interest Rate" = "avg_interest_rate_amt"))),
      
      # Conditional panel for when user selects Electronic Securities data set
      conditionalPanel(
        "input.var == 'Electronic Securities'",
        radioButtons("ESFilter", "Filter By Year", choices = c("2002", "2003", "both"), selected = "both"),
        checkboxGroupInput("ESColumns", "Select desired columns", choices = 
                             list("Record Date" = "record_date","Security Type" = "security_type_desc", "Security Class" = "security_class_desc", 
                                  "Securities Redeemed" = "securities_redeemed_cnt","Value of Securities Redeemed" = "securities_redeemed_amt"), 
                           selected = list("Record Date" = "record_date","Security Type" = "security_type_desc", "Security Class" = "security_class_desc", 
                                           "Securities Redeemed" = "securities_redeemed_cnt","Value of Securities Redeemed" = "securities_redeemed_amt"))),
      
      # Conditional panel for when user selects Public Debt data set
    conditionalPanel(
      "input.var == 'Public Debt'",
      checkboxGroupInput("PDColumns", "Select desired columns", choices =
                           list("Record Date" = "record_date","Security Type" = "security_type_desc","Security Class" = "security_class_desc","Public Debt Held (in Millions)" = "debt_held_public_mil_amt",
                             "Intra-Government Debt Held (in Millions)" = "intragov_hold_mil_amt","Total Debt Held (in Millions)" = "total_mil_amt"),
                         selected = list("Record Date" = "record_date","Security Type" = "security_type_desc","Security Class" = "security_class_desc","Public Debt Held (in Millions)" = "debt_held_public_mil_amt",
                                         "Intra-Government Debt Held (in Millions)" = "intragov_hold_mil_amt","Total Debt Held (in Millions)" = "total_mil_amt"))),
    
    # Conditional panel for when user selects Bond Issues data set
    conditionalPanel(
      "input.var == 'Bond Issues'",
      checkboxGroupInput("BIColumns", "Select desired columns", choices =
                           list("Record Date" = "record_date","CD Series" = "series_cd", "Overall Series" = "series_desc", "Bonds Issued" = "bonds_issued_cnt",
                              "Bonds Redeemed" =  "bonds_redeemed_cnt","Bonds Outstanding" = "bonds_out_cnt", "Bonds Matured" = "bonds_matured_cnt", "Bonds Unmatured" = "bonds_unmatured_cnt"),
                         selected = list("Record Date" = "record_date","CD Series" = "series_cd", "Overall Series" = "series_desc", "Bonds Issued" = "bonds_issued_cnt",
                                         "Bonds Redeemed" =  "bonds_redeemed_cnt","Bonds Outstanding" = "bonds_out_cnt", "Bonds Matured" = "bonds_matured_cnt", "Bonds Unmatured" = "bonds_unmatured_cnt"))),
    
    # Adding a download button so the user can download the data as a csv file
    downloadButton("downloadData", "Download CSV")
    ),
    
    # Outputting the subsetted data table to the main panel
    mainPanel(
      dataTableOutput("summary")
    )
  )
  ),
  
  
  # Data Exploration tab
  tabPanel("Data Exploration",
           # Explaining the purpose of this tab
           h3("Exploring the Gold Reserves Dataset"),
           p("This is the full dataset from the Gold Reserves of the US Treasury. This tab allows the 
             user to choose between making contingency tables, numerical summaries, and plots of certain
             Treasury Data."),
           # Explaining the contingency tables to user
           h3("Contingency Tables Info"),
           p("For the contingency tables, the data being tabulated is from the Gold Reserves
             data set. Selecting each variable or multiple variables will give a table of the count of 
             measurements in the data set taken from each location, which facility the location is in, and
             the count of gold coins vs. gold bullions.") ,
           # Explaining the numerical summaries to user
           h3("Numerical Summaries Info"),
           p("For the numerical summaries, the summaries are of variables
             within the Bonds Issued data set. The user can select which type of bond they want summarized, along
             with which statistic they would like. The statistic will be reported for 5 variables within the 
             Bonds Issued data set: Bonds Issued, Bonds Redeemed, Bonds Outstanding, Bonds Matured, Bonds 
             Unmatured."),
           # Explaining information about the graphs
            h3("Graphs Info"),
            p("Lastly, the graphs panel allows the user to select from 4 different graphs.
             These graphs use different data sets within the US Treasury API that can be accessed in
             the Data Download Panel."),
           
      sidebarLayout(sidebarPanel(
        # Allowing user to select whether they would like contingency tables, numerical summaries,
        # or graphs to be displayed
      selectInput("Summary", label = "Select which summary you would like.", 
                  choices = c("Contingency Tables", "Numerical Summaries", "Graphs")),
      # Conditional panel for the user selecting contingency table panel
      conditionalPanel(
        "input.Summary == 'Contingency Tables'",
        # Allowing user to select variables to make contingency tables of
        checkboxGroupInput("CategoricalSummaries", "Select variables to make a contigency table for.",
                           choices = list("Facility" = "facility_desc", 
                                          "Form of Gold" = "form_desc", "Location" = "location_desc"),
                           selected = list("facility_desc"))),
      
      # Conditional panel for the user selecting numerical summaries panel
      conditionalPanel(
        "input.Summary == 'Numerical Summaries'",
        # Allowing user to select bond type to get statistics for between 4 options
        radioButtons("BondType", "Select type of bond to summarize statistics for.", 
                           choiceNames = c("Series E", "Series EE", "Series F", "Series G"),
                          choiceValues = c("E", "EE", "F", "G"),
                           selected = "E"),
        # Allowing user to select which numerical summary they want for the bond type specified
        radioButtons("NumericalSummaryType", "Select which numerical summary you would like.", 
                     choices = c("mean", "median", "sd", "max", "min"), 
                     selected = "mean")),
      
      # Conditional panel for the user selecting graphs panel
      conditionalPanel("input.Summary == 'Graphs'",
                           selectInput("plotType", "Select Plot Type",
                                       choices = list("Bar Graph" = "bar", 
                                                      "Line Graph" = "line", 
                                                      "Swarm Plot" = "swarm", 
                                                      "Scatter Plot" = "scatter"))
                         )
        
      ),
      
      # Main panel that displays the tables or graphs the user selected
      mainPanel(conditionalPanel("input.Summary == 'Contingency Tables'",
                                 tableOutput("contingencyTable")),
                conditionalPanel("input.Summary == 'Numerical Summaries'",
                tableOutput("NumericalSummaries")),
                conditionalPanel("input.Summary == 'Graphs'",
                conditionalPanel("input.plotType == 'bar'",
                                 # Allowing the user to select whether they want faceting
                                 selectInput("FacetWrap", "Do you want to facet wrap this plot?",
                                             choices = c("Yes", "No"),
                                             selected = "No"),
                                  plotOutput("barPlot")
                                     ),
                conditionalPanel("input.plotType == 'line'",
                                  plotOutput("linePlot")
                                     ),
                conditionalPanel("input.plotType == 'swarm'",
                                  plotOutput("swarmPlot")
                                     ),
                conditionalPanel("input.plotType == 'scatter'",
                                  plotOutput("scatterPlot")
                                     )
                                   )
           ))
  ))
))
