# Loading required packages
library(shiny)
library(jsonlite)
library(DT)
library(caret)
library(httr)
library(tidyverse)
library(ggbeeswarm)

# Creating the shiny server
shinyServer(function(input, output) {
  # Function to clean each URL into a final data set
  API_Cleaning <- function(url) {
    raw_data <- httr::GET(url)
    parsed_data <- fromJSON(rawToChar(raw_data$content))
    final_data <- as_tibble(parsed_data$data)
    return(final_data)
  }
  
  #Creating data as a reactive variable
  data <- reactive(
    # In this section we will use conditional logic to clean the correct URL for the data set that is
    # selected by the user. Because each of the data sets have different columns and features, I will
    # each data set by year based on what the user inputs and will select all the columns the user selects.
    # Accessing the Gold Reserve data set when the user selects this
    if (input$var == "Gold Reserve") {
      url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/gold_reserve"
      final_data <- API_Cleaning(url)
      # Subsetting the data with the columns the user selects and year to filter the data
      final_data <- final_data |>
        select(all_of(input$GRColumns), record_calendar_year)
      # Conditional logic to filter the data by whichever year is selected by the user
      if(input$GRFilter == "both") {
        final_data_GR <- final_data
      }
      if(input$GRFilter == "2012") {
        final_data_GR <- final_data |>
          filter(record_calendar_year == "2012")
      }
      if (input$GRFilter == "2013") {
        final_data_GR <- final_data |>
          filter(record_calendar_year == "2013")
      }
      return(final_data_GR)
      # Repeating the process for when the user selects the balance sheet data set
    } else if (input$var == "Balance Sheets") {
      url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/balance_sheets"
      final_data <- API_Cleaning(url)
      final_data <- final_data |>
        select(all_of(input$BSColumns), record_calendar_year)
      if(input$BSFilter == "all") {
        final_data_BS <- final_data
      }
      if(input$BSFilter == "1995") {
        final_data_BS <- final_data |>
          filter(record_calendar_year == "1995")
      }
      if (input$BSFilter == "1996") {
        final_data_BS <- final_data |>
          filter(record_calendar_year == "1996")
      }
      if (input$BSFilter == "1997") {
        final_data_BS <- final_data |>
          filter(record_calendar_year == "1997")
      }
      return(final_data_BS)
      # Repeating the process for when the user selects the interest rates data set
    } else if (input$var == "Interest Rates") {
      url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/avg_interest_rates"
      final_data <- API_Cleaning(url)
      final_data_IR <- final_data |>
        select(all_of(input$IRColumns), record_calendar_year)
      return(final_data_IR)
      # Repeating the process for when the user selects the electronic securities data set
    } else if (input$var == "Electronic Securities") {
      url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/accounting/od/securities_redemptions"
      final_data <- API_Cleaning(url)
      final_data <- final_data |>
        select(all_of(input$ESColumns), record_calendar_year)
      if(input$ESFilter == "both") {
        final_data_ES <- final_data
      }
      if(input$ESFilter == "2002") {
        final_data_ES <- final_data |>
          filter(record_calendar_year == "2002")
      }
      if (input$ESFilter == "2003") {
        final_data_ES <- final_data |>
          filter(record_calendar_year == "2003")
      }
      return(final_data_ES)
      # Repeating the process for when the user selects the public debt data set
    } else if (input$var == "Public Debt") {
      url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/debt/mspd/mspd_table_1"
      final_data <- API_Cleaning(url)
      final_data_PD <- final_data |>
        select(all_of(input$PDColumns), record_calendar_year)
      return(final_data_PD)
      # Repeating the process for when the user selects the bond issues data set
    } else if (input$var == "Bond Issues") {
      url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/accounting/od/savings_bonds_report"
      final_data <- API_Cleaning(url)
      final_data_BI <- final_data |>
        select(all_of(input$BIColumns), record_calendar_year)
      return(final_data_BI)
    }
  )
  # Outputting this reactive data table created above
  output$summary <- DT::renderDataTable({
    data()
  })
  
  # Allowing the user to download the subsetted data set they selected as a csv file
  output$downloadData <- downloadHandler(
    filename = function() {
      'US-Treasury-Data.csv'
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
  # Outputting the contingency table of the gold reserve data set
  output$contingencyTable <- renderTable({
    url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/gold_reserve"
    final_data_GR <- API_Cleaning(url)
    # Selecting the variables selected by the user and then creating a contingency table based on those
    selected_categorical_data <- final_data_GR |>
      select(all_of(input$CategoricalSummaries))
    table(selected_categorical_data)
  })
  
  # Creating a table to produce numerical summaries for the bonds issued data set
  output$NumericalSummaries <- renderTable({
    # We will be using the bonds issued data set for this output
    url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/accounting/od/savings_bonds_report"
    final_data_BI <- API_Cleaning(url)
    # We will now change the necessary variables to numeric using mutate, group by the series CD,
    # select the type of bond specified by the user, and summarize the necessary variables with whatever
    # summary the user wants
    final_data_BI |>
      mutate(bonds_issued_cnt = as.numeric(bonds_issued_cnt),
             bonds_redeemed_cnt = as.numeric(bonds_redeemed_cnt),
             bonds_out_cnt = as.numeric(bonds_out_cnt),
             bonds_matured_cnt = as.numeric(bonds_matured_cnt),
             bonds_unmatured_cnt = as.numeric(bonds_unmatured_cnt)) |>
      group_by(series_cd) |>
      filter(series_cd == input$BondType) |>
      # We will use the match.fun function to strip the quotes of the numeric summary supplied by the user
      # and turn it into a type of summary. We will apply this to all of the numeric columns.
      summarize(across(where(is.numeric), ~ match.fun(input$NumericalSummaryType)(.x, na.rm = TRUE)))
  })
  
  # Creating a bar graph
  output$barPlot <- renderPlot({
    # This bar graph will use data from the Gold Reserves data set
    url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/gold_reserve"
    final_data_GR <- API_Cleaning(url)
    # We will group this data by location, obtain the sum of the book value of gold for each location,
    # and then create a bar graph plotting location vs. book value of gold reserves there.
    g <- final_data_GR |>
      group_by(location_desc) |>
      # Creating the total book value variable
      summarize(total_book_value = sum(as.numeric(book_value_amt))) |>
      ggplot(aes(x = location_desc, y = total_book_value, fill = location_desc)) +
      geom_bar(stat = "identity") +
      labs(title = "Total Gold Reserve Book Value by Location",
           x = "Location",
           y = "Total Book Value (USD)",
           fill = "Location") +
      theme_minimal() +
      # Slanting the x-axis text so it will fit nicely on the plot
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
      # Taking the y-axis labels out of scientific notation
      scale_y_continuous(labels = scales::comma)
    # Adding facet wrap by location when the user selects to facet wrap
    if(input$FacetWrap == "Yes") {
      g + 
        facet_wrap(~location_desc)
    } else {
      g
    }
  })
  
  # Creating a line graph
  output$linePlot <- renderPlot({
    # For this line graph we will use data from the interest rates data set. We will look at the trend
    # of interest rates over time throughout 2001.
    url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/avg_interest_rates"
    final_data_IR <- API_Cleaning(url)
    
    # Pipe to filter to interest rates for treasury bonds
    final_data_IR |>
      mutate(record_date = as.Date(record_date),
             # Dividing the interest rates by 100 so they can be in percentage form
             avg_interest_rate_amt = as.numeric(avg_interest_rate_amt)/100) |>
      filter(security_desc == "Treasury Bonds") |>
      # Grouping by date
      group_by(record_date) |>
     ggplot(aes(x = record_date, y = avg_interest_rate_amt)) +
      geom_line(color = "blue") +
      labs(title = "Trend of Interest Rates of Treasury Bonds in 2001 Throughout Months",
           x = "Month",
           y = "Interest Rate of Treasury Bonds") +
      theme_classic() +
      # Having the y-axis labels as percentages
      scale_y_continuous(labels = scales::percent)
  })
  
  # Creating a swarm plot (new plot I have never done before)
  output$swarmPlot <- renderPlot({
    # For this plot we will use the bonds issued data
    url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/accounting/od/savings_bonds_report"
    final_data_BI <- API_Cleaning(url)
    # Making the necessary columns numeric
    final_data_BI <- final_data_BI |>
      mutate(bonds_issued_cnt = as.numeric(bonds_issued_cnt),
             bonds_redeemed_cnt = as.numeric(bonds_redeemed_cnt)) |>
      # Filtering out data that is the sum of all series'
      filter(!(series_cd == ("null")))
   ggplot(final_data_BI, aes(x = series_desc, y = bonds_issued_cnt, color = series_desc)) +
      geom_beeswarm() +
      labs(title = "Number of Bonds Issued for Each Bond",
           x = "Type of Bond Issued",
           y = "Bonds Issued",
           color = "Type of Bond") +
      theme_minimal() +
      # Slanting the x-axis labels so they will fit on the plot
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12)) +
      # Making the y-axis labels not Scientific notation
      scale_y_continuous(labels = scales::comma)
  })
  
  # Creating a scatter plot
  output$scatterPlot <- renderPlot({
    # We will create this plot using the bonds issued data
    url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/accounting/od/savings_bonds_report"
    final_data_BI <- API_Cleaning(url)
    final_data_BI <- final_data_BI |>
      mutate(bonds_issued_cnt = as.numeric(bonds_issued_cnt),
             bonds_out_cnt = as.numeric(bonds_out_cnt)) |> 
      # Filtering out the largest groups of bonds issued so the graph looks better
      filter(!(series_cd %in% c("null", "E", "EE", "HH", "I", "SN")))
    
    # Creating the scatter plot of bonds issued vs. bonds outstanding for each type of bond
   ggplot(final_data_BI, aes(x = bonds_issued_cnt, y = bonds_out_cnt, color = series_desc)) +
      geom_point() +
      labs(title = "Total Bonds Issued vs. Bonds Outstanding Colored by Type",
           x = "Total Bonds Issued",
           y = "Bonds Outstanding",
           color = "Type of Bond") +
      theme_minimal() +
      # Making labels for both axes to not be in scientific notation
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::comma)
  })
})
