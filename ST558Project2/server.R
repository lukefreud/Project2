library(shiny)
library(caret)
library(tidyverse)
library(DT)

shinyServer(function(input, output) {
  # Function to clean each URL
  API_Cleaning <- function(url) {
    raw_data <- httr::GET(url)
    parsed_data <- fromJSON(rawToChar(raw_data$content))
    final_data <- as_tibble(parsed_data$data)
    return(final_data)
  }
  
  
  
  data <- reactive(
    if (input$var == "Gold Reserve") {
      url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/gold_reserve"
      final_data <- API_Cleaning(url)
      final_data <- final_data |>
        select(all_of(input$GRColumns), record_calendar_year)
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
    } else if (input$var == "Interest Rates") {
      url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/avg_interest_rates"
      final_data <- API_Cleaning(url)
      final_data_IR <- final_data |>
        select(all_of(input$IRColumns), record_calendar_year)
      return(final_data_IR)
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
    } else if (input$var == "Public Debt") {
      url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/debt/mspd/mspd_table_1"
      final_data <- API_Cleaning(url)
      final_data_PD <- final_data |>
        select(all_of(input$PDColumns), record_calendar_year)
      return(final_data_PD)
    } else if (input$var == "Bond Issues") {
      url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/accounting/od/savings_bonds_report"
      final_data <- API_Cleaning(url)
      final_data_BI <- final_data |>
        select(all_of(input$BIColumns), record_calendar_year)
      return(final_data_BI)
    }
  )
  output$summary <- DT::renderDataTable({
    data()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      'US-Treasury-Data.csv'
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  output$contingencyTable <- renderTable({
    url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/gold_reserve"
    final_data_GR <- API_Cleaning(url)
    selected_categorical_data <- final_data_GR |>
      select(all_of(input$CategoricalSummaries))
    table(selected_categorical_data)
  })
  output$NumericalSummaries <- renderTable({
    url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/gold_reserve"
    final_data_GR <- API_Cleaning(url)
    final_data_GR$fine_troy_ounce_qty <- as.numeric(final_data_GR$fine_troy_ounce_qty)
    final_data_GR$book_value_amt <- as.numeric(final_data_GR$book_value_amt)
    selected_numerical_data <- final_data_GR %>%
      select(all_of(input$NumericalSummaries))
    
    if (input$NumericalSummaryType == "mean") {
      summary_value <- list("Mean" = mean(selected_numerical_data[[1]]))
    } else if (input$NumericalSummaryType == "median") {
      summary_value <- list("Median" = median(selected_numerical_data[[1]]))
    } else if (input$NumericalSummaryType == "sd") {
      summary_value <- list("SD" = sd(selected_numerical_data[[1]]))
    } else if (input$NumericalSummaryType == "max") {
      summary_value <- list("Max" = max(selected_numerical_data[[1]]))
    } else if (input$NumericalSummaryType == "min") {
      summary_value <- list("Min" = min(selected_numerical_data[[1]]))
    }

    summary_value
  })
  output$barPlot <- renderPlot({
    url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/gold_reserve"
    final_data_GR <- API_Cleaning(url)
    
    final_data_GR |>
      group_by(location_desc) |>
      summarize(total_book_value = sum(as.numeric(book_value_amt))) |>
      ggplot(aes(x = location_desc, y = total_book_value, fill = location_desc)) +
      geom_bar(stat = "identity") +
      labs(title = "Total Gold Reserve Book Value by Location",
           x = "Location",
           y = "Total Book Value (USD)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
      scale_y_continuous(labels = scales::comma)
  })
  
  output$linePlot <- renderPlot({
    url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/avg_interest_rates"
    final_data_IR <- API_Cleaning(url)
    
    final_data_IR %>%
      mutate(record_date = as.Date(record_date),
             avg_interest_rate_amt = as.numeric(avg_interest_rate_amt)/100) %>%
      filter(security_desc == "Treasury Bonds") %>%
      group_by(record_date) %>%
      ggplot(aes(x = record_date, y = avg_interest_rate_amt)) +
      geom_line(color = "blue") +
      labs(title = "Trend of Interest Rates of Treasury Bonds in 2001 Throughout Months",
           x = "Month",
           y = "Interest Rate of Treasury Bonds") +
      theme_classic() +
      scale_y_continuous(labels = scales::percent)
  })
  
  output$boxPlot <- renderPlot({
  })
  
  output$scatterPlot <- renderPlot({
    url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/accounting/od/savings_bonds_report"
    final_data_BI <- API_Cleaning(url)
    # Assuming the data is cleaned and stored in final_data_Bonds
    # Convert necessary columns to numeric
    final_data_BI <- final_data_BI |>
      mutate(bonds_issued_cnt = as.numeric(bonds_issued_cnt),
             bonds_out_cnt = as.numeric(bonds_out_cnt)) |> 
      filter(!(series_cd %in% c("null", "E", "EE", "HH", "I", "SN")))
    
    # Create scatter plot
    ggplot(final_data_BI, aes(x = bonds_issued_cnt, y = bonds_out_cnt, color = series_desc)) +
      geom_point() +
      labs(title = "Scatterplot of Total Bonds Issued vs. Bonds Outstanding Colored by Series",
           x = "Total Bonds Issued",
           y = "Bonds Outstanding",
           color = "Series Description") +
      theme_minimal() +
      scale_x_continuous(labels = scales::comma) + # Format x-axis labels
      scale_y_continuous(labels = scales::comma)   # Format y-axis labels
    
  })
})
