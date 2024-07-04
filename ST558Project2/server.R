library(shiny)
library(caret)
library(tidyverse)
library(DT)
data("GermanCredit")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$summary <- DT::renderDataTable({
    if (input$var == "Gold Reserve") {
      url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/gold_reserve"
      raw_data <- httr::GET(url)
      parsed_data <- fromJSON(rawToChar(raw_data$content))
      final_data <- as_tibble(parsed_data$data)
      final_data <- final_data |>
        select(record_date, facility_desc, form_desc, location_desc, fine_troy_ounce_qty, book_value_amt, record_calendar_year)
        if(input$GRFilter == "2012") {
          final_data <- final_data |>
            filter(record_calendar_year == "2012")
        }
        if (input$GRFilter == "2013") {
          final_data <- final_data |>
            filter(record_calendar_year == "2013")
        }
      return(final_data)
    } else if (input$var == "Balance Sheets") {
      url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/balance_sheets"
      raw_data <- httr::GET(url)
      parsed_data <- fromJSON(rawToChar(raw_data$content))
      final_data <- as_tibble(parsed_data$data)
      final_data <- final_data |>
        select(record_date, restmt_flag, account_desc, line_item_desc, position_bil_amt, record_calendar_year)
      return(final_data)
    } else if (input$var == "Interest Rates") {
      url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/avg_interest_rates"
      raw_data <- httr::GET(url)
      parsed_data <- fromJSON(rawToChar(raw_data$content))
      final_data <- as_tibble(parsed_data$data)
      final_data <- final_data |>
        select(record_date, security_type_desc, security_desc, avg_interest_rate_amt, record_calendar_year)
      return(final_data)
    } else if (input$var == "Electronic Securities") {
      url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/accounting/od/securities_redemptions"
      raw_data <- httr::GET(url)
      parsed_data <- fromJSON(rawToChar(raw_data$content))
      final_data <- as_tibble(parsed_data$data)
      final_data <- final_data |>
        select(record_date, security_type_desc, security_class_desc, securities_redeemed_cnt, securities_redeemed_amt, record_calendar_year)
      return(final_data)
    } else if (input$var == "Public Debt") {
      url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/debt/mspd/mspd_table_1"
      raw_data <- httr::GET(url)
      parsed_data <- fromJSON(rawToChar(raw_data$content))
      final_data <- as_tibble(parsed_data$data)
      final_data <- final_data |>
        select(record_date, security_type_desc, security_class_desc, debt_held_public_mil_amt, intragov_hold_mil_amt, total_mil_amt, record_calendar_year)
      return(final_data)
    } else if (input$var == "Bond Issues") {
      url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/accounting/od/savings_bonds_report"
      raw_data <- httr::GET(url)
      parsed_data <- fromJSON(rawToChar(raw_data$content))
      final_data <- as_tibble(parsed_data$data)
      final_data <- final_data |>
        select(record_date, series_cd, series_desc, bonds_issued_cnt, bonds_redeemed_cnt, bonds_out_cnt, bonds_matured_cnt, bonds_unmatured_cnt, record_calendar_year)
      return(final_data)
    }
    round <- input$round
    tab <- GermanCredit %>% 
      select("Class", "InstallmentRatePercentage", var) %>%
      group_by(Class, InstallmentRatePercentage) %>%
      summarize(mean = round(mean(get(var)), round))
    tab
  })
  
  output$barPlot <- renderPlot({
    
    g <- ggplot(GermanCredit, aes(x = Class))  
    
    if(input$plot == "bar"){
      g + geom_bar()
    } else if(input$plot == "sideUmemploy"){ 
      g + geom_bar(aes(fill = as.factor(EmploymentDuration.Unemployed)), position = "dodge") + 
        scale_fill_discrete(name = "Unemployment status", labels = c("Employed", "Unemployed"))
    } else if(input$plot == "sideForeign"){
      g + geom_bar(aes(fill = as.factor(ForeignWorker)), position = "dodge") + 
        scale_fill_discrete(name = "Status", labels = c("German", "Foreign"))
    }
  })
  
})
