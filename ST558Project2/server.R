library(shiny)
library(caret)
library(tidyverse)
library(DT)
data("GermanCredit")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  API_Cleaning <- function(url) {
    raw_data <- httr::GET(url)
    parsed_data <- fromJSON(rawToChar(raw_data$content))
    final_data <- as_tibble(parsed_data$data)
    return(final_data)
  }
  output$summary <- DT::renderDataTable({
    if (input$var == "Gold Reserve") {
      url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/gold_reserve"
      final_data <- API_Cleaning(url)
      final_data <- final_data |>
        select(all_of(input$GRColumns))
        if(input$GRFilter == "both") {
          final_data <- final_data
        }
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
      final_data <- API_Cleaning(url)
      final_data <- final_data |>
        select(all_of(input$BSColumns))
      if(input$BSFilter == "all") {
        final_data <- final_data
      }
      if(input$BSFilter == "1995") {
        final_data <- final_data |>
          filter(record_calendar_year == "1995")
      }
      if (input$BSFilter == "1996") {
        final_data <- final_data |>
          filter(record_calendar_year == "1996")
      }
      if (input$BSFilter == "1997") {
        final_data <- final_data |>
          filter(record_calendar_year == "1997")
      }
      return(final_data)
    } else if (input$var == "Interest Rates") {
      url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/accounting/od/avg_interest_rates"
      final_data <- API_Cleaning(url)
      final_data <- final_data |>
        select(all_of(input$IRColumns))
      return(final_data)
    } else if (input$var == "Electronic Securities") {
      url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/accounting/od/securities_redemptions"
      final_data <- API_Cleaning(url)
      final_data <- final_data |>
        select(all_of(input$ESColumns))
      if(input$ESFilter == "both") {
        final_data <- final_data
      }
      if(input$ESFilter == "2002") {
        final_data <- final_data |>
          filter(record_calendar_year == "2002")
      }
      if (input$ESFilter == "2003") {
        final_data <- final_data |>
          filter(record_calendar_year == "2003")
      }
      return(final_data)
    } else if (input$var == "Public Debt") {
      url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/debt/mspd/mspd_table_1"
      final_data <- API_Cleaning(url)
      final_data <- final_data |>
        select(all_of(input$PDColumns))
      return(final_data)
    } else if (input$var == "Bond Issues") {
      url <- "https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/accounting/od/savings_bonds_report"
      final_data <- API_Cleaning(url)
      final_data <- final_data |>
        select(all_of(input$BIColumns))
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
