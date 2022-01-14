#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(openxlsx)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # Reactive elements -------------------------------------------------------
  
  ois.flag <- reactive({
    ifelse(input$TypeSelector %in% "OIS", TRUE, FALSE) 
  })
  
  ois.flag.daily <- reactive({
    ifelse(input$OISSelector %in% "OIS", TRUE, FALSE) 
  })
  
  cleared.flag <- reactive({
    1 |> 
      union(ifelse(input$OnOffExchange, 1, 0)) |> 
      unique()
  })
  
  forward.starting.flag <- reactive({
    as.numeric(input$ForwardToggle) |> 
      union(0) |> 
      unique()
  })
  
  pricing <- reactive({
    GetPricingCombined(cleared.flag(), forward.starting.flag(), 
                       input$dropdown_currency, input$TypeSelector)
  })  
  
  pricing.unique <- reactive({
    GetPricing(cleared.flag(), forward.starting.flag(), 
               input$dropdown_currency, ois.flag.daily())
  })
  
  pricing.filter <- reactive({
    SummarisePricing(pricing())
  })
  
  pricing.filter.unique <- reactive({
    SummarisePricing(pricing.unique())
  })
  
  curve <- reactive({
    GetCurveCombined(input$dropdown_currency, input$TypeSelector) 
  })  
  
  selected.rates <- reactive({
    CalculateRates(curve(), input$dropdown_rates)
  })
  
  buckets.distribution <- reactive({
    GetBucketDistribution(pricing.filter.unique(), input$datepick, 
                          bucket.options, input$OISSelector)
  }) 
  
  accuracy <- reactive({
    GetAccuracy(input$date_curve, input$dropdown_currency, ois.flag.daily())
  })
  
  histogram <- reactive({PlotHistogram(buckets.distribution(), 
                                       input$dropdown_type)})
  
  trades <- reactive({
    PlotTradesAndCurve(pricing.unique(), curve(), input$date_curve,
                       input$OISSelector)
  })
  
  plotcurve <- reactive({
    PlotCurve(selected.rates()$result,
              input$date_graphs)
  })
  
  tradesdata <- reactive({
    PlotHistoryTrades(pricing.filter(), 
                      input$dropdown_tenors,
                      input$dropdown_type,
                      input$date_graphs)
  })
  
  # Observers -------------------------------------------------------------
  
  observe({
    updateDateRangeInput(session, "date_graphs",
                         start = 
                           as.character(format(as.Date(min(pricing()$spot.date))),
                                        "yyyy-mm-dd"), # Start 
                         end = 
                           as.character(format(as.Date(max(pricing()$spot.date))),
                                        "yyyy-mm-dd"), # End 
                         min = 
                           as.character(format(as.Date(min(pricing()$spot.date))),
                                        "yyyy-mm-dd"),
                         max = 
                           as.character(format(as.Date(max(pricing()$spot.date))),
                                        "yyyy-mm-dd"))
  })
  
  observe({
    updateDateInput(session, "date_curve",
                    value = 
                      as.character(format(as.Date(min(curve()$curve.date))),
                                   "yyyy-mm-dd"),
                    min = 
                      as.character(format(as.Date(min(curve()$curve.date))),
                                   "yyyy-mm-dd"),
                    max = 
                      as.character(format(as.Date(max(curve()$curve.date))),
                                   "yyyy-mm-dd"))
  })
  
  
  # output$dateRange <- renderUI({
  #   dateRangeInput("date_graphs", "Select the date range:",
  #                  start = 
  #                    as.character(format(as.Date(min(pricing()$spot.date))),
  #                                 "yyyy-mm-dd"), # Start 
  #                  end = 
  #                    as.character(format(as.Date(max(pricing()$spot.date))),
  #                                 "yyyy-mm-dd"), # End 
  #                  min = 
  #                    as.character(format(as.Date(min(pricing()$spot.date))),
  #                                 "yyyy-mm-dd"),
  #                  max = 
  #                    as.character(format(as.Date(max(pricing()$spot.date))),
  #                                 "yyyy-mm-dd"),
  #                  format = "yyyy-mm-dd")
  #   
  # })
  
  # output$dateCurve <- renderUI({
  #   dateInput("date_curve", "Select the date range:",
  #             value = 
  #               as.character(format(as.Date(min(curve()$curve.date))),
  #                            "yyyy-mm-dd"),
  #             min = 
  #               as.character(format(as.Date(min(curve()$curve.date))),
  #                            "yyyy-mm-dd"),
  #             max = 
  #               as.character(format(as.Date(max(curve()$curve.date))),
  #                            "yyyy-mm-dd"),
  #             format = "yyyy-mm-dd")
  #   
  # })
  
  
  # Output ------------------------------------------------------------------
  
  output$error_text <- renderText({"Please select at least one interest rate metric"})
  
  output$error_text_tenor <- renderText({"Please select at least one bucket"})
  
  output$histogram <- renderPlotly(histogram()$plot)
  
  output$TradesData <- renderPlotly(tradesdata()$plot) 
  
  output$RatesGraph <- renderPlotly(plotcurve()$plot)
  
  output$trades <- renderPlotly(trades()$plot)
  outputOptions(output, "trades", suspendWhenHidden = TRUE) 
  
  output$accuracy <- renderText(glue::glue("The pricing accuracy is {sprintf('%.3f', accuracy())} \\
                                            times the pv01"))
  
  date <- reactive({output$datepick})
  
  output$Download <- downloadHandler(
    filename <- function() {
      "TradeRepositoryData.xlsx"
    },
    content <- function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, sheetName = "Interest Rates")
      writeData(wb, sheet = "Interest Rates", plotcurve()$data, rowNames = FALSE) 
      addWorksheet(wb, sheetName = "Trading Data")
      writeData(wb, sheet = "Trading Data", tradesdata()$data, rowNames = FALSE) 
      addWorksheet(wb, sheetName = "Distribution")
      writeData(wb, sheet = "Distribution", histogram()$data, rowNames = FALSE) 
      addWorksheet(wb, sheetName = "Trades and Pricing Curve")
      writeData(wb, sheet = "Trades and Pricing Curve", 
                trades()$data, rowNames = FALSE) 
      saveWorkbook(wb, file, overwrite = TRUE)
    })
  
})
