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
shinyServer(function(input, output) {
  
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
    GetPricing(cleared.flag(), forward.starting.flag(), input$dropdown_currency)
  })  
  
  pricing.filter <- reactive({
    SummarisePricing(pricing())
  })
  
  curve <- reactive({
    GetCurve(input$dropdown_currency) 
  })  
  
  selected.rates <- reactive({
    CalculateRates(curve(), input$dropdown_rates)
  })
  
  buckets.distribution <- reactive({
    GetBucketDistribution(pricing.filter(), input$datepick, 
                          bucket.options)
  }) 
  
  accuracy <- reactive({
    GetAccuracy(input$datepick, input$dropdown_currency)
  })
  
  output$error_text <- renderText({"Please select at least one interest rate metric"})
  
  output$error_text_tenor <- renderText({"Please select at least one bucket"})
  
  histogram <- reactive({PlotHistogram(buckets.distribution(), 
                                       input$dropdown_type)})
  
  output$histogram <- renderPlotly(histogram()$plot)
  
  tradesdata <- reactive({
    PlotHistoryTrades(pricing.filter(), 
                      input$dropdown_tenors,
                      input$dropdown_type)
  })
  
  output$TradesData <- renderPlotly(tradesdata()$plot) 
  
  plotcurve <- reactive({
    PlotCurve(selected.rates()$result)
  })
  
  output$RatesGraph <- renderPlotly(plotcurve()$plot)
  
  trades <- reactive({
    PlotTradesAndCurve(pricing(), curve(), input$datepick)
  })
  
  output$trades <- renderPlotly(trades()$plot)
  
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
