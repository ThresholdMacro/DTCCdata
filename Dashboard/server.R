#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

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
  
  output$histogram <- renderPlotly(PlotHistogram(buckets.distribution(), 
                                                 input$dropdown_type))
  
  output$TradesData <- renderPlotly(PlotHistoryTrades(pricing.filter(), 
                                                      input$dropdown_tenors,
                                                      input$dropdown_type)) 
  
  output$RatesGraph <- renderPlotly(PlotCurve(selected.rates()))
  
  output$trades <- renderPlotly(PlotTradesAndCurve(pricing(), curve(),
                                                   input$datepick))
  
  
})
