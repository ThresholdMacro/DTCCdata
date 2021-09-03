library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(glue)

dashboardPage(
  dashboardHeader(title = "Swap Trading Dashboard"),
  dashboardSidebar(width = "300px",
                   pickerInput(inputId = "dropdown_currency", 
                               "Choose the currency to visualize",
                               choices = currency.options, selected = FALSE,
                               multiple = FALSE),
                   pickerInput(inputId = "dropdown_rates", 
                               "Choose the rates to visualize",
                               choices = curve.options, 
                               options = list(`actions-box` = TRUE),
                               selected = "10",
                               multiple = TRUE),
                   materialSwitch(inputId = "OnOffExchange", 
                                  label = "Include only cleared transactions?",
                                  status = "primary",
                                  value = TRUE),
                   materialSwitch(inputId = "ForwardToggle", 
                                  label = "Include forward starting swaps?",
                                  status = "primary",
                                  value = FALSE),
                   pickerInput(inputId = "dropdown_tenors", 
                               "Choose the buckets to visualize",
                               choices = bucket.options, 
                               options = list(`actions-box` = TRUE),
                               selected = "10-15",
                               multiple = TRUE),
                   pickerInput(inputId = "dropdown_type", 
                               "Choose the metric to visualize",
                               choices = metric.options,
                               multiple = FALSE)
  ),
  dashboardBody(
    fluidRow(
      box(
        width = "100%",
        title = "Interest Rate Data",
        status = "primary",
        plotlyOutput("RatesGraph")
      )),
    fluidRow( 
      tabBox(
        width = "100%",
        title = "Trade Analysis",
        tabPanel("Trade Data", 
                 plotlyOutput("TradesData")),
        tabPanel("One Day Data", 
                 fluidRow(
                   dateInput("datepick", "Choose a Date",
                             value = as.Date("2021-01-04"),
                             min = as.Date("2021-01-04"),
                             max = as.Date("2021-01-15"))
                 ),
                 fluidRow(
                   column(6, plotlyOutput("histogram")),
                   column(6, plotlyOutput("trades"))
                 )
        )
      )
    )
  )
)