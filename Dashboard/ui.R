library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
library(plotly)
library(glue)

dashboardPage(
  dashboardHeader(title = "Swap Trading Dashboard",
                  titleWidth = 300),
  dashboardSidebar(width = "300px",
                   pickerInput(inputId = "dropdown_currency", 
                               "Choose the currency to visualize",
                               choices = currency.options, 
                               selected = "USD",
                               multiple = FALSE,
                               choicesOpt = list(
                                 style = rep("color: black;", 4))),
                   pickerInput(inputId = "dropdown_rates", 
                               "Choose the rates to visualize",
                               choices = curve.options, 
                               options = list(`actions-box` = TRUE),
                               selected = "10",
                               multiple = TRUE,
                               choicesOpt = list(
                                 style = rep("color: black;", 17))),
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
                               multiple = TRUE,
                               choicesOpt = list(
                                 style = rep("color: black;", 12))),
                   pickerInput(inputId = "dropdown_type", 
                               "Choose the metric to visualize",
                               choices = metric.options,
                               multiple = FALSE,
                               choicesOpt = list(
                                 style = rep("color: black;", 2)))
  ),
  dashboardBody(
    fluidRow(
      box(
        width = "12 col-lg-12",
        title = "Interest Rate Data",
        plotlyOutput("RatesGraph")
      )),
    fluidRow( 
      tabBox(
        width = "12 col-lg-12",
        title = "Trade Analysis",
        side = "right",
        tabPanel("Historical Timeseries", 
                 plotlyOutput("TradesData")),
        tabPanel("Daily Data", 
                 fluidRow(
                   box(width = "6 col-lg-6",
                       plotlyOutput("histogram")),
                   box(width = "6 col-lg-6",
                       fluidRow(
                         column(6, dateInput("datepick", "Choose a Date",
                                             value = as.Date("2021-01-04"),
                                             min = as.Date("2021-01-04"))),
                         column(6, textOutput("accuracy"),
                                style = "text-align: center; padding:30px;")
                       ),
                       fluidRow(plotlyOutput("trades")))
                 )
        )
      )
    )
  )
)