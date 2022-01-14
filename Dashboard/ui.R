library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
library(plotly)
library(glue)

dashboardPage(
  dashboardHeader(title = "Swap Trading Dashboard - Hedge Analytics Ltd",
                  titleWidth = 500),
  dashboardSidebar(width = "300px",
                   checkboxGroupButtons(inputId = "TypeSelector",
                                        label = "Choose the type of swaps to include",
                                        choices = c("Libor", "OIS"),
                                        justified = TRUE,
                                        selected = c("Libor", "OIS"),
                                        checkIcon = list(
                                          yes = icon("ok", 
                                                     lib = "glyphicon"))),
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
                               selected = "7-10",
                               multiple = TRUE,
                               choicesOpt = list(
                                 style = rep("color: black;", 12))),
                   pickerInput(inputId = "dropdown_type", 
                               "Choose the metric to visualize",
                               choices = metric.options,
                               multiple = FALSE,
                               choicesOpt = list(
                                 style = rep("color: black;", 2))),
                   dateRangeInput("date_graphs", "Select the date range:",
                                  format = "yyyy-mm-dd",
                                  start = "2021-01-01",
                                  end = Sys.Date(),
                                  min = "2021-01-01",
                                  max = Sys.Date()),
                   conditionalPanel("input.dropdown_rates.length > 0 &&
                                    input.dropdown_tenors.length > 0",
                                    downloadButton("Download",
                                                   "Download the data selected",
                                                   style = "margin-top:15px;
                                                   margin-left:45px;"))
  ),
  dashboardBody(
    fluidRow(
      box(
        width = "12 col-lg-12",
        title = "Interest Rate Data",
        conditionalPanel("input.dropdown_rates.length == 0 || input.TypeSelector.length == 0",
                         textOutput("error_text")),
        conditionalPanel("input.dropdown_rates.length > 0 && input.TypeSelector.length > 0",
                         plotlyOutput("RatesGraph"))
      )),
    fluidRow( 
      tabBox(
        width = "12 col-lg-12",
        title = "Trade Analysis",
        side = "right",
        tabPanel("Historical Timeseries", 
                 conditionalPanel("input.dropdown_tenors.length == 0 || input.TypeSelector.length == 0",
                                  textOutput("error_text_tenor")),
                 conditionalPanel("input.dropdown_tenors.length > 0 && input.TypeSelector.length > 0",
                                  plotlyOutput("TradesData"))),
        tabPanel("Daily Data",
                 fluidRow(
                   column(width = 12, align = 'center',
                          pickerInput(
                            inputId = "OISSelector",
                            label = "Choose the type of swap", 
                            choices = c("Libor", "OIS"),
                            selected = "Libor"
                          ))
                 ),
                 fluidRow(
                   box(width = "6 col-lg-6",
                       plotlyOutput("histogram")),
                   box(width = "6 col-lg-6",
                       fluidRow(
                         column(6, dateInput("date_curve", "Select the date range:",
                                             format = "yyyy-mm-dd",
                                             value = Sys.Date(),
                                             min = "2021-01-01",
                                             max = Sys.Date()),),
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