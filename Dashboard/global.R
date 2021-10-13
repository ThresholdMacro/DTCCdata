options(shiny.reactlog = TRUE)

currency.options <- c("EUR", "GBP", "JPY", "USD")

bucket.options <- list(
  "Short Term" = list("0-1", "1-3"),
  "Medium Term" = list("3-4", "4-5", "5-7", "7-10"),
  "Long Term" = list("10-15", "15-20", "20-25", "25-30", "30-40",
                     "40-50", "50-100"))



curve.options <- list(
  "Rates" = as.list(c(1:5, 7, 10, 15, seq(from = 20,
                                          to = 50, by = 10))),
  "Spreads" = list("1s5s", "2s10s", "5s20s"),
  "Butterflies" = list("2s5s10s", "2s10s30s"))

metric.options <- c("Notional" = "notional", "Risk" = "pv01")


# DB Functions ------------------------------------------------------------

ConnectToDB <- function(){
  db_user <- Sys.getenv("user")
  db_password <-  Sys.getenv("password")
  db_name <- Sys.getenv("dbname")
  db_host <- 'localhost'
  db_port <- 3306
  
  mydb <-  RMySQL::dbConnect(RMySQL::MySQL(), user = db_user, 
                             password = db_password, dbname = db_name, 
                             host = db_host, port = db_port)
}



GetPricing <- function(cleared.flag, forward.starting.flag, cur) {
  con <- ConnectToDB()

  data <- tibble::as_tibble(DBI::dbReadTable(con, "pricing_results")) |> 
    dplyr::mutate(spot.date = as.Date(spot.date, format = "%d/%m/%Y")) |>
    dplyr::filter(cleared %in% cleared.flag,
                  forward.starting %in% forward.starting.flag)|> 
    dplyr::filter(currency %in% cur,
                  spot.date >= as.Date("2021-01-01"))
  
  DBI::dbDisconnect(con)
  return(data)
}


GetAccuracy <- function(chosen.date, cur) {
  con <- ConnectToDB()
  
  data <- tibble::as_tibble(DBI::dbReadTable(con, "accuracy")) |> 
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::filter(currency %in% cur,
                  date %in% chosen.date) |> 
    dplyr::pull(accuracy)
  
  DBI::dbDisconnect(con)
  return(data)
}

GetCurve <- function(cur) {
  con <- ConnectToDB()
  data <- tibble::as_tibble(DBI::dbReadTable(con, "pricing_curve")) |> 
    dplyr::mutate(curve.date = as.Date(curve.date)) |> 
    dplyr::filter(currency %in% cur) 
  
  DBI::dbDisconnect(con)
  return(data)
}


# Calculation Functions ---------------------------------------------------------------

SummarisePricing <- function(priced.portfolio) {
  priced.portfolio  |> 
    dplyr::group_by(spot.date, currency, Bucket) |>  
    dplyr::summarise(notional = sum(notional),
                     pv01 = sum(pv01),
                     .groups = "keep") |> 
    dplyr::ungroup()  
}

CalculateDerivedMetric <- function(metric, metric.char, histo.rates) {
  rates <- metric |> 
    as.vector() |> 
    as.numeric()
  
  histo.rates <- histo.rates |> 
    dplyr::filter(Bucket %in% rates) |> 
    dplyr::arrange(curve.date) |> 
    tidyr::pivot_wider(names_from = Bucket, values_from = Strike,
                       names_prefix = "Rate_") |> 
    dplyr::mutate(metric = metric.char)  |>  
    {\(x, y) if(length(y) == 1) {
      dplyr::mutate(x, rate = x[[glue("Rate_{y[1]}")]])
    } else {
      if(length(y) == 2) {
        dplyr::mutate(x, rate = x[[glue("Rate_{y[2]}")]] - x[[glue("Rate_{y[1]}")]])
      } else {
        dplyr::mutate(x, rate = x[[glue("Rate_{y[3]}")]] - 2*x[[glue("Rate_{y[2]}")]] + x[[glue("Rate_{y[1]}")]])
      }
    }}(rates) |>  
    dplyr::select(curve.date, metric, rate)
  
  return(histo.rates)
}


CalculateRates <- function(histo.rates, metric) {
  
  if (!is.null(metric)) {
    result <- stringr::str_match_all(metric, "[:digit:]+") |> 
      purrr::map2_dfr(as.list(metric), ~CalculateDerivedMetric(.x, .y, histo.rates))
    flag <- TRUE
  } else {
    
    result <- NULL
    flag <- FALSE
  }
  return(list(result = result, flag = flag))
}

FillBuckets <- function(priced.portfolio, bucket.options) {
  bucket.options |> 
    purrr::flatten() |> 
    unlist() |> 
    {\(buckets) tibble::tibble(Bucket = buckets)}() |> 
    dplyr::left_join(priced.portfolio, by = "Bucket") |> 
    tidyr::replace_na(list(notional = 0, pv01 = 0))
}


GetBucketDistribution <- function(priced.portfolio, date, 
                                  bucket.options) {
  
  bucketed.results <- priced.portfolio |> 
    dplyr::group_nest(spot.date) |> 
    dplyr::mutate(bucketed.results = purrr::map(data, FillBuckets, 
                                                bucket.options)) |> 
    dplyr::select(-data) |> 
    tidyr::unnest(bucketed.results)
  
  return(bucketed.results)
}


# Plot Functions ----------------------------------------------------------

PlotHistogram <- function(buckets.distribution, input) {
  
  data <- buckets.distribution |> 
    dplyr::mutate(Bucket = forcats::fct_inorder(Bucket, ordered = TRUE)) 
  
  plot <- data |> 
    plot_ly() |> 
    add_bars(x = ~Bucket, y = ~get(input), frame = ~spot.date,
             showlegend = FALSE) |> 
    layout(yaxis = list(title = paste0(stringr::str_to_sentence(input), 
                                       " Traded")),
           xaxis = list(title = ""),
           annotations = 
             list(x = 1, y = -0.2, text = "Source: DTCC and CME", 
                  showarrow = F, xref='paper', yref='paper', 
                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                  font=list(size=13, color="black")))
  
  return(list(data = data, plot = plot))
  
}

PlotHistoryTrades <- function(priced.portfolio, bucket, input, dates) {

  priced.portfolio <- priced.portfolio |> 
    dplyr::filter(Bucket %in% bucket,
                  spot.date >= as.Date(dates[1]) & spot.date <= as.Date(dates[2])) |> 
    dplyr::arrange(spot.date)

  moving.average <- priced.portfolio |> 
    dplyr::group_by(spot.date) |> 
    dplyr::summarise(value = sum(!!sym(input))) |> 
    mutate(ma = slider::slide_dbl(value, 
                                  ~ mean(.x, na.rm = TRUE),
                                  .before = 5)) |> 
    dplyr::select(-value)
  
  plot <- plot_ly(priced.portfolio) |>  
    add_bars(x = ~spot.date, y = ~get(input), color = ~Bucket, 
             opacity = 0.2) |> 
    add_lines(data = moving.average, x = ~spot.date, y = ~ma,
              showlegend = FALSE) |> 
    layout(legend = list(title = list(text = "Duration Bucket"),
                         orientation = 'h'),
           yaxis = list(title = paste0(stringr::str_to_sentence(input), 
                                       " Traded")),
           xaxis = list(title = "Date",
                        range = c(min(priced.portfolio$spot.date) - 1, 
                                  max(priced.portfolio$spot.date) + 1),
                        rangebreaks=list(
                          list(bounds=list("sat", "mon")))),
           annotations = 
             list(x = 1, y = -0.2, text = "Source: DTCC and CME", 
                  showarrow = F, xref='paper', yref='paper', 
                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                  font=list(size=13, color="black")), barmode = 'stack') |> 
    config(displayModeBar = FALSE)
  
  return(list(data = priced.portfolio, plot = plot))
  
}

PlotCurve <- function(curve, dates) {
  if(!is.null(curve)) {
    data <- curve |> 
      dplyr::filter(curve.date >= as.Date(dates[1]) & 
                      curve.date <= as.Date(dates[2])) |> 
      dplyr::arrange(curve.date) 
    
    plot <- data |> 
      plot_ly(type = "scatter", mode = "lines+markers") |>  
      add_trace(x = ~curve.date, y = ~rate, color = ~metric) |> 
      layout(legend = list(title = list(text = "Interest Rate"),
                           orientation = 'h'),
             yaxis = list(title = "Rate",
                          tickformat= ".3%"),
             xaxis = list(title = "Date",
                          range = c(min(data$curve.date) - 1,
                                    max(data$curve.date) + 1),
                          rangebreaks=list(
                            list(bounds=list("sat", "mon")))),
             annotations = 
               list(x = 1, y = -0.2, text = "Source: DTCC and CME", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
                    font=list(size=13, color="black")))
    return(list(data = data, plot = plot))
  } 
}

PlotTradesAndCurve <- function(priced.portfolio, curve, date) {
  
  curve <- curve |> 
    dplyr::filter(curve.date %in% date)
  
  data <- priced.portfolio |> 
    dplyr::filter(spot.date %in% date) |> 
    dplyr::select(spot.date, time.to.mat, strike, cleared, forward.starting, 
                  outlier) |> 
    dplyr::mutate(type = dplyr::case_when(
      outlier == 1 ~ "Outlier removed", 
      cleared == 1 & forward.starting == 0 ~ "Cleared and spot starting", 
      TRUE ~ "Non cleared and/or forward starting" )) 

  plot <- data |> 
    plot_ly(type = "scatter", mode = "markers") |> 
    add_trace(x = ~time.to.mat, y = ~strike, color = ~type,
              opacity = 0.5) |> 
    add_trace(data = curve, x = ~Bucket, y = ~Strike, name = "Pricing Rate",
              marker = list(size = 8), color = "red") |>
    layout(legend = list(title = list(text = "Type of trade"),
                         orientation = 'h',
                         y=-0.2),
           yaxis = list(title = "Interest Rate Level",
                        tickformat= ".3%"),
           xaxis = list(title = "Tenor"),
           annotations = 
             list(x = 1, y = -0.2, text = "Source: DTCC and CME", 
                  showarrow = F, xref='paper', yref='paper', 
                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                  font=list(size=13, color="black")))
  
  data <- curve |> 
    dplyr::mutate(cleared = 0, forward.starting = 0, outlier = 0, 
                  type = "Pricing Rate") |> 
    dplyr::select(spot.date = curve.date, time.to.mat = Bucket,
                  strike = Strike, dplyr::everything()) |> 
    dplyr::select(-currency) |> 
    dplyr::bind_rows(data)

  return(list(data = data, plot = plot))
}
