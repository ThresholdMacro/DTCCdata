currency.options <- c("EUR", "GBP", "JPY", "USD")

bucket.options <- list(
  "Short Term" = list("0-1", "1-3"),
  "Medium Term" = list("3-4", "4-5", "5-7", "7-10"),
  "Long Term" = list("10-15", "15-20", "20-25", "25-30",
                     "40-50", "50-100"))



curve.options <- list(
  "Rates" = as.list(c(1:5, 7, 10, 15, seq(from = 20,
                                          to = 50, by = 10))),
  "Spreads" = list("1s5s", "2s10s", "5s20s"),
  "Butterflies" = list("2s5s10s", "2s10s30s"))

metric.options <- c("Notional" = "notional", "Risk" = "pv01")


# DB Functions ------------------------------------------------------------

ConnectToDB <- function(){
  db_user <- 'root'
  db_password <- 'Scr@0p1960'
  db_name <- 'trade_repo_swap_data'
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
    dplyr::filter(currency %in% cur)
  
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


# Functions ---------------------------------------------------------------

SummarisePricing <- function(priced.portfolio) {
  priced.portfolio  |> 
    dplyr::group_by(spot.date, currency, Bucket) |>  
    dplyr::summarise(notional = sum(notional),
                     pv01 = sum(pv01),
                     .groups = "keep") |> 
    dplyr::ungroup() |> 
    dplyr::filter(spot.date >= as.Date("2021-01-01")) 
}

CalculateDerivedMetric <- function(metric, metric.char, histo.rates) {
  rates <- metric |> 
    as.vector() |> 
    as.numeric()
  
  histo.rates <- histo.rates |> 
    dplyr::filter(Bucket %in% rates) |> 
    
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
  
  stringr::str_match_all(metric, "[:digit:]+") |> 
    purrr::map2_dfr(as.list(metric), ~CalculateDerivedMetric(.x, .y, histo.rates))
}

GetBucketDistribution <- function(priced.portfolio, date, 
                                  bucket.options) {

  priced.portfolio <- priced.portfolio |> 
    dplyr::filter(spot.date %in% date)
  
  bucketed.results <- bucket.options |> 
    purrr::flatten() |> 
    unlist() |> 
    {\(buckets) tibble::tibble(Bucket = buckets)}() |> 
    dplyr::left_join(priced.portfolio, by = "Bucket") |> 
    tidyr::replace_na(list(notional = 0, pv01 = 0))
  
  return(bucketed.results)
}


# Plot Functions ----------------------------------------------------------

PlotHistogram <- function(buckets.distribution, input) {

  buckets.distribution |> 
    ggplot(aes(x = forcats::fct_inorder(Bucket, ordered = TRUE), 
               y = !!sym(input))) +
    geom_col() +
    theme_bw() +
    labs(x = "Buckets", y = paste0(stringr::str_to_sentence(input), " Traded")) +
    scale_y_continuous(labels = scales::label_number(suffix = "m", scale = 1e-6))
}

PlotHistoryTrades <- function(priced.portfolio, bucket, input) {

  priced.portfolio |> 
    dplyr::filter(Bucket %in% bucket) |> 
    ggplot(aes_string(x = "spot.date", y = input, colour = "Bucket")) +
    geom_point() + 
    geom_line()+
    theme_bw() +
    labs(x = "Date", y = paste0(stringr::str_to_sentence(input), " Traded"), 
         col = "Duration Bucket") +
    scale_y_continuous(labels = scales::label_number(suffix = "m", scale = 1e-6))
}

PlotCurve <- function(curve) {
  curve |> 
    ggplot(aes(x = curve.date, y = rate, colour = metric)) +
    geom_point() + 
    geom_line() +
    scale_y_continuous(labels = scales::percent)+
    labs(x = "Date", y = "Rate", col = "Interest Rate Type") +
    theme_bw()
  
}

PlotTradesAndCurve <- function(priced.portfolio, curve, date) {
  curve <- curve |> 
    dplyr::filter(curve.date %in% date)
 
  priced.portfolio |> 
    dplyr::filter(spot.date %in% date) |> 
    dplyr::select(ID, time.to.mat, strike, cleared , forward.starting, outlier) |> 
    dplyr::mutate(type = dplyr::case_when(
      outlier == 1 ~ "Outlier removed", 
      cleared == 1 & forward.starting == 0 ~ "Cleared and spot starting", 
      TRUE ~ "Non cleared and/or forward starting" )) |> 
    ggplot(aes(x = time.to.mat, y = strike, colour = type)) + 
    geom_point(alpha = 0.2) + 
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Tenor", y = "Interest Rate Level", col = "Type of trade") +
    geom_point(data = curve, aes(x = Bucket, y = Strike, colour = "Pricing Rate")) + 
    theme(legend.position="bottom")
}
