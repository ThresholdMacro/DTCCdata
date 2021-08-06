source(here::here("R/Bootstrap.R"))
source(here::here("R/DataIngestion.R"))
SwapsTRAnalysis <- function(date) {
  message("*** Downloading the data from the trade repository ***")

  swaps.portfolio <- SwapsFromDTCC(date) |> 
    dplyr::bind_rows(SwapsFromCME(date))
  
  message("*** Data from the trade repository downloaded ***")
  swap.curve <- swaps.portfolio |> 
    dplyr::mutate(start.date = as.Date(start.date, format = "%d/%m/%Y"),
                  maturity.date = as.Date(maturity.date, format = "%d/%m/%Y"),
                  time.to.mat = round((maturity.date - start.date)/365,0) |> 
                    as.numeric(),
                  Bucket = cut(
                    as.numeric(time.to.mat),
                    breaks = c(seq(from = 0.5,to = 12.5, by = 1), 
                               seq(from = 17.5,to = 52.5, by = 5)),
                    labels = c(1:12, seq(from = 15, to = 50, by = 5)),
                    right = FALSE),
                  Bucket = as.character(Bucket) |> as.numeric()) |> 
    dplyr::group_by(Bucket) |> 
    dplyr::summarise(Strike = median(strike))
  
  message("*** Bootstrapping the implied market curve ***")
  df.curve <- BootstrapCurve(date, swap.curve)
  
  message("*** Pricing the portfolio ***")
  priced.portfolio <- SwapPricer::SwapPortfolioPricing(swaps.portfolio, date, 
                                                       df.curve, 
                                                       duration.flag = TRUE) |> 
    dplyr::mutate(Bucket = cut(
      duration,
      breaks = c(0, 1, 3, 4, 5, 7, 10, 15, 20, 25, 30, 40, 50, 100),
      labels = c("0-1", "1-3", "3-4", "4-5", "5-7", "7-10", "10-15", "15-20", "20-25", "25-30", "30-40", "40-50", "50-100" ),
      right = FALSE))
  
  histogram <- priced.portfolio |> 
    dplyr::group_by(Bucket) |> 
    dplyr::summarise(pv01 = sum(pv01)) |> 
    ggplot(aes(x = Bucket, y = pv01, text = sprintf("PV01: %s", scales::comma(pv01)))) + 
    geom_col() + 
    theme_bw() + 
    labs(x = "Buckets", y = "PV01 Traded") + 
    scale_y_continuous(labels = scales::label_number(suffix = "m", scale = 1e-6)) 
  
  message("*** Portfolio Priced ***")
  return(list(priced.portfolio = priced.portfolio,
              swaps.portfolio = swaps.portfolio,
              histogram = histogram))
}