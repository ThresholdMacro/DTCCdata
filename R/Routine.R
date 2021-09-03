source(here::here("R/Bootstrap.R"))
source(here::here("R/DataIngestion.R"))
library(ggplot2)

`%notin%` <- Negate(`%in%`)

SwapsTRAnalysis <- function(date, currency) {
  
  logr::log_print(glue::glue("*** Analysing Day {date} and currency {currency} ***"))
  
  original.data.dtcc <- DownloadFromDTCC(date) |> 
    dplyr::distinct()
  
  original.data.cme <- DownloadFromCME(date)|> 
    dplyr::distinct()
  
  swaps.portfolio <- SwapsFromDTCC(date, currency) |> 
    dplyr::bind_rows(SwapsFromCME(date, currency))

  if (nrow(swaps.portfolio) > 0) {
    message("*** Data from the trade repository downloaded ***")
    
    swap.curve <- swaps.portfolio |> 
      dplyr::filter(forward.starting == 0,
                    cleared == 1)

    if(nrow(swap.curve) > 0) {
      swap.curve <- swap.curve |> 
      dplyr::mutate(Bucket = cut(
        as.numeric(time.to.mat),
        breaks = c(seq(from = 0.5,to = 12.5, by = 1), 
                   seq(from = 17.5,to = 52.5, by = 5)),
        labels = c(1:12, seq(from = 15, to = 50, by = 5)),
        right = TRUE),
        Bucket = as.character(Bucket) |> as.numeric()) 
      
      message("*** Outlier Detection ***")
      
      if (nrow(swap.curve) > 1) {
        outlier <- swap.curve |> 
          dplyr::select(time.to.mat, strike) |> 
          outForest::outForest() |> 
          purrr::pluck("outliers") |> 
          dplyr::filter(col %in% "strike") |> 
          dplyr::pull(row)
        
        outlier.ID <- swap.curve[outlier, "ID"] |> 
          dplyr::pull("ID") |> 
          as.character()
        
        swaps.portfolio <- swaps.portfolio |> 
          dplyr::mutate(outlier = dplyr::if_else(ID %in% outlier.ID,
                                                 TRUE, FALSE) |> as.numeric())
      } else {
        outlier.ID <- NA
        
        swaps.portfolio <- swaps.portfolio |> 
          dplyr::mutate(outlier = FALSE)
      }

      curve <- swap.curve |> 
        dplyr::filter(ID %notin% outlier.ID) |> 
        dplyr::group_by(Bucket) |> 
        dplyr::summarise(Strike = median(strike)) |> 
        dplyr::mutate(currency = currency,
                      curve.date = date) |> 
        dplyr::select(curve.date, currency, Bucket, Strike)
      
      message("*** Bootstrapping the implied market curve ***")
      df.curve <- BootstrapCurve(date, curve, currency)
      
      message("*** Pricing the portfolio ***")
      
      priced.portfolio <- SwapPricer::SwapPortfolioPricing(swaps.portfolio, date, 
                                                           df.curve, 
                                                           duration.flag = TRUE) |> 
        dplyr::mutate(Bucket = cut(
          duration,
          breaks = c(0, 1, 3, 4, 5, 7, 10, 15, 20, 25, 30, 40, 50, 100),
          labels = c("0-1", "1-3", "3-4", "4-5", "5-7", "7-10", "10-15", "15-20", 
                     "20-25", "25-30", "30-40", "40-50", "50-100" ),
          right = FALSE))
      
      results <- swaps.portfolio |> 
        dplyr::left_join(priced.portfolio, by = c("ID" = "swap.id", "currency",
                                                  "notional"))
      
      
      message("*** Check Pricing ***")
      fit <- results |> 
        dplyr::filter(ID %in% swap.curve$ID) |> 
        dplyr::summarise(accuracy = sum(clean.mv)/sum(pv01)) |> 
        dplyr::pull(accuracy)
      
      if (length(outlier) > 0) {

        fit.clean <- results |> 
          dplyr::filter(ID %in% swap.curve$ID,
                        ID %notin% as.character(outlier.ID)) |>
          dplyr::summarise(accuracy = sum(clean.mv)/sum(pv01)) |> 
          dplyr::pull(accuracy)
      } else {
        fit.clean <- 1e99
      }
      
      fit.threshold <- 1
      
      if (abs(fit) > fit.threshold) {
        if (abs(fit.clean) < fit.threshold) {
          results <- results[-outlier,]
          fit <- fit.clean
          logr::log_print(glue::glue("Removed {length(outlier)} outlier(s)"))
        }
      } 
      
      accuracy <- tibble::tibble(date = date, currency = currency, accuracy = fit)
      outliers.removed <- tibble::tibble(date = date, currency = currency, 
                                         outliers_removed = length(outlier),
                                         ratio = length(outlier)/nrow(swaps.portfolio))
      message("*** Portfolio Priced ***")
    } else {
      curve <- NULL
      accuracy <- NULL
      outliers.removed <- NULL
      results <- NULL
    }
      
  } else {
    curve <- NULL
    accuracy <- NULL
    outliers.removed <- NULL
    results <- NULL
  }
  
  return(list(original.data.dtcc = original.data.dtcc,
              original.data.cme = original.data.cme, 
              results = results, 
              accuracy = accuracy,
              swap.curve = curve,
              outliers.removed = outliers.removed))
}