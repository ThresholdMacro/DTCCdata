source("/home/threshold/DTCCdata/R/Bootstrap.R")
source("/home/threshold/DTCCdata/R/DataIngestion.R")
library(ggplot2)

`%notin%` <- Negate(`%in%`)
CreateList <- function(data, ois.flag, libor.flag) {
  if (libor.flag) {
    res <- list(Libor = data)
  } 
  if (ois.flag) {
    res <- list(OIS = data)
  }
  return(res)
}

RunOneDay <- function(date, currencies, cme.flag, libor.flag = FALSE, 
                      ois.flag = FALSE) {
  
  res <- purrr::map(currencies, 
             ~SwapsTRAnalysis(as.Date(date, origin = "1970-01-01"), .x,
                              cme.flag, libor.flag, ois.flag)) |> 
    purrr::set_names(currencies) |> 
    purrr::transpose() 
  
  if (libor.flag | ois.flag) {
    res$pricing.data <- res$pricing.data |> 
      purrr::transpose() |>
      purrr::map(dplyr::bind_rows) |> 
      purrr::map(~CreateList(.x, ois.flag, libor.flag))
  } else {
    res$pricing.data <- res$pricing.data |> 
      purrr::transpose() |> 
      purrr::map(purrr::transpose) |> 
      purrr::map(~purrr::map(.x, dplyr::bind_rows)) |> 
      purrr::transpose()
  }
  
  return(res)
}

PricingProcedure <- function(swaps.portfolio, currency, date) {

  if (nrow(swaps.portfolio) > 0) {
    
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
      
      row.test <- swap.curve |> 
        dplyr::select(-ID) |> 
        dplyr::distinct(.keep_all = TRUE)
      
      message("*** Outlier Detection ***")
      
      if (nrow(row.test) > 1 && length(unique(swap.curve$Bucket)) > 1) {
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
                                                 TRUE, FALSE) |> as.numeric()) |> 
          na.omit()
      } else {
        outlier.ID <- NULL
        
        swaps.portfolio <- swaps.portfolio |> 
          dplyr::mutate(outlier = FALSE) |> 
          na.omit()
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
                                                  "notional")) |> 
        dplyr::select(-time.unit.receive, -time.unit.pay,
                      -dcc.receive, -dcc.pay)
      
      
      message("*** Check Pricing ***")
      fit <- results |> 
        dplyr::filter(ID %in% swap.curve$ID) |> 
        dplyr::summarise(accuracy = sum(clean.mv)/sum(pv01)) |> 
        dplyr::pull(accuracy)
      
      if (length(outlier.ID) > 0) {
        
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
                                         outliers_removed = length(outlier.ID),
                                         ratio = outliers_removed/nrow(swaps.portfolio))
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
  
  return(list(results = results, 
              accuracy = accuracy,
              swap.curve = curve,
              outliers.removed = outliers.removed))
}

SwapsTRAnalysis <- function(date, currency, cme.flag, libor.flag = FALSE, 
                            ois.flag = FALSE) {
  message(glue::glue("*** Analysing Day {date} and currency {currency} ***"))
  
  original.data.dtcc <- DownloadFromDTCC(date) |> 
    dplyr::distinct()
  
  dtcc.swaps <- SwapsFromDTCC(date, currency, libor.flag, ois.flag)
  
  if (cme.flag) {
    original.data.cme <- DownloadFromCME(date)|> 
      dplyr::distinct()
    
    cme.swaps <- SwapsFromCME(date, currency)
    
    if (nrow(cme.swaps) == 0) {
      original.data.cme <- NULL
      cme.swaps <- NULL
    }
  } else {
    original.data.cme <- NULL
    cme.swaps <- NULL
  }
  
  swaps.portfolio <- dtcc.swaps |> 
    dplyr::bind_rows(cme.swaps)
  
  message("*** Data from the trade repository downloaded ***")

  if (!any(colnames(swaps.portfolio) %in% "data")) {
    results <- PricingProcedure(swaps.portfolio, currency, date)
  } else {
    if (nrow(swaps.portfolio) == 0) {
      results <- list(results = NULL, outliers.removed = NULL, 
                      swap.curve = NULL, accuracy = NULL)
    } else {
      results <- purrr::map(swaps.portfolio$data, ~PricingProcedure(.x, currency, 
                                                                    date)) |> 
        purrr::set_names(c("Libor", "OIS"))
    }
  }
  
  return(list(original.data.dtcc =  original.data.dtcc,
              original.data.cme = original.data.cme,
              pricing.data = results))
  
  # if (nrow(swaps.portfolio) > 0) {
  #   
  #   swap.curve <- swaps.portfolio |> 
  #     dplyr::filter(forward.starting == 0,
  #                   cleared == 1)
  # 
  #   if(nrow(swap.curve) > 0) {
  #     swap.curve <- swap.curve |> 
  #     dplyr::mutate(Bucket = cut(
  #       as.numeric(time.to.mat),
  #       breaks = c(seq(from = 0.5,to = 12.5, by = 1), 
  #                  seq(from = 17.5,to = 52.5, by = 5)),
  #       labels = c(1:12, seq(from = 15, to = 50, by = 5)),
  #       right = TRUE),
  #       Bucket = as.character(Bucket) |> as.numeric()) 
  #     
  #     row.test <- swap.curve |> 
  #       dplyr::select(-ID) |> 
  #       dplyr::distinct(.keep_all = TRUE)
  #     
  #     message("*** Outlier Detection ***")
  # 
  #     if (nrow(row.test) > 1 && length(unique(swap.curve$Bucket)) > 1) {
  #       outlier <- swap.curve |> 
  #         dplyr::select(time.to.mat, strike) |> 
  #         outForest::outForest() |> 
  #         purrr::pluck("outliers") |> 
  #         dplyr::filter(col %in% "strike") |> 
  #         dplyr::pull(row)
  #       
  #       outlier.ID <- swap.curve[outlier, "ID"] |> 
  #         dplyr::pull("ID") |> 
  #         as.character()
  #       
  #       swaps.portfolio <- swaps.portfolio |> 
  #         dplyr::mutate(outlier = dplyr::if_else(ID %in% outlier.ID,
  #                                                TRUE, FALSE) |> as.numeric()) |> 
  #         na.omit()
  #     } else {
  #       outlier.ID <- NULL
  #       
  #       swaps.portfolio <- swaps.portfolio |> 
  #         dplyr::mutate(outlier = FALSE) |> 
  #         na.omit()
  #     }
  # 
  #     curve <- swap.curve |> 
  #       dplyr::filter(ID %notin% outlier.ID) |> 
  #       dplyr::group_by(Bucket) |> 
  #       dplyr::summarise(Strike = median(strike)) |> 
  #       dplyr::mutate(currency = currency,
  #                     curve.date = date) |> 
  #       dplyr::select(curve.date, currency, Bucket, Strike)
  # 
  #     message("*** Bootstrapping the implied market curve ***")
  #     df.curve <- BootstrapCurve(date, curve, currency)
  #     
  #     message("*** Pricing the portfolio ***")
  #     
  #     priced.portfolio <- SwapPricer::SwapPortfolioPricing(swaps.portfolio, date, 
  #                                                          df.curve, 
  #                                                          duration.flag = TRUE) |> 
  #       dplyr::mutate(Bucket = cut(
  #         duration,
  #         breaks = c(0, 1, 3, 4, 5, 7, 10, 15, 20, 25, 30, 40, 50, 100),
  #         labels = c("0-1", "1-3", "3-4", "4-5", "5-7", "7-10", "10-15", "15-20", 
  #                    "20-25", "25-30", "30-40", "40-50", "50-100" ),
  #         right = FALSE))
  # 
  #     results <- swaps.portfolio |> 
  #       dplyr::left_join(priced.portfolio, by = c("ID" = "swap.id", "currency",
  #                                                 "notional"))
  #     
  #     
  #     message("*** Check Pricing ***")
  #     fit <- results |> 
  #       dplyr::filter(ID %in% swap.curve$ID) |> 
  #       dplyr::summarise(accuracy = sum(clean.mv)/sum(pv01)) |> 
  #       dplyr::pull(accuracy)
  #     
  #     if (length(outlier.ID) > 0) {
  # 
  #       fit.clean <- results |> 
  #         dplyr::filter(ID %in% swap.curve$ID,
  #                       ID %notin% as.character(outlier.ID)) |>
  #         dplyr::summarise(accuracy = sum(clean.mv)/sum(pv01)) |> 
  #         dplyr::pull(accuracy)
  #     } else {
  #       fit.clean <- 1e99
  #     }
  #     
  #     fit.threshold <- 1
  #     
  #     if (abs(fit) > fit.threshold) {
  #       if (abs(fit.clean) < fit.threshold) {
  #         results <- results[-outlier,]
  #         fit <- fit.clean
  #         logr::log_print(glue::glue("Removed {length(outlier)} outlier(s)"))
  #       }
  #     } 
  #     
  #     accuracy <- tibble::tibble(date = date, currency = currency, accuracy = fit)
  #     outliers.removed <- tibble::tibble(date = date, currency = currency, 
  #                                        outliers_removed = length(outlier.ID),
  #                                        ratio = outliers_removed/nrow(swaps.portfolio))
  #     message("*** Portfolio Priced ***")
  #   } else {
  #     curve <- NULL
  #     accuracy <- NULL
  #     outliers.removed <- NULL
  #     results <- NULL
  #   }
  #     
  # } else {
  #   curve <- NULL
  #   accuracy <- NULL
  #   outliers.removed <- NULL
  #   results <- NULL
  # }
  # 
  # return(list(original.data.dtcc = original.data.dtcc,
  #             original.data.cme = original.data.cme, 
  #             results = results, 
  #             accuracy = accuracy,
  #             swap.curve = curve,
  #             outliers.removed = outliers.removed))
}