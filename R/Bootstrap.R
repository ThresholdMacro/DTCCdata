Boostrapper <- function(df, portfolio, curve, spot_date) {
  curve$discount <- curve$discount |> 
    dplyr::bind_rows(
      tibble::tibble(Date = as.Date(portfolio$maturity.date, format = "%d/%m/%Y"),
                     df = df)
    )
  
  pricing <- SwapPricer::SwapPortfolioPricing(portfolio, spot_date, curve)
  target <- pricing |> 
    dplyr::pull(clean.mv) |> 
    {\(x) sum(x^2)}()
  
  return(target)
}

BootstrapCurve <- function(spot_date, par.curve, currency) {

    calendar <- dplyr::case_when(
      grepl("EUR", currency) ~ "TARGET",
      grepl("JPY", currency) ~ "Japan",
      TRUE ~ "UnitedStates"
    )
  
  curve <- list()
  curve$currency <- currency
  curve$discount <- tibble::tibble(Date = spot_date, df = 1) 
  
  years <- par.curve |> 
    dplyr::pull(Bucket)
  
  maturities <- CalculateEffectiveDate(spot_date, currency) |> 
    {\(x) x + lubridate::years(dplyr::pull(par.curve, Bucket))}() |> 
    {\(adjusted.date) RQuantLib::adjust(dates = adjusted.date, 
                                        calendar = calendar)}()
  
  strike <- par.curve |> 
    dplyr::pull(Strike)
  
  portfolio <- tibble::tibble(ID = 1:length(years), currency = curve$currency, 
                              notional = 1e7, 
                              start.date = as.character(spot_date,
                                                        "%d/%m/%Y"), 
                              maturity.date = as.character(maturities,
                                                           "%d/%m/%Y"),
                              strike = strike,
                              type = "receiver", standard = TRUE)
  

  for (i in 1:nrow(portfolio)) {

    starting.df <- curve$discount |> 
      dplyr::slice_tail() |> 
      dplyr::pull(df) 

    opt_swappricer <- optim(starting.df, Boostrapper,
                            control = list(abstol=10^(-20), reltol=10^(-20), 
                                           maxit=50000, trace=2), 
                            method = c("Brent"),
                            lower = 0, upper = 1.5,
                            portfolio = portfolio[i,],             
                            curve = curve,  
                            spot_date = spot_date) 
    
    curve$discount <- curve$discount |> 
      dplyr::bind_rows(
        tibble::tibble(
          Date = as.Date(portfolio[i,]$maturity.date, format = "%d/%m/%Y"),
          df = opt_swappricer$par
        )
      )
    message(glue::glue("Optimisation for maturity {maturities[i]} completed"))
  }
  
  return(curve)
}

