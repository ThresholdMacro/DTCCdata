CalculateEffectiveDate <- function(date, currency) {
  if (!grepl("GBP", currency)) {
    calendar <- dplyr::case_when(
      grepl("EUR", currency) ~ "TARGET",
      grepl("JPY", currency) ~ "Japan",
      TRUE ~ "UnitedStates"
    )
    
    date <- (date + 1) |> 
      {\(adjusted.date) RQuantLib::adjust(dates = adjusted.date, calendar = calendar)}() |> 
      {\(adjusted.date) RQuantLib::adjust(dates = adjusted.date + 1, calendar = calendar)}()
  }
  return(date)

}

DownloadFromDTCC <- function(dates) {
  purrr::map_dfr(dates, ~dataonderivatives::ddr(.x, "IR")) |> 
    dplyr::mutate(`Event Timestamp` = as.Date(`Event Timestamp`, format = "%Y-%m-%dT%H:%M:%S"),
                  `Effective Date` = as.Date(`Effective Date`),
                  `Expiration Date` = as.Date(`Expiration Date`),
                  `Spread 1` = as.numeric(`Spread 1`),
                  `Spread 2` = as.numeric(`Spread 2`),
                  `Time to Mat` = (`Expiration Date` - `Effective Date`)/365 |> 
                    as.numeric(),
                  `Notional Amount 1` = stringr::str_remove_all(`Notional Amount 1`, "\\,") |> 
                    stringr::str_remove("\\+") |> 
                    as.numeric(),
                  `Notional Amount 2` = stringr::str_remove_all(`Notional Amount 1`, "\\,") |> 
                    stringr::str_remove("\\+") |> 
                    as.numeric())
}

DownloadFromCME <- function(dates) {
  purrr::map_dfr(dates, ~dataonderivatives::cme(.x, "IR")) |> 
    dplyr::mutate(`Dissemination Time` = as.Date(`Dissemination Time`, format = "%m/%d/%Y %H:%M:%S"),
                  `Effective Date` = as.Date(`Effective Date`, format = "%m/%d/%Y"),
                  `Maturity Date` = as.Date(`Maturity Date`, format = "%m/%d/%Y"),
                  `Leg 1 Designated Maturity` = stringr::str_remove_all(`Leg 1 Designated Maturity`, "Y") |> 
                    as.numeric(),
                  `Leg 2 Designated Maturity` = stringr::str_remove_all(`Leg 2 Designated Maturity`, "Y") |> 
                    as.numeric(),
                  `Floating Rate` = dplyr::if_else(
                    `Leg 1 Type` %in% "Fixed", `Leg 2 Floating Index`, `Leg 1 Floating Index`))
}


SwapsFromCME <- function(dates, currency) {
  swaps <-  DownloadFromCME(dates) |> 
    dplyr::filter(`Contract Type` == "InterestRateSwap",
                  `Leg 1 Notional Currency` == `Leg 2 Notional Currency`,
                  `Leg 1 Notional` == `Leg 2 Notional`,
                  `Leg 1 Designated Maturity` == `Leg 2 Designated Maturity`,
                  `Leg 1 Designated Maturity` > 1,
                  `Settlement Currency` == currency,
                  !grepl("OIS", Product)) |> 
    dplyr::mutate(`Spot Effective Date` = CalculateEffectiveDate(`Dissemination Time`, currency))
  
  report.date <- max(swaps$`Spot Effective Date`)
  
  swaps |> 
    dplyr::filter(`Effective Date` == `Spot Effective Date`) |> #To be relaxed
    dplyr::summarise(ID = glue::glue("CME_{`Rpt ID`}"),
                     currency = `Leg 1 Notional Currency`,
                     notional = `Leg 1 Notional`,
                     start.date = as.character(`Effective Date`, 
                                               format = "%d/%m/%Y"),
                     maturity.date = as.character(`Maturity Date`, 
                                                  format = "%d/%m/%Y"),
                     strike = dplyr::if_else(
                       `Leg 1 Type` %in% "Fixed",`Leg 1 Fixed Rate`, `Leg 2 Fixed Rate`),
                     type = dplyr::if_else(
                       `Leg 1 Type` %in% "Fixed", "receiver", "payer"),
                     standard = TRUE,
                     spot.date = as.character(`Spot Effective Date`, format = "%d/%m/%Y")) 
  
}

SwapsFromDTCC <- function(dates, currency) {
  
  standard.freq <- dplyr::case_when(
    grepl("EUR", currency) ~ c("1Y", "6M"),
    grepl("JPY", currency) ~ c("6M"),
    grepl("GBP", currency) ~ c("6M"),
    TRUE ~ c("3M", "6M")
  )

  swaps <- DownloadFromDTCC(dates) |> 
    dplyr::filter(`Product ID` == "InterestRate:IRSwap:FixedFloat",
                  Action == "NEW",
                  `Notional Currency 1` == currency | `Notional Currency 2` == currency,
                  `Payment Frequency Period 1` %in% standard.freq,
                  `Payment Frequency Period 2` %in% standard.freq,
                  is.na(`Spread 1`) | `Spread 1` == 0,
                  is.na(`Spread 2`) | `Spread 2` == 0,
                  Cleared == "C",
                  `Time to Mat` >= 1.0) |> 
    dplyr::mutate(`Spot Effective Date` = CalculateEffectiveDate(`Event Timestamp`, currency))
  
  report.date <- max(swaps$`Spot Effective Date`)
  
  swaps |> 
    dplyr::filter(`Effective Date` == report.date) |> #To be relaxed
    dplyr::summarise(ID = glue::glue("DTCC_{`Dissemination ID`}"),
                     currency = `Notional Currency 1`,
                     notional = `Notional Amount 1` |> 
                       stringr::str_replace_all(",","") |> 
                       as.numeric(),
                     start.date = as.character(`Effective Date`, format = "%d/%m/%Y"),
                     maturity.date = as.character(`Expiration Date`, format = "%d/%m/%Y"),
                     strike = dplyr::if_else(
                       is.na(`Fixed Rate 1`),as.numeric(`Fixed Rate 2`), 
                       as.numeric(`Fixed Rate 1`)),
                     type = dplyr::if_else(
                       is.na(`Fixed Rate 1`), "payer", "receiver"),
                     standard = TRUE,
                     spot.date = as.character(`Spot Effective Date`, format = "%d/%m/%Y")) 
}
