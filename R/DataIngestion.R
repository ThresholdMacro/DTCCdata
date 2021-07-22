CalculateEffectiveDate <- function(date) {
  (date + lubridate::days(1)) |> 
    fmdates::adjust(bdc = "mf", calendar = fmdates::USNYCalendar()) |> 
    {\(adjusted.date) adjusted.date + lubridate::days(1)}() |> 
    fmdates::adjust(bdc = "mf", calendar = fmdates::USNYCalendar())
}

DownloadFromDTCC <- function(dates) {
  purrr::map_dfr(dates, ~dataonderivatives::ddr(.x, "IR")) |> 
    dplyr::mutate(`Event Timestamp` = as.Date(`Event Timestamp`, format = "%Y-%m-%dT%H:%M:%S"),
                  `Effective Date` = as.Date(`Effective Date`),
                  `Expiration Date` = as.Date(`Expiration Date`),
                  `Time to Mat` = (`Expiration Date` - `Effective Date`)/365 |> 
                    as.numeric(),
                  `Notional Amount 1` = stringr::str_remove_all(`Notional Amount 1`, "\\,") |> 
                    stringr::str_remove("\\+") |> 
                    as.numeric(),
                  `Notional Amount 2` = stringr::str_remove_all(`Notional Amount 1`, "\\,") |> 
                    stringr::str_remove("\\+") |> 
                    as.numeric(),
                  `Spot Effective Date` = CalculateEffectiveDate(`Event Timestamp`),
                  )
}

DownloadFromCME <- function(dates) {
  purrr::map_dfr(dates, ~dataonderivatives::cme(.x, "IR")) |> 
    dplyr::mutate(`Dissemination Time` = as.Date(`Dissemination Time`, format = "%m/%d/%Y %H:%M:%S"),
                  `Spot Effective Date` = CalculateEffectiveDate(`Dissemination Time`),
                  `Effective Date` = as.Date(`Effective Date`, format = "%m/%d/%Y"),
                  `Maturity Date` = as.Date(`Maturity Date`, format = "%m/%d/%Y"),
                  `Leg 1 Designated Maturity` = stringr::str_remove_all(`Leg 1 Designated Maturity`, "Y") |> 
                    as.numeric(),
                  `Leg 2 Designated Maturity` = stringr::str_remove_all(`Leg 2 Designated Maturity`, "Y") |> 
                    as.numeric(),
                  `Floating Rate` = dplyr::if_else(
                    `Leg 1 Type` %in% "Fixed", `Leg 2 Floating Index`, `Leg 1 Floating Index`))
}

SwapsFromDTCC <- function(dates) {
  DownloadFromDTCC(dates) |> 
    dplyr::filter(`Product ID` == "InterestRate:IRSwap:FixedFloat",
                  Action == "NEW",
                  `Transaction Type` == "Trade",
                  `Notional Currency 1` == "USD",
                  `Payment Frequency Period 1` %in% c("3M", "6M"),
                  `Payment Frequency Period 2` %in% c("3M", "6M"),
                  Cleared == "C",
                  `Block Trade Election Indicator` == "N",
                  is.na(`Other Payment Amount`),
                  grepl("N",`Non-Standardized Pricing Indicator`),
                  `Effective Date` == `Spot Effective Date`,
                  `Time to Mat` >= 1.0) |> 
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
                     standard = TRUE) 
}


SwapsFromCME <- function(dates) {
  DownloadFromCME(dates) |> 
    dplyr::filter(`Contract Type` == "InterestRateSwap",
                  `Leg 1 Notional Currency` == `Leg 2 Notional Currency`,
                  `Leg 1 Notional` == `Leg 2 Notional`,
                  `Leg 1 Designated Maturity` == `Leg 2 Designated Maturity`,
                  `Leg 1 Designated Maturity` > 1,
                  grepl("LIBOR",`Floating Rate`),
                  `Settlement Currency` == "USD",
                  `Effective Date` == `Spot Effective Date`) |> 
    dplyr::summarise(ID = glue::glue("CME_{`Rpt ID`}"),
                     currency = `Leg 1 Notional Currency`,
                     notional = `Leg 1 Notional`,
                     start.date = `Effective Date`,
                     maturity.date = `Maturity Date`,
                     strike = dplyr::if_else(
                       `Leg 1 Type` %in% "Fixed",`Leg 1 Fixed Rate`, `Leg 2 Fixed Rate`),
                     type = dplyr::if_else(
                       `Leg 1 Type` %in% "Fixed", "receiver", "payer"),
                     standard = TRUE) 
  
}


