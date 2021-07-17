SwapsFromDTCC <- function(date) {
  effective.date <- (date + lubridate::days(1)) |> 
    fmdates::adjust(bdc = "mf", calendar = fmdates::USNYCalendar()) |> 
    {\(adjusted.date) adjusted.date + lubridate::days(1)}() |> 
    fmdates::adjust(bdc = "mf", calendar = fmdates::USNYCalendar())
  
  test <- dataonderivatives::ddr(date, "IR") |> 
    dplyr::mutate(`Effective Date` = as.Date(`Effective Date`),
                  `Expiration Date` = as.Date(`Expiration Date`),
                  `Time to Mat` = (`Expiration Date` - `Effective Date`)/365 |> 
                    as.numeric(),
                  `Notional Amount 1` = stringr::str_remove_all(`Notional Amount 1`, "\\,") |> 
                    stringr::str_remove("\\+") |> 
                    as.numeric()) |> 
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
                  `Effective Date` %in% effective.date,
                  `Time to Mat` >= 1.0) |> 
    dplyr::summarise(ID = `Dissemination ID`,
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

# WIP
SwapsFromCME <- function(date) {
  a <- dataonderivatives::cme(date, "IR") |> 
    dplyr::mutate(`Effective Date` = as.Date(`Effective Date`, format = "%m/%d/%Y"),
                  `Maturity Date` = as.Date(`Maturity Date`, format = "%m/%d/%Y")) |> 
    dplyr::filter(`Contract Type` == "InterestRateSwap",
                  `Leg 1 Notional Currency` = `Leg 2 Notional Currency`,
                  `Leg 1 Notional Currency` = `Leg 2 Notional Currency`,
                  `Settlement Currency` == "USD",
                  `Payment Frequency Period 1` %in% c("3M", "6M"),
                  Cleared == "C",
                  `Block Trade Election Indicator` == "N",
                  is.na(`Other Payment Amount`),
                  !stringr::str_detect(`Notional Amount 1`, "\\+"),
                  grepl("N",`Non-Standardized Pricing Indicator`),
                  `Effective Date` %in% effective.date) |> 
    dplyr::summarise(ID = `Dissemination ID`,
                     currency = `Notional Currency 1`,
                     notional = `Notional Amount 1` |> 
                       stringr::str_replace_all(",","") |> 
                       as.numeric(),
                     start.date = as.character(`Effective Date`, format = "%d/%m/%Y"),
                     maturity.date = as.character(`Expiration Date`, format = "%d/%m/%Y"),
                     strike = dplyr::if_else(
                       is.na(`Fixed Rate 1`),`Fixed Rate 2`, `Fixed Rate 1`),
                     type = dplyr::if_else(
                       is.na(`Fixed Rate 1`), "payer", "receiver"),
                     standard = TRUE) 
  
}


