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
                  `Time to Mat` = (`Expiration Date` - `Effective Date`)/365,
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
                  `Time to Mat` = (`Maturity Date` - `Effective Date`)/365,
                  `Leg 1 Designated Maturity` = stringr::str_remove_all(`Leg 1 Designated Maturity`, "Y") |> 
                    as.numeric(),
                  `Leg 2 Designated Maturity` = stringr::str_remove_all(`Leg 2 Designated Maturity`, "Y") |> 
                    as.numeric(),
                  `Floating Rate` = dplyr::if_else(
                    `Leg 1 Type` %in% "Fixed", `Leg 2 Floating Index`, `Leg 1 Floating Index`))
}


SwapsFromCME <- function(dates, currency) {
  swaps <-  DownloadFromCME(dates) |> 
    dplyr::distinct() |> 
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
    dplyr::filter(`Effective Date` >= report.date,) |> 
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
                     spot.date = as.character(`Dissemination Time`, format = "%d/%m/%Y"),
                     time.to.mat = as.numeric(`Time to Mat`),
                     cleared = dplyr::if_else(Cleared == "Intend to Clear", 
                                              TRUE, FALSE)|> as.numeric(),
                     forward.starting = dplyr::if_else(`Effective Date` == report.date,
                                                       FALSE, TRUE)|> as.numeric())
  
}

FilterSwaps <- function(data, dcc, product.type, report.date, ois.rate) {
  
  if (stringr::str_detect(product.type, "OIS")) {
    
    data <- data |> 
      dplyr::mutate(variable.rate = `Leg 1 - Floating Rate Index`)
    
    data$variable.rate[which(is.na(data$variable.rate))] <-
      data$`Leg 2 - Floating Rate Index`[which(is.na(data$variable.rate))]
    
    data <- data |> 
      dplyr::filter(stringr::str_detect(variable.rate, ois.rate))
  }

  # dplyr::filter(floating.frequency %in% frequency[[2]],
  #               fixed.frequency %in% frequency[[1]]) |> 
  data <- data |> 
    dplyr::filter(`Effective Date` >= report.date) |> #To be relaxed 
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
                     standard = FALSE,
                     time.unit.receive = dplyr::case_when(
                       stringr::str_detect(`Payment Frequency Period 1`, "Y") ~ 12L,
                       stringr::str_detect(`Payment Frequency Period 1`, "M") ~ as.integer(stringr::str_extract(`Payment Frequency Period 1`, "\\d+")),
                       TRUE ~ NA_integer_
                     ),
                     time.unit.pay = dplyr::case_when(
                       stringr::str_detect(`Payment Frequency Period 2`, "Y") ~ 12L,
                       stringr::str_detect(`Payment Frequency Period 2`, "M") ~ as.integer(stringr::str_extract(`Payment Frequency Period 2`, "\\d+")),
                       TRUE ~ NA_integer_
                     ),
                     dcc.pay = dplyr::if_else(
                       type %in% "payer", dcc[[1]],  dcc[[2]]),
                     dcc.receive = dplyr::if_else(
                       type %in% "payer", dcc[[2]],  dcc[[1]]),
                     spot.date = as.character(`Event Timestamp`, format = "%d/%m/%Y"),
                     time.to.mat = as.numeric(`Time to Mat`),
                     cleared = dplyr::if_else(Cleared == "C", TRUE, FALSE) |> as.numeric(),
                     forward.starting = dplyr::if_else(`Effective Date` == report.date,
                                                       FALSE, TRUE)|> as.numeric()) |> 
    dplyr::filter(!is.na(time.unit.receive) & !is.na(time.unit.pay))
  
  
  return(data)
}

SwapsFromDTCC <- function(dates, currency, libor.flag = FALSE,
                          ois.flag = FALSE) {
  
  # standard.freq <- dplyr::case_when(
  #   grepl("EUR", currency) ~ list("1Y", "6M"),
  #   grepl("JPY", currency) ~ list("6M", "6M"),
  #   grepl("GBP", currency) ~ list("6M", "6M"),
  #   TRUE ~ list("1Y", "3M")
  # )
  # 
  # standard.freq.ois <- dplyr::case_when(
  #   grepl("EUR", currency) ~ list("1Y", "1Y"),
  #   grepl("JPY", currency) ~ list("1Y", "1Y"),
  #   grepl("GBP", currency) ~ list("1Y", "1Y"),
  #   TRUE ~ list("1Y", "1Y")
  # )
  
  dcc.libor <- dplyr::case_when(
    grepl("EUR", currency) ~ list("30/360", "act/360"),
    grepl("JPY", currency) ~ list("act/365", "act/365"),
    grepl("GBP", currency) ~ list("act/365", "act/365"),
    TRUE ~ list("30/360", "act/360")
  )
  
  dcc.ois <- dplyr::case_when(
    grepl("EUR", currency) ~ list("act/360", "act/360"),
    grepl("JPY", currency) ~ list("act/365", "act/365"),
    grepl("GBP", currency) ~ list("act/365", "act/365"),
    TRUE ~ list("act/360", "act/360")
  )
  
  ois.rate <- dplyr::case_when(
    grepl("EUR", currency) ~ "EUROSTR",
    grepl("JPY", currency) ~ "TONA",
    grepl("GBP", currency) ~ "SONIA",
    TRUE ~ "SOFR"
  )
  
  swaps <- DownloadFromDTCC(dates) |> 
    dplyr::distinct() |> 
    dplyr::filter(`Product ID` %in% c("InterestRate:IRSwap:FixedFloat",
                                      "InterestRate:IRSwap:OIS"),
                  Action == "NEW",
                  stringr::str_detect(`Transaction Type`, "Trade") ,
                  `Notional Currency 1` == currency | `Notional Currency 2` == currency,
                  is.na(`Spread 1`) | `Spread 1` == 0,
                  is.na(`Spread 2`) | `Spread 2` == 0,
                  `Time to Mat` >= 1.0) |> 
    dplyr::mutate(`Spot Effective Date` = CalculateEffectiveDate(`Event Timestamp`, currency)) 
  
  report.date <- max(swaps$`Spot Effective Date`)
  
  dcc <- list()
  if (length(which(swaps$`Product ID` == "InterestRate:IRSwap:FixedFloat") > 0)) {
    dcc <- c(dcc, list(libor = dcc.libor))
  }
  if (length(which(swaps$`Product ID` == "InterestRate:IRSwap:OIS") > 0)) {
    dcc <- c(dcc, list(ois = dcc.ois))
  }
  
  swaps <- swaps |> 
    dplyr::group_nest(`Product ID`) |> 
    dplyr::mutate(data = purrr::pmap(list(a = data, b = dcc, c = `Product ID`),
                                     function(a, b, c) FilterSwaps(a, b, c,
                                                                   report.date,
                                                                   ois.rate)))

  if (libor.flag) {
    swaps <- swaps |> 
      dplyr::filter(stringr::str_detect(`Product ID`, "IRSwap:FixedFloat")) |> 
      tidyr::unnest(cols = data) |> 
      dplyr::select(-`Product ID`)
  } else {
    if (ois.flag) {
      swaps <- swaps |> 
        dplyr::filter(stringr::str_detect(`Product ID`, "IRSwap:OIS")) |> 
        tidyr::unnest(cols = data) |> 
        dplyr::select(-`Product ID`)
    }
  }
  
  
  return(swaps)
}
