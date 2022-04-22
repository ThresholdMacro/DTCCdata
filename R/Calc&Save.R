library(RMySQL)
source(here::here("R/Routine.R"))

# This routine was used to populate historical data 

# con <-  RMySQL::dbConnect(RMySQL::MySQL(), user = Sys.getenv("user"),
#                           password = Sys.getenv("password"),
#                           dbname = Sys.getenv("dbname"),
#                           host = 'localhost', port = 3306)
# 
# query <- "SELECT date FROM accuracy"
# 
# dates <- DBI::dbGetQuery(con, query) |> 
#   dplyr::pull(date) |> 
#   unique()
# 
# dbListConnections( dbDriver( drv = "MySQL")) |>
#   lapply(dbDisconnect)
start_date <- "2022-04-12"
end.date <- "2022-04-12"
bizdays <- bizdays::bizseq(start_date, end.date, "weekends")

for (date in bizdays) {
  con <-  RMySQL::dbConnect(RMySQL::MySQL(), user = Sys.getenv("user"),
                            password = Sys.getenv("password"),
                            dbname = Sys.getenv("dbname"),
                            host = 'localhost', port = 3306)
  
  cme.flag <- FALSE
  currencies <- c("EUR", "USD", "GBP", "JPY")
  # currencies <- "EUR"
  
  results <- RunOneDay(date, currencies, cme.flag, libor.flag = FALSE,
                       ois.flag = FALSE)

  # results <- purrr::map(bizdays, ~RunOneDay(.x, currencies, cme.flag, 
  #                                           libor.flag = FALSE,
  #                                           ois.flag = TRUE)) 
  
  # results.compact <- results |> 
  #   purrr::transpose() |> 
  #   purrr::map(dplyr::bind_rows) |> 
  #   purrr::map(dplyr::distinct)
  
  
  message("*** Connecting to the DB ***")
  
  # message("*** Writing Results ***")
  DBI::dbWriteTable(con, 'pricing_results',
                    results$pricing.data$results$Libor, append = TRUE,
                    row.names = FALSE)
  message("*** Writing Outliers ***")
  DBI::dbWriteTable(con, 'outliers_removed',
                    results$pricing.data$outliers.removed$Libor, append = TRUE,
                    row.names = FALSE)
  message("*** Writing Curve ***")
  DBI::dbWriteTable(con, 'pricing_curve',
                    results$pricing.data$swap.curve$Libor, append = TRUE,
                    row.names = FALSE)
  message("*** Writing Accuracy ***")
  DBI::dbWriteTable(con, 'accuracy',
                    results$pricing.data$accuracy$Libor, append = TRUE,
                    row.names = FALSE)
  message("*** Writing DTCC data ***")
  DBI::dbWriteTable(con, 'dtcc_data', dplyr::select(results$original.data.dtcc,
                                                    -`Collateralization Type`),
                    append = TRUE, row.names = FALSE)
  DBI::dbWriteTable(con, 'pricing_results_ois', 
                    results$pricing.data$results$OIS, append = TRUE,
                    row.names = FALSE)
  message("*** Writing Outliers ***")
  DBI::dbWriteTable(con, 'outliers_removed_ois', 
                    results$pricing.data$outliers.removed$OIS, append = TRUE,
                    row.names = FALSE)
  message("*** Writing Curve ***")
  DBI::dbWriteTable(con, 'pricing_curve_ois', 
                    results$pricing.data$swap.curve$OIS, append = TRUE,
                    row.names = FALSE)
  message("*** Writing Accuracy ***")
  DBI::dbWriteTable(con, 'accuracy_ois', 
                    results$pricing.data$accuracy$OIS, append = TRUE,
                    row.names = FALSE)
  # message("*** Writing DTCC data ***")
  # DBI::dbWriteTable(con, 'dtcc_data', dplyr::select(results.compact$original.data.dtcc,
  #                                                   -`Collateralization Type`),
  #                   append = TRUE, row.names = FALSE)
  # DBI::dbWriteTable(con, 'dtcc_data', results.compact$original.data.dtcc,
  #                   append = TRUE, row.names = FALSE)
  # message("*** Writing CME data ***")
  # DBI::dbWriteTable(con, 'cme_data', results.compact$original.data.cme, append = TRUE,
  #                   row.names = FALSE)
  message("*** Closing Connection ***")
  dbListConnections( dbDriver( drv = "MySQL")) |>
    lapply(dbDisconnect)
}

