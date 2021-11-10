library(RMySQL)
source(here::here("R/Routine.R"))

# This routine was used to populate historical data - Not used anymore

RunOneDay <- function(date, currencies, cme.flag) {

  purrr::map(currencies, 
             ~SwapsTRAnalysis(as.Date(date, origin = "1970-01-01"), .x,
             cme.flag)) |> 
    purrr::set_names(currencies) |> 
    purrr::transpose() |> 
    purrr::map(dplyr::bind_rows)
}

start_date <- "2021-10-11"
end.date <- "2021-10-11"
cme.flag <- TRUE
bizdays <- bizdays::bizseq(start_date, end.date, "weekends")
currencies <- c("JPY")
#currencies <- c("EUR", "USD", "GBP")

results <- purrr::map(bizdays, ~RunOneDay(.x, currencies, cme.flag)) 

results.compact <- results |> 
  purrr::transpose() |> 
  purrr::map(dplyr::bind_rows) |> 
  purrr::map(dplyr::distinct)


message("*** Connecting to the DB ***")

con <-  RMySQL::dbConnect(RMySQL::MySQL(), user = Sys.getenv("user"),
                          password = Sys.getenv("password"),
                          dbname = Sys.getenv("dbname"),
                          host = 'localhost', port = 3306)

message("*** Writing Results ***")
DBI::dbWriteTable(con, 'pricing_results', results.compact$results, append = TRUE,
                  row.names = FALSE)
message("*** Writing Outliers ***")
DBI::dbWriteTable(con, 'outliers_removed', results.compact$outliers.removed, append = TRUE,
                  row.names = FALSE)
message("*** Writing Curve ***")
DBI::dbWriteTable(con, 'pricing_curve', results.compact$swap.curve, append = TRUE,
                  row.names = FALSE)
message("*** Writing Accuracy ***")
DBI::dbWriteTable(con, 'accuracy', results.compact$accuracy, append = TRUE,
                  row.names = FALSE)
message("*** Writing DTCC data ***")
DBI::dbWriteTable(con, 'dtcc_data', dplyr::select(results.compact$original.data.dtcc,
                                                  -`Collateralization Type`),
                  append = TRUE, row.names = FALSE)
# DBI::dbWriteTable(con, 'dtcc_data', results.compact$original.data.dtcc,
#                   append = TRUE, row.names = FALSE)
message("*** Writing CME data ***")
DBI::dbWriteTable(con, 'cme_data', results.compact$original.data.cme, append = TRUE,
                  row.names = FALSE)
message("*** Closing Connection ***")
dbListConnections( dbDriver( drv = "MySQL")) |>
  lapply(dbDisconnect)
