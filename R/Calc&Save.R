library(RMySQL)
source(here::here("R/Routine.R"))

RunOneDay <- function(date, currencies) {
  purrr::map(currencies, 
             ~SwapsTRAnalysis(as.Date(date, origin = "1970-01-01"), .x)) |> 
    purrr::set_names(currencies) |> 
    purrr::transpose() |> 
    purrr::map(dplyr::bind_rows)
}

dir <- file.path(here::here("log"), "log.txt")
lf <- logr::log_open(dir)
start_date <- "2021-01-04"
end.date <- "2021-02-14"
bizdays <- bizdays::bizseq(start_date, end.date, "weekends")
currencies <- c("EUR", "USD", "GBP", "JPY")

results <- purrr::map(bizdays, RunOneDay, currencies) 

results.compact <- results |> 
  purrr::transpose() |> 
  purrr::map(dplyr::bind_rows) |> 
  purrr::map(dplyr::distinct)

logr::log_close()

con <-  RMySQL::dbConnect(RMySQL::MySQL(), user = "root",
                          password = "Scr@0p1960",
                          dbname = "trade_repo_swap_data",
                          host = 'localhost', port = 3306)

DBI::dbWriteTable(con, 'pricing_results', results.compact$results, append = TRUE,
                  row.names = FALSE)
DBI::dbWriteTable(con, 'outliers_removed', results.compact$outliers.removed, append = TRUE,
                  row.names = FALSE)
DBI::dbWriteTable(con, 'pricing_curve', results.compact$swap.curve, append = TRUE,
                  row.names = FALSE)
DBI::dbWriteTable(con, 'accuracy', results.compact$accuracy, append = TRUE,
                  row.names = FALSE)
DBI::dbWriteTable(con, 'dtcc_data', results.compact$original.data.dtcc, append = TRUE,
                  row.names = FALSE)
DBI::dbWriteTable(con, 'cme_data', results.compact$original.data.cme, append = TRUE,
                  row.names = FALSE)

dbListConnections( dbDriver( drv = "MySQL")) |>
  lapply(dbDisconnect)
