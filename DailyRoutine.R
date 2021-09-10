library(RMySQL)
library(glue)
source(here::here("R/Routine.R"))

ConnectToDB <- function(){
  db_user <- Sys.getenv("user")
  db_password <-  Sys.getenv("password")
  db_name <- Sys.getenv("dbname")
  db_host <- 'localhost'
  db_port <- 3306
  
  mydb <-  RMySQL::dbConnect(RMySQL::MySQL(), user = db_user, 
                             password = db_password, dbname = db_name, 
                             host = db_host, port = db_port)
}

report.date <- Sys.Date() - 1

query <- glue("SELECT * FROM `pricing_results` WHERE `spot.date` = \\
              '{as.character(report.date, format = '%d')}/\\
              {as.character(report.date, format = '%m')}/\\
              {as.character(report.date, format = '%Y')}'")

test.previous.upload <- ConnectToDB() |> 
  DBI::dbGetQuery(query) |> 
  nrow()

if (test.previous.upload == 0) {
  currencies <- c("EUR", "USD", "GBP", "JPY")
  results <- RunOneDay(report.date, currencies, TRUE) 
  
  message("*** Writing Results ***")
  DBI::dbWriteTable(con, 'pricing_results', results$results, append = TRUE,
                    row.names = FALSE)
  message("*** Writing Outliers ***")
  DBI::dbWriteTable(con, 'outliers_removed', results$outliers.removed, append = TRUE,
                    row.names = FALSE)
  message("*** Writing Curve ***")
  DBI::dbWriteTable(con, 'pricing_curve', results$swap.curve, append = TRUE,
                    row.names = FALSE)
  message("*** Writing Accuracy ***")
  DBI::dbWriteTable(con, 'accuracy', results$accuracy, append = TRUE,
                    row.names = FALSE)
  message("*** Writing DTCC data ***")
  DBI::dbWriteTable(con, 'dtcc_data', dplyr::select(results.compact$original.data.dtcc,
                                                    -`Collateralization Type`), 
                    append = TRUE, row.names = FALSE)
  message("*** Writing CME data ***")
  DBI::dbWriteTable(con, 'cme_data', results$original.data.cme, append = TRUE,
                    row.names = FALSE)
  message("*** Closing Connection ***")
  dbListConnections( dbDriver( drv = "MySQL")) |>
    lapply(dbDisconnect)
} else {
  message("*** No New Data Available ***")
  
}
