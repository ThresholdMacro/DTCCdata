library(RMySQL)
library(glue)
source("/home/threshold/DTCCdata/R/Routine.R")

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

con <- ConnectToDB()

test.previous.upload <- con |> 
  DBI::dbGetQuery(query) |> 
  nrow()

if (test.previous.upload == 0) {
  currencies <- c("EUR", "USD", "GBP", "JPY")
  results <- RunOneDay(report.date, currencies, cme.flag = FALSE, 
                       libor.flag = FALSE, ois.flag = FALSE) 
  
  message("*** Writing Results ***")
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
  # message("*** Writing CME data ***")
  # DBI::dbWriteTable(con, 'cme_data', results$original.data.cme, append = TRUE,
  #                   row.names = FALSE)
  message("*** Writing Results ***")
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
  message("*** Closing Connection ***")
  dbListConnections( dbDriver( drv = "MySQL")) |>
    lapply(dbDisconnect)
} else {
  message("*** No New Data Available ***")
  dbListConnections( dbDriver( drv = "MySQL")) |>
    lapply(dbDisconnect)
}