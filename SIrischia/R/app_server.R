#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom pool dbPool poolClose
#' @importFrom RSQLite SQLite
#' @noRd
app_server <- function(input, output, session) {
  conn <- pool::dbPool(
    drv = RSQLite::SQLite(),
    dbname = "./data/SIrischia.db",
    extended_types = TRUE
  )
  shiny::onStop(function() {
    pool::poolClose(conn)
  })

  r_global <- reactiveValues(
    conn = conn           # db connection shared across modules
  )
}
