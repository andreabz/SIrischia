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
    dbname = "./inst/extdata/SIrischia.db",
    extended_types = TRUE
  )
  shiny::onStop(function() {
    pool::poolClose(conn)
  })

  r_global <- reactiveValues(
    conn = conn,           # db connection shared across modules
    tab = NULL             # tab switcher
  )

  observeEvent(input$navbar, {
    if(is.null(r_global$tab)) {
      r_global$tab <- 1
    } else {
      r_global$tab <- isolate(r_global$tab) + 1
    }
  })

  mod_insert01_server("methods", r_global)
  mod_view02_server("risk", r_global)
}
