#' Formatting risk data for DT
#'
#' @description data are retrieved from the db and formatted for displaying the
#' risk in a DT table in the SIrischia app.
#'
#' @param conn database connection settings.
#' @param area the area of the lab used for filtering data.
#'
#' @return a data.table
#'
#' @noRd
#' @import data.table
#' @importFrom pool dbGetQuery
#' @importFrom glue glue
format_risk <- function(conn, area) {
  stopifnot(is.character(area))

  formatted_risk <- NULL
  settore <- NULL
  metodo <- NULL
  tecnica <- NULL
  anno <- NULL
  . <- NULL

  risk_dt <- sql_getrisktable(conn, area) |> data.table::data.table()
  risk_dt[, formatted_risk := glue::glue("<span class='risk {risk_dt$colore}'>{risk_dt$rischio}</span>")][,
    .(settore, metodo, tecnica, anno, formatted_risk)] |>
    data.table::dcast(settore + metodo + tecnica ~ anno, value.var = "formatted_risk")
}

#' DT risk summary
#'
#' @description A DT table with a list of risk assessment results.
#' @param data data to be included in the DT.
#'
#' @return A datatable function
#'
#' @noRd
#' @importFrom DT datatable formatStyle
riskDT <- function(data) {
  stopifnot(is.data.frame(data))

  oldcols <- c("settore", "metodo", "tecnica")
  newcols <- c("Settore", "Metodo", "Tecnica")

  colnames(data)[colnames(data) %in% oldcols] <- newcols

  DT::datatable(
    data,
    #  filter = list(position = 'top', clear = TRUE),
    selection = "none",
    escape = FALSE,
    rownames = FALSE,
    callback = DT::JS(
      '$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "Tutti" )'
    ),
    options = list(processing = FALSE, language = dt_italian)
  ) |>
    DT::formatStyle(newcols, 'vertical-align' = 'middle')
}

#' Risk value boxes
#'
#' @description Text for the risk value boxes
#' @param conn database connection settings.
#' @param level a character, level of the risk.
#' @param year integer id of the year
#'
#' @return a string
#'
#' @noRd
#' @importFrom glue glue
risk_txt <- function(conn,
                     level,
                     year_id){

  stopifnot(is.character(level))
  stopifnot(is.integer(year_id))

  n <- sql_countrisk(conn, level, year_id)
  n_text <- ifelse(n == 0, "nessun", n)
  method_text <- ifelse(n > 1, "metodi", "metodo")
  glue::glue("{n_text} {method_text}")
}


