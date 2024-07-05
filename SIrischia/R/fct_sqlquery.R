#' SQL query for getting a list of elements
#'
#' @description the function returns a list of all the elements in a column of a table
#' in a SQL database.
#'
#' @param conn db connection parameters.
#' @param tablename name of the table.
#' @param columnname name of the column.
#'
#' @return an R vector of elements
#'
#' @noRd
#' @importFrom pool dbGetQuery
#' @importFrom glue glue_sql
sql_getlist <- function(conn,
                        table,
                        col){

  stopifnot(is.character(table))
  stopifnot(is.character(col))

  query <- glue::glue_sql("SELECT {`col`}
                            FROM {`table`};",
                          .con = conn)

  pool::dbGetQuery(conn, query) |>
    unlist() |>
    unname()
}

#' SQL query for getting a list of elements that satisfy a condition
#'
#' @description the function returns a list of the elements in a column of a table
#' in a SQL database that satisfy a condition.
#'
#' @param conn db connection parameters.
#' @param table name of the table.
#' @param col name of the column.
#' @param colcond name of the column on which the condition is set.
#' @param valcond value of the condition.
#'
#' @return an R vector of elements
#'
#' @noRd
#' @importFrom pool dbGetQuery
#' @importFrom glue glue_sql
sql_getcondlist <- function(conn,
                            table,
                            col,
                            colcond,
                            valcond){

  stopifnot(is.character(table))
  stopifnot(is.character(col))
  stopifnot(is.character(colcond))

  query <- glue::glue_sql("SELECT {`col`}
                            FROM {`table`}
                           WHERE {`colcond`} = {valcond};",
                          .con = conn)

  pool::dbGetQuery(conn, query) |>
    unlist() |>
    unname()
}

#' SQL query for getting a list of methods from a lab area
#'
#' @description the function returns a list of methods for a lab area
#'
#' @param conn db connection parameters.
#' @param area name of the lab area
#'
#' @return an R vector of elements
#'
#' @noRd
#' @importFrom pool dbGetQuery
#' @importFrom glue glue_sql
sql_getmethsforarea <- function(conn,
                               area){

  stopifnot(is.character(area))

  query <- glue::glue_sql("SELECT metodo
                            FROM metodo
                            INNER JOIN settore
                              ON metodo.settore_id = settore.settore_id
                           WHERE settore.valore = {area};",
                          .con = conn)

  pool::dbGetQuery(conn, query) |>
    unlist() |>
    unname()
}

#' SQL query for getting a list of techniques for a method id.
#'
#' @description the function returns a list of techniques for a method id.
#'
#' @param conn db connection parameters.
#' @param method name of the lab location.
#'
#' @return an R vector of elements
#'
#' @noRd
#' @importFrom pool dbGetQuery
#' @importFrom glue glue_sql
sql_gettechsformethod <- function(conn,
                                  method){

  stopifnot(is.character(method))

  query <- glue::glue_sql("SELECT valore
                            FROM tecnica
                            INNER JOIN metodo
                              ON tecnica.tecnica_id = metodo.tecnica_id
                           WHERE metodo.metodo = {method};",
                          .con = conn)

  pool::dbGetQuery(conn, query) |>
    unlist() |>
    unname()
}
