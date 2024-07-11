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

  if(is.character(colcond)){
    mycols <- glue::glue_sql("{`colcond`}", .con = conn)

  } else if (is.numeric(colcond)){
    mycols <- glue::glue_sql("{colcond}", .con = conn)

  }

  query <- glue::glue_sql("SELECT {`col`}
                            FROM {`table`}
                           WHERE {mycols} = {valcond};",
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

#' SQL query for getting a the method identifiers given lab id, method name
#' and technique id
#'
#' @description the function returns a vector of integers for method identifiers.
#'
#' @param conn db connection parameters.
#' @param area the id of the lab area.
#' @param method the name of the method.
#' @param tech the id of the technique.
#'
#' @return an vector of integers
#'
#' @noRd
#' @importFrom pool dbGetQuery
#' @importFrom glue glue_sql
sql_getmethid <- function(conn,
                          area,
                          method,
                          tech){

  stopifnot(is.integer(area))
  stopifnot(is.character(method))
  stopifnot(is.integer(tech))

  query <- glue::glue_sql("SELECT metodo_id
                            FROM metodo
                           WHERE settore_id = {area}
                            AND metodo = {method}
                            AND tecnica_id = {tech};",
                          .con = conn)

  pool::dbGetQuery(conn, query) |>
    unlist() |>
    unname()
}

#' SQL code for adding new data
#'
#' @description The function add new data to a database using SQL code.
#' @param conn connection object.
#' @param tbl the name of the table in which new data is to be added.
#' @param cols the name of the fields for which new data has been provided.
#' @param vals the new values to be added to the dataset.
#'
#' @return no output
#'
#' @noRd
#' @importFrom DBI dbGetQuery dbExecute
#' @importFrom glue glue_sql glue_sql_collapse
#' SQL code for adding new data
#'
#' @description The function add new data to a database using SQL code.
#' @param conn connection object.
#' @param tbl the name of the table in which new data is to be added.
#' @param cols the name of the fields for which new data has been provided.
#' @param vals the new values to be added to the dataset.
#'
#' @return no output
#'
#' @noRd
#' @importFrom DBI dbGetQuery dbExecute
#' @importFrom glue glue_sql glue_sql_collapse
sql_insert <- function(conn, tbl, cols, vals){

  # for properly quoting only character arguments
  myvals <- lapply(vals, function(x) {
    if(is.character(x)){
      glue::glue_sql("{as.character(x)}", .con = conn)
    } else {
      glue::glue_sql("{x}", .con = conn)
    }
  }) |>
    unlist() |>
    glue::glue_sql_collapse(sep = ", ")

  sql_cols <- paste0("(",
                     glue::glue_sql_collapse({`cols`}, sep = ","),
                     ")") |> glue::glue_sql()
  sql_vals <- glue::glue_sql("({myvals})")
  sql_query <- glue::glue_sql(.con = conn,
                              "INSERT INTO {`tbl`}
                               {sql_cols}
                               VALUES {sql_vals};")

  pool::dbExecute(conn, sql_query)
}

#' SQL code for updating the data
#'
#' @description The function updates the database using SQL code.
#' @param conn connection object.
#' @param tbl the name of the table in which the data is to be updated.
#' @param cols the name of the fields for which the data to be updated has been provided.
#' @param vals the new values to update the dataset.
#' @param condcol the column name on which the WHERE condition is set.
#' @param condval the value on which the WHERE condition is set.
#'
#' @return no output
#'
#' @noRd
#' @importFrom DBI dbGetQuery dbExecute
#' @importFrom glue glue_sql glue_sql_collapse
sql_update <- function(conn, tbl, cols, vals, condcol, condval){

  sql_cols <- lapply(cols, function(x, data = vals) {
    val <- vals[which(cols == x)]

    if(is.character(val)){
      glue::glue_sql("{`x`} = {as.character(val)}", .con = conn)
    }

    glue::glue_sql("{`x`} = {val}", .con = conn)
  }) |> glue::glue_sql_collapse(sep = ", ")

  if(is.character(condval)){
    condval <- as.character(condval)
  }

  sql_query <- glue::glue_sql(.con = conn,
                              "UPDATE {`tbl`}
                               SET {sql_cols}
                               WHERE {`condcol`} = {condval};")

  print(sql_query)

  pool::dbExecute(conn, sql_query)
}

#' SQL query for getting the id of a record in probability, rilevability or
#' gravity tables given a method and year id
#'
#' @description the function returns an integer.
#'
#' @param conn db connection parameters.
#' @param table the name of the table.
#' @param method_id the id of the method.
#' @param year_id the id of the year.
#'
#' @return an integers
#'
#' @noRd
#' @importFrom pool dbGetQuery
#' @importFrom glue glue_sql
sql_getresid <- function(conn,
                         table,
                         method_id,
                         year_id){

  stopifnot(is.character(table))
  stopifnot(is.integer(method_id))
  stopifnot(is.integer(year_id))

  col_id <- glue::glue("{`table`}", "_id")

  query <- glue::glue_sql("SELECT {`col_id`}
                            FROM {`table`}
                           WHERE metodo_id = {method_id}
                            AND anno_id = {year_id};",
                          .con = conn)

  pool::dbGetQuery(conn, query) |>
    unlist() |>
    unname()
}

#' SQL query for getting the id of a record risk table given the id of probability
#' gravity and detectability records
#'
#' @description the function returns an integer.
#'
#' @param conn db connection parameters.
#' @param detect_id the of the record in the detectability table.
#' @param prob_id the of the record in the probability table.
#' @param magn_id the of the record in the magnitude table.
#' @param year_id the id of the year.
#'
#' @return a vector of integers
#'
#' @noRd
#' @importFrom pool dbGetQuery
#' @importFrom glue glue_sql
sql_getriskid <- function(conn,
                         detect_id,
                         prob_id,
                         magn_id){

   stopifnot(is.integer(detect_id))
   stopifnot(is.integer(prob_id))
   stopifnot(is.integer(magn_id))

  query <- glue::glue_sql("SELECT rischio_id
                            FROM rischio
                           WHERE rilevabilita_id = {detect_id}
                            AND probabilita_id = {prob_id}
                            AND gravita_id = {magn_id};",
                          .con = conn)

  pool::dbGetQuery(conn, query) |>
    unlist() |>
    unname()
}

#' SQL query for getting the max year value stored in a result table
#'
#' @description the function returns a four digits integer.
#'
#' @param conn db connection parameters.
#' @param table the name of the table.
#' @param method_id the id of the method.
#' @param year_id the id of the year.
#'
#' @return an integer
#'
#' @noRd
#' @importFrom pool dbGetQuery db
#' @importFrom glue glue_sql
sql_getmaxyear <- function(conn,
                           table,
                           method_id){

  stopifnot(is.character(table))
  stopifnot(is.integer(method_id))
  stopifnot(pool::dbExistsTable(conn, table))
  stopifnot(pool::dbExistsTable(conn, "anno"))
  stopifnot(c("anno_id", "valore") %in% pool::dbListFields(conn, "anno"))
  stopifnot(c(paste0(table, "_id"), "metodo_id") %in% pool::dbListFields(conn, table))


  query <- glue::glue_sql("SELECT MAX(valore) AS anno
                            FROM {`table`}
                           JOIN anno
                            ON {`table`}.anno_id = anno.anno_id
                           WHERE metodo_id = {method_id};",
                          .con = conn)

  pool::dbGetQuery(conn, query) |>
    unlist() |>
    unname()
}

#' SQL query for getting the mean of the value associated to a table result
#'
#' @description the function returns a four digits integer.
#'
#' @param conn db connection parameters.
#' @param table the name of the table.
#' @param element_id the id of the element.
#'
#' @return an integer
#'
#' @noRd
#' @importFrom pool dbGetQuery db
#' @importFrom glue glue_sql
sql_getmean <- function(conn,
                        table,
                        element_id){

  stopifnot(is.character(table))
  stopifnot(is.integer(element_id))
  stopifnot(pool::dbExistsTable(conn, table))

  col_names <- pool::dbListFields(conn, table)
  table_names <- col_names |> (\(x) {gsub("_id", "", x)})()

  lapply(table_names, function (x) {
    stopifnot(pool::dbExistsTable(conn, x))
  })

 query <- lapply(col_names[-c(1:3)], function(x) {
    uniontable <-  gsub("_id", "", x)
    table_id <- paste0(table, "_id")

    union <- glue::glue_sql("SELECT valore
                     FROM {`uniontable`}
                     INNER JOIN {`table`}
                      ON {`uniontable`}.{`x`} = {`table`}.{`x`}
                     WHERE {`table_id`} = {element_id}",
                     .con = conn)
  }) |> glue::glue_sql_collapse(sep = " UNION ALL ")

  pool::dbGetQuery(conn, query) |>
    unlist() |>
    unname() |>
    mean() |>
    round(2)
}
