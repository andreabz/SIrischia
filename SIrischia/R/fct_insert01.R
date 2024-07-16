#' Prepare the results for SQL tables
#'
#' @description a data.frame with three columns with the results of the
#' SIrischia questionnaire is prepared.
#'
#' @details The returned data.frame is structured as follow
#' \itemize{
#'  \item{"name"}{The name of the question;}
#'  \item{"selected"}{the numeric id of the selected answer;}
#'  \item{"value"}{the numeric value assigned to the selected answer.}
#' }
#'
#' @param conn db connection parameters.
#' @param table_name a character string, indicating the name of the table for which the data has to be prepared.
#' @param method_id an integer indicating the id of the method.
#' @param year_id an integer indicating the id of the year.
#' @param questions the vector of the questions, usually the app's input.
#'
#' @return a data.frame with three columns named "name", "selected" and "value".
#' The three columns are of type character, integer and integer, respectively.
#'
#' @noRd
prepare_results <- function(conn,
                            table_name,
                            method_id,
                            year_id,
                            questions) {
  stopifnot(is.character(table_name))
  stopifnot(is.integer(method_id))
  stopifnot(is.integer(year_id))

  mycols <- pool::dbListFields(conn, table_name)
  primary_key <- paste0(table_name, "_id")
  mynames <- c(mycols[!{
    mycols %in% primary_key
  }])

  excluded_cols <- c(primary_key, "metodo_id", "anno_id", "valore")
  included_cols <- !{
    mycols %in% excluded_cols
  }
  sel_cols <- mycols[included_cols]

  answers <- lapply(sel_cols, function(x) {
    # each columnname_id references a corresponding columnname table storing each possible
    # answer for each question
    table <- gsub("_id", "", x)
    # qestions are coded in SIrischia as q_question1, q_question2, ...
    question <- paste0("q_", table)

    # select the answer id and score when descrizione equals the selected answer in the app.
    sql_getcondlist(conn, table, c(x, "valore"), "descrizione", questions[[question]])
  })

  data <- do.call(rbind.data.frame, answers)
  colnames(data) <- c("selected", "score")

  cbind.data.frame(
    name = mynames,
    value = c(
      method_id,
      year_id,
      data$selected,
      mean(data$score) |> round(2)
    )
  )
}
