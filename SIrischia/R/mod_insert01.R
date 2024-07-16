#' insert01 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom bslib navset_tab nav_panel layout_column_wrap card card_body card_header input_task_button
#' @importFrom shinyjs hidden
mod_insert01_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "d-inline-flex input-group align-items-center gap-2 mx-3",

      #### top area, method and year selector ----
      shiny::selectInput(
        inputId = ns("area"),
        label = "Settore",
        choices = NULL
      ),

      shiny::selectInput(
        inputId = ns("method"),
        label = "Metodo",
        choices = NULL
      ),

      shiny::selectInput(
        inputId = ns("tech"),
        label = "Tecnica",
        choices = NULL
      ),

      shiny::selectInput(
        inputId = ns("year"),
        label = "Anno",
        choices = NULL
      ),

      div(
        style = "margin-top: 15px;",
        bslib::input_task_button(
          id = ns("filter"),
          label = "Filtra i risultati",
          icon = shiny::icon("filter")
        )
      )
    ),

    #### hidden panel for missing data ----

    shinyjs::hidden(div(
      id = ns("missing"),
      class = "container",

      htmlOutput(ns("missing-txt")),
      bslib::input_task_button(
        id = ns("add"),
        label = "Aggiungi",
        icon = shiny::icon("plus")
      )
    )),

    #### hidden panel for freshly added data ----

    shinyjs::hidden(div(
      id = ns("done"),
      class = "container",
      htmlOutput(ns("done-txt"))
      )),

    #### questions ----
    shinyjs::hidden(bslib::card(
      id = ns("risk"),
      bslib::card_header(textOutput(ns("title"))),
      fill = FALSE,

      bslib::layout_column_wrap(
        width = 1 / 4,

        bslib::card_body(
          id = ns("rilevability"),

          shiny::h5("Rilevabilit\u00E0"),

          shiny::selectInput(
            inputId = ns("q_partecipazionept"),
            label = "A quanti proficency test partecipi?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_giustezza"),
            label = "Con che frequenza esegui prove di giustezza?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_ripetibilita"),
            label = "Con che frequenza esegui prove di ripetibilit\u00E0?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_cartacontrollo"),
            label = "Con che frequenza verifichi la carta di controllo?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_selettivita"),
            label = "Con che frequenza verifichi la selettivit\u00E0?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_contaminazione"),
            label = "Con che frequenza verifichi la contaminazione?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_taratura"),
            label = "Con che frequenza esegui la taratura?",
            choices = NULL
          )

        ),

        bslib::card_body(
          id = ns("probability"),

          shiny::h5("Probabilit\u00E0"),

          shiny::selectInput(
            inputId = ns("q_personale"),
            label = "Quanti operatori sono abilitati?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_metodotipo"),
            label = "A quale tipologia \u00E8 appartiene il metodo?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_manutenzione"),
            label = "Quanti interventi di manutenzione correttiva sono stati eseguiti negli ultimi tre anni?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_risultatopt"),
            label = "Quanto tempo è passato dall'ultimo proficency non conforme?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_campionianno"),
            label = "Quanti campioni sono stati analizzati nell'anno di interesse?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_metodoaccreditato"),
            label = "La prova \u00E8 accreditata?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_erroririsultato"),
            label = "Quanto spesso si sono verificati errori di calcolo o di trasferimento dati?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_esperienzametodo"),
            label = "Da quanti anni il metodo \u00E8 in utilizzo?",
            choices = NULL
          )

        ),

        bslib::card_body(
          id = ns("magnitude"),

          shiny::h5("Gravit\u00E0"),

          shiny::selectInput(
            inputId = ns("q_provasanzionatoria"),
            label = "La prova \u00E8 sanzionatoria?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_dannoimmagine"),
            label = "In caso di misura errata, come sar\u00E0 il danno di immagine per il laboratorio?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_perditafiducia"),
            label = "In caso di misura errata, come sar\u00E0 la perdita di fiducia del cliente?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_fattoriaggravanti"),
            label = "I parametri prestazionali sono adeguati ai requisiti?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_provaripetibile"),
            label = "La prova pu\u00F2 essere ripetuta?",
            choices = NULL
          )

        ),


        #### save button and results preview ----
        bslib::card_body(
          id = ns("result-tab"),

          div(style = "margin-top: 15px;", bslib::input_task_button(id = ns("save"), label = "Salva")),

          div(
            id = ns("result"),
            div(style = "margin-top: 15px", shiny::h5("Risultato")),
            tags$ul(
              tags$li("Rilevabilit\u00E0: ", htmlOutput(ns(
                "detectability_result"
              ))),
              tags$li("Probabilit\u00E0: ", htmlOutput(ns(
                "probability_result"
              ))),
              tags$li("Gravit\u00E0: ", htmlOutput(ns("magnitude_result")))
            ),
            tags$div("Rischio: ", htmlOutput(ns("risk_result")))
          )
        )
      )
    ))

  )

}

#' insert01 Server Functions
#'
#' @noRd
#' @importFrom glue glue glue_sql
#' @importFrom pool dbListFields dbGetQuery
#' @importFrom shinyjs show hide
mod_insert01_server <- function(id, r_global) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    r_local <- reactiveValues(
      area_id = NULL,
      area = NULL,
      method_id = NULL,
      method = NULL,
      technique_id = NULL,
      technique = NULL,
      year_id = NULL,
      year = NULL,
      oldyear_id = NULL,
      oldyear = NULL,
      questions = NULL,
      probability = data.frame(name = rep(NA, 11), value = rep(NA, 11)),
      probability_id = NULL,
      probability_cols = NULL,
      magnitude = data.frame(name = rep(NA, 8), value = rep(NA, 8)),
      magnitude_id = NULL,
      magnitude_cols = NULL,
      detectability = data.frame(name = rep(NA, 10), value = rep(NA, 10)),
      detectability_id = NULL,
      detectability_cols = NULL,
      risk_id = NULL,
      risk_value = NULL,
      risk_color = NULL,
      new_data = NULL,
      new_year = NULL,
      title = NULL,
      missing = NULL,
      done = NULL
    )

    #### initialise selectinput choices ----
    observeEvent(r_global$conn, {
      # initialise the area selectinput
      updateSelectInput(
        session,
        inputId = "area",
        choices = sql_getlist(r_global$conn, "settore", "valore")
      )
      # initialise the year selectinput
      updateSelectInput(
        session,
        inputId = "year",
        choices = sql_getlist(r_global$conn, "anno", "valore")
      )

      # save the question ids
      r_local$questions <- names(input)[grepl("q_", names(input))] |>
        (\(x) {
          gsub(pattern = "q_", replacement = "", x)
        })()

      # save the column names for the answer tables
      r_local$detectability_cols <- pool::dbListFields(r_global$conn, "rilevabilita") |>
        unlist() |>
        unname()

      r_local$probability_cols <- pool::dbListFields(r_global$conn, "probabilita") |>
        unlist() |>
        unname()

      r_local$magnitude_cols <- pool::dbListFields(r_global$conn, "gravita") |>
        unlist() |>
        unname()

    })

    #### update the methods list based on lab site ----
    observeEvent(input$area, {
      r_local$area <- input$area
      r_local$area_id <- sql_getcondlist(r_global$conn,
                                         "settore",
                                         "settore_id",
                                         "valore",
                                         input$area)

      methods_area <- sql_getmethsforarea(r_global$conn, input$area)

      freezeReactiveValue(input, "method")
      updateSelectInput(session, inputId = "method", choices = methods_area)
    })


    #### update the technique list based on lab site and method ----
    observeEvent(input$method, {
      r_local$method <- input$method
      r_local$method_id <- sql_getcondlist(r_global$conn,
                                           "metodo",
                                           "metodo_id",
                                           "metodo",
                                           input$method)

      tech_method <- sql_gettechsformethod(r_global$conn, input$method)

      freezeReactiveValue(input, "tech")
      updateSelectInput(session, inputId = "tech", choices = tech_method)
    })

    #### save choices and check if a record already exists ----
    observeEvent(input$filter, {
      r_local$technique <- input$tech
      r_local$technique_id <- sql_getcondlist(r_global$conn,
                                              "tecnica",
                                              "tecnica_id",
                                              "valore",
                                              input$tech)
      r_local$year <- input$year
      r_local$year_id <- sql_getcondlist(r_global$conn, "anno", "anno_id", "valore", input$year)

      r_local$method_id <- sql_getmethid(
        r_global$conn,
        area = r_local$area_id,
        method = r_local$method,
        tech = r_local$technique_id
      ) |> min()
      # get just one of the ids as method_id (MI) nor method names (MN) correspond to method definition (MD).
      # MD is a sequence of operations performed on samples. The performances of such a
      # sequence is monitored with quality controls and improved analyzing the results of risk assessment.
      # During the development of this piece of software, MN can be a collection of MD
      # performed in different laboratories areas, locations and on totally different kind of instruments,
      # whereas MI represents the method list on accredia sites and can be too granular for quality control
      # and risk assessment analysis.

      r_local$title <- glue::glue(
        "Metodo {r_local$method}",
        " basato su {r_local$technique}",
        ", in uso presso il settore {r_local$area}",
        " - {r_local$year}"
      )

      r_local$probability_id  <- sql_getresid(r_global$conn,
                                              "probabilita",
                                              r_local$method_id,
                                              r_local$year_id)

      r_local$magnitude_id  <- sql_getresid(r_global$conn,
                                            "gravita",
                                            r_local$method_id,
                                            r_local$year_id)

      r_local$detectability_id  <- sql_getresid(r_global$conn,
                                                "rilevabilita",
                                                r_local$method_id,
                                                r_local$year_id)

      if (length(r_local$probability_id) != 0) {
        shinyjs::show("risk")
        shinyjs::hide("missing")
        shinyjs::hide("done")

        r_local$new_data <- NULL
        r_local$new_year <- NULL
        r_local$risk_id <- sql_getriskid(
          r_global$conn,
          r_local$detectability_id,
          r_local$probability_id,
          r_local$magnitude_id
        )

      } else {
        shinyjs::show("missing")
        shinyjs::hide("risk")
        shinyjs::hide("done")
        # look for data for the same method but another year
        r_local$risk_id <- NULL
        r_local$oldyear <- sql_getmaxyear(r_global$conn, "probabilita", r_local$method_id)
        r_local$oldyear_id <- sql_getcondlist(r_global$conn,
                                              "anno",
                                              "anno_id",
                                              "valore",
                                              r_local$oldyear)

        if (length(r_local$oldyear_id != 0)) {
          r_local$new_year <- 1
          r_local$new_data <- NULL

          r_local$missing <- glue::glue(
            "Nessun dato trovato per il metodo {r_local$method}",
            " basato su {r_local$technique}",
            ", in uso presso il settore {r_local$area}",
            " nell'anno {r_local$year}.<br/>
            Vuoi aggiungere una nuova analisi partendo dai risultati salvati
            per l'anno {r_local$oldyear}?"
          )

        } else {
          r_local$new_data <- 1
          r_local$new_year <- NULL

          r_local$missing <- glue::glue(
            "Nessun dato trovato per il metodo {r_local$method}",
            " basato su {r_local$technique}",
            ", in uso presso il settore {r_local$area}.<br/>",
            "Vuoi aggiungere una nuova analisi?"
          )

        }
      }
    })

    observeEvent(input$add, {
      shinyjs::hide("missing")
      shinyjs::show("risk")
      shinyjs::hide("result")

      #### init the menus for new risk assessment ----
      if (!is.null(r_local$new_data)) {
        lapply(r_local$questions, function(x) {
          mychoices <- sql_getlist(r_global$conn, x, "descrizione")
          myid <- paste0("q_", x)
          updateSelectInput(session, inputId = myid, choices = mychoices)
        })

        #### restore answers for last recorded year
      } else if (!is.null(r_local$new_year)) {
        oldprobability_id  <- sql_getresid(r_global$conn,
                                           "probabilita",
                                           r_local$method_id,
                                           r_local$oldyear_id)

        oldmagnitude_id  <- sql_getresid(r_global$conn,
                                         "gravita",
                                         r_local$method_id,
                                         r_local$oldyear_id)

        olddetectability_id  <- sql_getresid(r_global$conn,
                                             "rilevabilita",
                                             r_local$method_id,
                                             r_local$oldyear_id)

        lapply(r_local$questions, function(x) {
          mychoices <- sql_getlist(r_global$conn, x, "descrizione")
          myid <- paste0("q_", x)
          mycol <- paste0(x, "_id")
          myval <- if (mycol %in% r_local$detectability_cols) {
            c(table = "rilevabilita",
              table_id = "rilevabilita_id",
              id = olddetectability_id)
          } else if (mycol %in% r_local$probability_cols) {
            c(table = "probabilita",
              table_id = "probabilita_id",
              id = oldprobability_id)
          } else if (mycol %in% r_local$magnitude_cols) {
            c(table = "gravita",
              table_id = "gravita_id",
              id = oldmagnitude_id)
          }

          myquery <- glue::glue_sql(
            .con = r_global$conn,
            "SELECT descrizione
             FROM {`myval['table']`}
             INNER JOIN {`x`} ON {`myval['table']`}.{`mycol`} = {`x`}.{`mycol`}
             WHERE {`myval['table_id']`} = {as.numeric(myval['id'])};"
          )

          myselected <- pool::dbGetQuery(r_global$conn, myquery) |> unlist() |> unname()
          updateSelectInput(
            session,
            inputId = myid,
            choices = mychoices,
            selected = myselected
          )
        })

      }

    })

    #### restore saved questionnaire choices when they exist ----
    observeEvent(r_local$risk_id, {
      shinyjs::show("result")

      # if data were saved restored them and load the menus
      lapply(r_local$questions, function(x) {
        mychoices <- sql_getlist(r_global$conn, x, "descrizione")
        myid <- paste0("q_", x)
        mycol <- paste0(x, "_id")
        myval <- if (mycol %in% r_local$detectability_cols) {
          c(
            table = "rilevabilita",
            table_id = "rilevabilita_id",
            id = r_local$detectability_id
          )
        } else if (mycol %in% r_local$probability_cols) {
          c(
            table = "probabilita",
            table_id = "probabilita_id",
            id = r_local$probability_id
          )
        } else if (mycol %in% r_local$magnitude_cols) {
          c(
            table = "gravita",
            table_id = "gravita_id",
            id = r_local$magnitude_id
          )
        }

        myquery <- glue::glue_sql(
          .con = r_global$conn,
          "SELECT descrizione
           FROM {`myval['table']`}
           INNER JOIN {`x`} ON {`myval['table']`}.{`mycol`} = {`x`}.{`mycol`}
           WHERE {`myval['table_id']`} = {as.numeric(myval['id'])};"
        )

        myselected <- pool::dbGetQuery(r_global$conn, myquery) |> unlist() |> unname()
        updateSelectInput(
          session,
          inputId = myid,
          choices = mychoices,
          selected = myselected
        )
      })

      # get the risk value
      r_local$risk_value <- sql_getcondlist(r_global$conn,
                                            "rischio",
                                            "valore",
                                            "rischio_id",
                                            r_local$risk_id)
      r_local$risk_color <- sql_getcondlist(r_global$conn,
                                            "rischio",
                                            "colore",
                                            "rischio_id",
                                            r_local$risk_id)
    })

    observeEvent(input$save, {
      #### populating the probability table results ----
      r_local$probability <- prepare_results(r_global$conn,
                                             "probabilita",
                                             r_local$method_id,
                                             r_local$year_id,
                                             input)

      #### populating the detectability table results ----
      r_local$detectability <- prepare_results(r_global$conn,
                                               "rilevabilita",
                                               r_local$method_id,
                                               r_local$year_id,
                                               input)

      #### populating the magnitude table results ----
      r_local$magnitude <- prepare_results(r_global$conn,
                                           "gravita",
                                           r_local$method_id,
                                           r_local$year_id,
                                           input)

      if (!is.null(r_local$new_data) ||
          !is.null(r_local$new_year)) {
        sql_insert(
          r_global$conn,
          "probabilita",
          r_local$probability$name,
          r_local$probability$value
        )
        sql_insert(
          r_global$conn,
          "rilevabilita",
          r_local$detectability$name,
          r_local$detectability$value
        )
        sql_insert(
          r_global$conn,
          "gravita",
          r_local$magnitude$name,
          r_local$magnitude$value
        )
        verb <- "inseriti"
      } else {
        sql_update(
          r_global$conn,
          "probabilita",
          r_local$probability$name,
          r_local$probability$value,
          "probabilita_id",
          r_local$probability_id
        )
        sql_update(
          r_global$conn,
          "rilevabilita",
          r_local$detectability$name,
          r_local$detectability$value,
          "rilevabilita_id",
          r_local$detectability_id
        )
        sql_update(
          r_global$conn,
          "gravita",
          r_local$magnitude$name,
          r_local$magnitude$value,
          "gravita_id",
          r_local$magnitude_id
        )
        verb <- "modificati"
      }

      # risk calculation
      r_local$risk_value <- {
        r_local$detectability[r_local$detectability$name == "valore", "value"] *
          r_local$probability[r_local$probability$name == "valore", "value"] *
          r_local$magnitude[r_local$magnitude$name == "valore", "value"]
      } |>
        round(0)
      r_local$risk_color <- if (r_local$risk_value >= 1 &
                                r_local$risk_value <= 50) {
        "verde"
      } else if (r_local$risk_value > 50 &
                 r_local$risk_value <= 100) {
        "giallo"
      } else if (r_local$risk_value > 100 &
                 r_local$risk_value <= 1000) {
        "rosso"
      }


      risk_data <- c(
        rilevabilita_id = sql_getresid(
          r_global$conn,
          "rilevabilita",
          r_local$method_id ,
          r_local$year_id
        ),
        probabilita_id = sql_getresid(
          r_global$conn,
          "probabilita",
          r_local$method_id,
          r_local$year_id
        ),
        gravita_id = sql_getresid(
          r_global$conn,
          "gravita",
          r_local$method_id,
          r_local$year_id
        ),
        valore = r_local$risk_value,
        colore = r_local$risk_color
      )

      if (!is.null(r_local$new_data) ||
          !is.null(r_local$new_year)) {
        sql_insert(
          r_global$conn,
          "rischio",
          names(risk_data),
          risk_data |> unname()
        )
      } else {
        sql_update(
          r_global$conn,
          "rischio",
          names(risk_data),
          risk_data |> unname(),
          "rischio_id",
          r_local$risk_id
        )
      }

      r_local$done <- glue::glue(
        "Risultati {verb} per il metodo {r_local$method}",
        " basato su {r_local$technique}",
        ", in uso presso il settore {r_local$area}",
        " nell'anno {r_local$year}.</br>

        Il rischio \u00E8 <span class='risk risk_{r_local$risk_color}'>{r_local$risk_value}</span>"
      )

      shinyjs::hide("risk")
      shinyjs::show("done")

    })

    #### title of the questionnaire card ----
    output$title <- renderText(r_local$title)

    #### new data added ----
    output$`done-txt` <- renderText(r_local$done)

    #### missing recorded data ----
    output$`missing-txt` <- renderText(r_local$missing)


    output$detectability_result <- renderText({
      req(r_local$detectability_id)

      sql_getcondlist(
        r_global$conn,
        "rilevabilita",
        "valore",
        "rilevabilita_id",
        r_local$detectability_id
      )
    })


    output$probability_result <- renderText({
      req(r_local$probability_id)

      sql_getcondlist(
        r_global$conn,
        "probabilita",
        "valore",
        "probabilita_id",
        r_local$probability_id
      )
    })


    output$magnitude_result <- renderText({
      req(r_local$magnitude_id)

      sql_getcondlist(
        r_global$conn,
        "gravita",
        "valore",
        "gravita_id",
        r_local$magnitude_id
      )
    })

    output$risk_result <- renderText({
      req(r_local$detectability_id)
      req(r_local$probability_id)
      req(r_local$magnitude_id)

      glue::glue("<span class='risk risk_{r_local$risk_color}'>{r_local$risk_value}</span>")
    })

  })
}

## To be copied in the UI
# mod_insert01_ui("insert01_1")

## To be copied in the server
# mod_insert01_server("insert01_1")

