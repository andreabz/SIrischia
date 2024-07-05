#' insert01 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom bslib navset_tab nav_panel
mod_insert01_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "d-inline-flex input-group align-items-center gap-2 mx-3",

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

    # div(
    #   id = ns("missing"),
    #   class = "container",
    #
    #   textOutput(ns("missing-txt")),
    #   bslib::input_task_button(
    #     id = ns("add"),
    #     label = "Aggiungi",
    #     icon = shiny::icon("plus")
    #   )
    # ),

    bslib::card(
      id = ns("risk"),
      bslib::card_header(textOutput(ns("title"))),

      bslib::layout_column_wrap(
        width = 1 / 4,

        bslib::card_body(
          id = ns("rilevability"),

          shiny::h5("Rilevabilità"),

          shiny::selectInput(
            inputId = ns("q_partecipazionept"),
            label = "Con che frequenza partecipi a proficency test?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_giustezza"),
            label = "Con che frequenza esegui una prova di giustezza?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_ripetibilita"),
            label = "Con che frequenza esegui una prova di ripetibilità?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_cartacontrollo"),
            label = "Con che frequenza verifichi la carta di controllo?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_selettivita"),
            label = "Con che frequenza verifichi la selettività?",
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

          shiny::h5("Probabilità"),

          shiny::selectInput(
            inputId = ns("q_personale"),
            label = "Quanti operatori sono abilitati?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_metodotipo"),
            label = "Qual è la tipologia di metodo?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_manutenzione"),
            label = "Quanti interventi di manutenzione correttiva sono stati eseguiti negli ultimi tre anni?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_risultatopt"),
            label = "Quanti proficency test hanno avuto esito non conforme negli ultimi quattro anni?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_campionianno"),
            label = "Quanti campioni sono analizzati all'anno?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_metodoaccreditato"),
            label = "La prova è accreditata?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_erroririsultato"),
            label = "Quanto spesso si sono verificati errori di calcolo o di trasferimento dati?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_esperienzametodo"),
            label = "Da quanto il metodo è in utilizzo?",
            choices = NULL
          )

          ),

        bslib::card_body(
          id = ns("magnitude"),

          shiny::h5("Gravità"),

          shiny::selectInput(
            inputId = ns("q_provasanzionatoria"),
            label = "La prova è sanzionatoria?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_dannoimmagine"),
            label = "In caso di misura errata, come sarà il danno di immagine per il laboratorio?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_perditafiducia"),
            label = "In caso di misura errata, come sarà la perdita di fiducia del cliente?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_fattoriaggravanti"),
            label = "I parametri prestazionali sono adeguati ai requisiti?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("q_provaripetibile"),
            label = "La prova può essere ripetuta?",
            choices = NULL
          )

          ),

        bslib::card_body(id = ns("result"), shiny::h5("Risultato"), )

      )


    )

  )
}

#' insert01 Server Functions
#'
#' @noRd
mod_insert01_server <- function(id, r_global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    r_local <- reactiveValues(area_id = NULL,
                              area = NULL,
                              method_id = NULL,
                              method = NULL,
                              technique_id = NULL,
                              technique = NULL,
                              year_id = NULL,
                              year = NULL,
                              title = NULL,
                              new = NULL)

    #### initialise selectinput choices ----
    observeEvent(r_global$conn, {
      # initialise the area selectinput
      updateSelectInput(session,
                        inputId = "area",
                        choices = sql_getlist(conn, "settore", "valore"))
      # initialise the year selectinput
      updateSelectInput(session,
                        inputId = "year",
                        choices = sql_getlist(conn, "anno", "valore"))

      # getting choices for all questions
      # questions inputId have a "q_" prefix
      questions <- names(input)[grepl("q_", names(input))] |>
        (\(x) {gsub(pattern = "q_", replacement = "", x)})()

      lapply(questions, function(x){
        mychoices <- sql_getlist(r_global$conn, x, "descrizione")
        myid <- paste0("q_", x)
        updateSelectInput(session, inputId = myid, choices = mychoices)
      })

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
      updateSelectInput(session,
                        inputId = "method",
                        choices = methods_area)
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
      updateSelectInput(session,
                        inputId = "tech",
                        choices = tech_method)
    })

    observeEvent(input$filter, {
      r_local$technique <- input$tech
      r_local$technique_id <- sql_getcondlist(r_global$conn,
                                              "tecnica",
                                              "tecnica_id",
                                              "valore",
                                              input$tech)
      r_local$year <-input$year
      r_local$year_id <- sql_getcondlist(r_global$conn,
                                         "anno",
                                         "anno_id",
                                         "valore",
                                         input$year)

      r_local$title <- glue::glue("Metodo {r_local$method}",
                            " basato su {r_local$technique}",
                            ", in uso presso il settore {r_local$area}",
                            " - {r_local$year}")
    })

    output$title <- renderText(r_local$title)





  })
}

## To be copied in the UI
# mod_insert01_ui("insert01_1")

## To be copied in the server
# mod_insert01_server("insert01_1")

