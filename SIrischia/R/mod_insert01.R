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
        inputId = ns("method"),
        label = "Metodo",
        choices = NULL
      ),

      shiny::selectInput(
        inputId = ns("site"),
        label = "Sede",
        choices = NULL
      ),

      shiny::selectInput(
        inputId = ns("tech"),
        label = "Tecnica",
        choices = NULL
      ),

      shiny::selectInput(
        inputId = ns("matrix"),
        label = "Matrice",
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
      bslib::card_header("metodo XY, sede, matrice, anno"),

      bslib::layout_column_wrap(
        width = 1 / 4,

        bslib::card_body(
          id = ns("rilevability"),

          shiny::h3("Rilevabilità"),

          shiny::selectInput(
            inputId = ns("npt"),
            label = "Con che frequenza partecipi a proficency test?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("nrepeatability"),
            label = "Con che frequenza esegui una prova di ripetibilità?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("nqcchart"),
            label = "Con che frequenza verifichi la carta di controllo?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("nselectivity"),
            label = "Con che frequenza verifichi la selettività?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("ncontamination"),
            label = "Con che frequenza verifichi la contaminazione?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("ncalibration"),
            label = "Con che frequenza esegui la taratura?",
            choices = NULL
          )

        ),

        bslib::card_body(
          id = ns("probability"),

          shiny::h3("Probabilità"),

          shiny::selectInput(
            inputId = ns("npers"),
            label = "Quanti operatori sono abilitati?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("methodtype"),
            label = "Qual è la tipologia di metodo?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("nmanu"),
            label = "Quanti interventi di manutenzione correttiva sono stati eseguiti negli ultimi tre anni?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("pt"),
            label = "Quanti proficency test hanno avuto esito non conforme negli ultimi quattro anni?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("ncampioni"),
            label = "Quanti campioni sono analizzati all'anno?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("accredia"),
            label = "La prova è accreditata?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("nmistakes"),
            label = "Quanto spesso si sono verificati errori di calcolo o di trasferimento dati?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("exp"),
            label = "Da quanto il metodo è in utilizzo?",
            choices = NULL
          )

          ),

        bslib::card_body(
          id = ns("magnitude"),

          shiny::h3("Gravità"),

          shiny::selectInput(
            inputId = ns("sanz"),
            label = "La prova è sanzionatoria?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("labimage"),
            label = "In caso di misura errata, come sarà il danno di immagine per il laboratorio?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("trust"),
            label = "In caso di misura errata, come sarà la perdita di fiducia del cliente?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("req"),
            label = "I parametri prestazionali sono adeguati ai requisiti?",
            choices = NULL
          ),

          shiny::selectInput(
            inputId = ns("newtest"),
            label = "La prova può essere ripetuta?",
            choices = NULL
          )

          ),

        bslib::card_body(id = ns("result"), shiny::h3("Risultato"), )

      )


    )

  )
}

#' insert01 Server Functions
#'
#' @noRd
mod_insert01_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_insert01_ui("insert01_1")

## To be copied in the server
# mod_insert01_server("insert01_1")

