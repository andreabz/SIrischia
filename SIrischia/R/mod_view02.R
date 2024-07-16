#' view02 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT render
#' @importFrom bslib layout_column_wrap value_box value_box_theme
mod_view02_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_column_wrap(
      width = 1 / 4,

      div(
        class = "d-inline-flex input-group align-items-center gap-2 mx-3",

        #### top area, method and year selector ----
        shiny::selectInput(
          inputId = ns("area"),
          label = "Settore",
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

      bslib::value_box(
        id = ns("alert_box"),
        title = "Rischio non accettabile",
        value = textOutput(ns("alert")),
        p(textOutput(ns("alert_year"))),
        p("nell'intero laboratorio"),
        showcase = shiny::icon("face-frown"),
        theme = bslib::value_box_theme(bg = "#C70039", fg = "white"),
        class = "small-box",
        max_height = "150px"
      ),
      bslib::value_box(
        id = ns("warning_box"),
        title = "Rischio tollerabile",
        value = textOutput(ns("warning")),
        p(textOutput(ns("warning_year"))),
        p("nell'intero laboratorio"),
        showcase = shiny::icon("face-meh"),
        theme = bslib::value_box_theme(bg = "#FFC300", fg = "black"),
        class = "small-box",
        max_height = "150px"
      ),
      bslib::value_box(
        id = ns("fine_box"),
        title = "Rischio sotto controllo",
        value = textOutput(ns("fine")),
        p(textOutput(ns("fine_year"))),
        p("nell'intero laboratorio"),
        showcase = shiny::icon("face-grin"),
        theme = bslib::value_box_theme(bg = "#04AA6D", fg = "white"),
        class = "small-box",
        max_height = "150px"
      )
    ),

    DT::DTOutput(ns("table"))

  )
}

#' view02 Server Functions
#'
#' @noRd
#' @import data.table
mod_view02_server <- function(id, r_global) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    r_local <- reactiveValues(
      this_year = NULL,
      this_year_id = NULL,
      this_year_txt = NULL,
      area_id = NULL,
      area = NULL,
      risk_data = data.table::data.table()
    )

    observeEvent(r_global$conn, {
      # initialise the area selectinput
      updateSelectInput(
        session,
        inputId = "area",
        choices = sql_getlist(r_global$conn, "settore", "valore")
      )

      r_local$this_year <- Sys.Date() |> year()
      r_local$this_year_id <- sql_getcondlist(r_global$conn,
                                              "anno",
                                              "anno_id",
                                              "valore",
                                              r_local$this_year)
      r_local$this_year_txt <- glue::glue("nell'anno {r_local$this_year}")
    })

    observeEvent(input$filter, {
      r_local$area <- input$area
      r_local$area_id <- sql_getcondlist(r_global$conn,
                                         "settore",
                                         "settore_id",
                                         "valore",
                                         input$area)
      r_local$risk_data <- format_risk(r_global$conn, r_local$area)
    })

    output$table <- DT::renderDT({
      req(colnames(r_local$risk_data))

      riskDT(r_local$risk_data)
    })

    output$alert <- renderText({
      n <- sql_countrisk(r_global$conn, "rosso", r_local$this_year_id)
      n_text <- ifelse(n == 0, "nessun", n)
      method_text <- ifelse(n > 1, "metodi", "metodo")
      glue::glue("{n_text} {method_text}")
    })

    output$warning <- renderText({
      n <- sql_countrisk(r_global$conn, "giallo", r_local$this_year_id)
      n_text <- ifelse(n == 0, "nessun", n)
      method_text <- ifelse(n > 1, "metodi", "metodo")
      glue::glue("{n_text} {method_text}")
    })

    output$fine <- renderText({
      n <- sql_countrisk(r_global$conn, "verde", r_local$this_year_id)
      n_text <- ifelse(n == 0, "nessun", n)
      method_text <- ifelse(n > 1, "metodi", "metodo")
      glue::glue("{n_text} {method_text}")
    })

    output$alert_year <- output$warning_year <- output$fine_year <- renderText({
      r_local$this_year_txt
    })

  })
}

## To be copied in the UI
# mod_view02_ui("view02_1")

## To be copied in the server
# mod_view02_server("view02_1")
