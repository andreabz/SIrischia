#' view02 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_view02_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' view02 Server Functions
#'
#' @noRd 
mod_view02_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_view02_ui("view02_1")
    
## To be copied in the server
# mod_view02_server("view02_1")
