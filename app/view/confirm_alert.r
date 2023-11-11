box::use(
  shiny[
    NS, tagList, moduleServer, eventReactive, is.reactive, selectInput,
    observeEvent, req
  ],
  shinyWidgets[confirmSweetAlert],
  purrr
)

#' @title Create a confirmAlert linked to a button input
#'
#' @description This module creates a confirmAlert linked to a button input.
#' When the button is clicked, a SweetAlert confirmation dialog is displayed.
#' If the user confirms the action, the specified function is executed.
#'
#' @param id \code{character} An identifier for the module that should be unique within the Shiny
#' app.
#' @param button \code{reactive} A reactive input button that triggers the confirmAlert
#' when clicked.
#' @param text \code{shiny.tag} The message to display in the alert. This can be a character string
#' or a UI component.
#' @param fn  \code{function} A function object or a lambda function to be executed when the user
#' confirms the action in #'the confirmAlert dialog.
#' @param ignore_init \code{logical} Whether to ignore the initial value of the button input.
#'
#' @export
#'
#' @examples
#' box::use(
#'   app / view / mod_confirmAlert
#' )
#'
#' ui <- fluidPage(
#'   actionButton("my_button", "Click me"),
#' )
#'
#' server <- function(input, output, session) {
#'   # Initialize the confirmAlert module
#'   mod_confirmAlert$server(
#'     id = "confirm_alert",
#'     button = reactive(input$my_button),
#'     text = "Are you sure you want to proceed?",
#'     fn = \() {
#'       message("User confirmed the action.")
#'     }
#'   )
#' }
#'
#' shinyApp(ui, server)
#'
server <- function(id, button, text, fn, ignore_init = FALSE) {
  stopifnot(is.reactive(button))
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      observeEvent(button(),
        {
          confirmSweetAlert(
            inputId             = ns("confirm"),
            title               = NULL,
            text                = text,
            type                = "warning",
            allowEscapeKey      = TRUE,
            showCloseButton     = TRUE,
            closeOnClickOutside = TRUE
          )
        },
        ignoreInit = ignore_init
      )

      observeEvent(input$confirm, {
        if (isTRUE(input$confirm)) {
          purrr$exec(fn)
        }
      })
    }
  )
}
