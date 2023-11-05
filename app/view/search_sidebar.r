# Load required packages
box::use(
  shiny[...],
  bslib[...],
  shinyWidgets[searchInput, pickerInput, updatePickerInput, updateSearchInput]
)

#' UI function for search sidebar
#'
#' @param id
#'
#' @return The UI for the search sidebar
#'
#' @export
ui <- function(id) {
  ns <- NS(id)
  sidebar(
    searchInput(
      inputId = ns("search"),
      label = "Search ML",
      placeholder = "kaweco",
      btnSearch = icon("magnifying-glass"),
      btnReset = icon("xmark"),
      width = "100%"
    ),
    pickerInput(
      inputId = ns("history"),
      label = "History",
      choices = NULL,
      options = list(
        `live-search` = TRUE
      )
    )
  )
}

#' Server function for search sidebar
#'
#' @param id
#' @param previous_search \code{NULL} or a \code{character} vector of previous searches
#'
#' @return Returns a \code{reactive} containing a \code{character} of length 1
#'
#' @export
server <- function(id, previous_search = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    history <- if (is.null(previous_search)) reactiveVal(NULL) else reactiveVal(previous_search)

    observeEvent(input$search, {
      req(input$search)
      history(c(input$search, history()))
      updatePickerInput(
        session,
        "history",
        selected = NULL,
        choices = history(),
        clearOptions = FALSE
      )
    })

    observeEvent(input$history, {
      updateSearchInput(
        session,
        "search",
        value = input$history,
        trigger = FALSE
      )
    })

    reactive(input$search)
  })
}
