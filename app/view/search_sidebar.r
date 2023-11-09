# Load required packages
box::use(
  glue[glue],
  shiny[...],
  bslib[...],
  shinyWidgets[searchInput, pickerInput, updatePickerInput, updateSearchInput],
  app / view / confirm_alert
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
    width = "350px",
    searchInput(
      inputId = ns("search"),
      label = "Search ML",
      placeholder = "Enter your search...",
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
    ),
    radioButtons(
      inputId = ns("n_pages"),
      label = "Number of Pages:",
      choices = c("1", "5", "10", "Custom", "All")
    )
  )
}

#' Server function for search sidebar
#'
#' @param id
#' @param previous_search \code{NULL} or a \code{character} vector of previous searches
#'
#' @return Returns a \code{reactiveValues} list containing a \code{character} of length 1 and
#' a \code{numeric} of length 1
#'
#' @export
server <- function(id, previous_search = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    search_details <- reactiveValues(
      string = NULL,
      max_pages = NULL
    )

    custom_value <- reactiveVal(0)

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
      search_details$string <- input$search
    })

    observeEvent(input$history, {
      updateSearchInput(
        session,
        "search",
        value = input$history,
        trigger = FALSE
      )
    })

    observeEvent(input$n_pages, {
      switch(input$n_pages,
        "1" = search_details$max_pages <- 1,
        "5" = search_details$max_pages <- 5,
        "10" = search_details$max_pages <- 10,
        "Custom" = custom_value(custom_value() + 1), # hack to force the confirm_alert to re-render
        "All" = search_details$max_pages <- 0
      )
    })

    confirm_alert$server(
      id = "confirm_alert",
      button = custom_value,
      text = div(
        p("Enter the number of pages to scrape:"),
        numericInput(
          inputId = ns("pages"),
          label = NULL,
          min = 1,
          max = 100,
          value = 1,
          width = "100%"
        )
      ),
      fn = \() {
        search_details$max_pages <- input$pages
      },
      ignore_init = TRUE
    )

    observeEvent(search_details$max_pages, {
      if (search_details$max_pages > 0) {
        updateRadioButtons(
          session = session,
          inputId = "n_pages",
          label   = glue("Number of Pages: {search_details$max_pages}")
        )
      } else {
        updateRadioButtons(
          session = session,
          inputId = "n_pages",
          label   = glue("Number of Pages: All")
        )
      }
    })

    search_details
  })
}
