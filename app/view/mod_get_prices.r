box::use(
  shiny[...],
  app/logic/scrape_functions[...],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("get_prices"), "Get Prices")
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}
