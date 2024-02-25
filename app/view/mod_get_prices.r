box::use(
  shiny[moduleServer, NS],
)

box::use(
  app/logic/scrape_functions[search_product_extra],
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
