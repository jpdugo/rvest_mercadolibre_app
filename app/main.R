box::use(
  shiny[moduleServer, NS],
  DT[datatable, renderDT, DTOutput],
  shinyWidgets[searchInput],
  waiter[useWaiter],
  future[plan, multicore],
  bslib[page_navbar, bs_theme, nav_spacer, nav_panel],
  bsicons[bs_icon],
)

box::use(
  app/view/mod_search,
  app/view/mod_compare,
)

plan(multicore, workers = 10)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    theme = bs_theme(
      version = 5,
      preset = "darkly",
      primary = "#00bc8c"
    ),
    title = "Search MercadoLibre",
    sidebar = NULL,
    nav_spacer(),
    mod_search$ui(ns("search")),
    mod_compare$ui(ns("compare")),
    nav_panel(title = "About", icon = bs_icon("chat-left-dots"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    search_result <- mod_search$server("search")
    mod_compare$server("compare", search_result)
  })
}
