box::use(
  shiny[moduleServer, NS],
  DT[datatable, renderDT, DTOutput],
  shinyWidgets[searchInput, useSweetAlert],
  waiter[useWaiter],
  future[plan, multicore],
  shinyjs[useShinyjs],
  bslib[...],
  bsicons[...],
  app / view / mod_search,
  app / view / mod_compare
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
    nav_panel(
      title = "Search",
      useShinyjs(),
      useSweetAlert(theme = "borderless"),
      useWaiter(),
      mod_search$ui(ns("search")),
      icon = bs_icon("search")
    ),
    nav_panel(
      title = "Compare",
      mod_compare$ui(ns("compare")),
      icon = bs_icon("layout-split")
    ),
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
