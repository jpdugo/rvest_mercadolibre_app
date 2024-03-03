db_mode <- "none"

box::use(
  shiny[moduleServer, NS, onStop],
  DT[datatable, renderDT, DTOutput],
  shinyWidgets[searchInput],
  waiter[useWaiter],
  future[plan, multicore],
  bslib[page_navbar, bs_theme, nav_spacer, nav_panel],
  bsicons[bs_icon],
  DBI,
  config,
)

box::use(
  app/view/mod_search,
  app/view/mod_compare,
  app/logic/connections[connect_mysql],
)

config <- config$get(config = db_mode)

if (Sys.info()["sysname"] == "Windows") {
  plan(multisession)
} else {
  plan(multicore)
}

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    theme = bs_theme(
      version = 5,
      preset  = "darkly",
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

    con <- connect_mysql(config$mysql$host)

    search_result <- mod_search$server("search", con)
    mod_compare$server("compare", search_result)

    onStop(function() {
      if (!is.null(con)) DBI$dbDisconnect(con) else NULL
    })
  })
}
