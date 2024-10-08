db_mode <- "none"

box::use(
  shiny[moduleServer, NS, onSessionEnded],
  DT[datatable, renderDT, DTOutput],
  shinyWidgets[searchInput],
  waiter[useWaiter],
  future[plan, multicore],
  bslib[page_navbar, bs_theme, nav_spacer, nav_panel],
  bsicons[bs_icon],
  DBI,
  config,
  logger,
)

box::use(
  app/view/mod_search,
  app/view/mod_compare,
  app/view/mod_search_prev,
  app/logic/connections[connect_mysql],
)

logger$log_threshold(if (db_mode != "deploy") {logger$DEBUG} else {logger$INFO})

config <- if (db_mode != "none") config$get(config = db_mode)

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
    mod_search_prev$ui(ns("search_prev")),
    mod_compare$ui(ns("compare"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- connect_mysql(config)
    search_result <- mod_search$server("search", con)
    search_previous <- mod_search_prev$server("search_prev", con)
    mod_compare$server("compare", search_result, search_previous)
    onSessionEnded(function() {
      if (!is.null(con)) DBI$dbDisconnect(con) else NULL
    })
  })
}
