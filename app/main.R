db_mode <- "local"

box::use(
  shiny[moduleServer, NS, onSessionEnded],
  shiny,
  DT[datatable, renderDT, DTOutput],
  shinyWidgets[searchInput],
  waiter[useWaiter],
  future[plan, multicore],
  bslib[page_navbar, bs_theme, nav_spacer, nav_panel],
  bsicons[bs_icon],
  DBI,
  config,
  cookies
)

box::use(
  app/view/mod_search,
  app/view/mod_compare,
  app/view/mod_search_prev,
  app/logic/connections[connect_mysql],
)

config <- if (db_mode != "none") config$get(config = db_mode)

if (Sys.info()["sysname"] == "Windows") {
  plan(multisession)
} else {
  plan(multicore)
}

#' @export
ui <- function(id) {
  ns <- NS(id)
  cookies$add_cookie_handlers(
    page_navbar(
      theme = bs_theme(
        version = 5,
        preset  = "darkly",
        primary = "#00bc8c"
      ),
      title = shiny$sliderInput(
        ns("number_selector"),
        label = paste(
          "Select a number.",
          "This selector sets the cookie value.",
          "It also initializes with the cookie value.",
          "Refresh to see it remembered.",
          sep = "\n"
        ),
        min = 1,
        max = 10,
        value = 1
      ),
      sidebar = NULL,
      nav_spacer(),
      mod_search$ui(ns("search")),
      mod_search_prev$ui(ns("search_prev")),
      mod_compare$ui(ns("compare"))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- connect_mysql(config)

    shiny$observeEvent(
    input$number_selector,
    {
      cookies$set_cookie(
        cookie_name = "selected_number",
        cookie_value = input$number_selector
      )
    }
   )

   shiny$observeEvent(
    cookies$get_cookie("selected_number"),
    shiny$updateSliderInput(
      inputId = "number_selector",
      value = cookies$get_cookie("selected_number")
    ),
    once = TRUE
   )

    search_result <- mod_search$server("search", con)
    search_previous <- mod_search_prev$server("search_prev", con)
    mod_compare$server("compare", search_result, search_previous)
    onSessionEnded(function() {
      if (!is.null(con)) DBI$dbDisconnect(con) else NULL
    })
  })
}
