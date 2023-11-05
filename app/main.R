box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput, icon, eventReactive],
  DT[datatable, renderDT, DTOutput],
  shinyWidgets[searchInput],
  future[plan, multisession],
  app / view / search_sidebar,
  app / logic / scrape_functions,
  glue[glue],
  dplyr[...],
  bslib[...],
  bsicons[...]
)

plan(multisession, workers = 10)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    theme = bs_theme(version = 5),
    title = "Search MercadoLibre",
    sidebar = NULL,
    nav_spacer(),
    nav_panel(
      "Search",
      card(
        full_screen = TRUE,
        card_header(""),
        layout_sidebar(
          sidebar = search_sidebar$ui(ns("search_sidebar")),
          div(
            DTOutput(ns("results_table"))
          )
        )
      ),
      icon = bs_icon("search")
    ),
    nav_panel("Compare", icon = bs_icon("layout-split")),
    nav_panel("About", icon = bs_icon("chat-left-dots"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    search_string <- search_sidebar$server("search_sidebar")

    current_search <- eventReactive(search_string(), {
      search_string() |> scrape_functions$search_product()
    })

    output$results_table <- renderDT({
      datatable(current_search())
    })
  })
}
