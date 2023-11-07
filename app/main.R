box::use(
  shiny[
    bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput, icon, eventReactive, validate,
    need
  ],
  DT[datatable, renderDT, DTOutput],
  shinyWidgets[searchInput, useSweetAlert],
  future[plan, multisession],
  app / view / search_sidebar,
  app / logic / scrape_functions[...],
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
    theme = bs_theme(version = 5, preset = "darkly"),
    title = "Search MercadoLibre",
    sidebar = NULL,
    nav_spacer(),
    nav_panel(
      "Search",
      useSweetAlert(theme = "borderless"),
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

    search <- search_sidebar$server("search_sidebar")

    current_search <- eventReactive(search$string, {
      if (search$string == "") NULL else search$string |> search_product(search$max_pages)
    })

    output$results_table <- renderDT({
      validate(
        need(!is.null(current_search()), "Nothing to show yet!")
      )

      datatable(current_search())
    })
  })
}
