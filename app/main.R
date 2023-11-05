box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput],
  future[plan, multisession],
  tidyr[unnest],
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
    title = "Search MercadoLibre",
    sidebar = NULL,
    nav_spacer(),
    nav_panel(
      "Search",
      card(full_screen = TRUE, card_header("")),
      icon = bs_icon("search")
    ),
    nav_panel("Compare", icon = bs_icon("layout-split")),
    nav_panel("About", icon = bs_icon("chat-left-dots"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}
