box::use(
  shiny[
    bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput, icon, eventReactive, validate,
    need, reactive, a, req, showNotification
  ],
  DT[datatable, renderDT, DTOutput],
  shinyWidgets[searchInput, useSweetAlert],
  future[plan, multisession],
  app / view / search_sidebar,
  app / logic / scrape_functions[...],
  app / view / proxy_dt,
  purrr[...],
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
    theme = bs_theme(
      version = 5,
      preset = "darkly",
      primary = "#00bc8c"
    ),
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
          proxy_dt$ui(ns("fast_table"))
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
      req(search$string)
      res <- tryCatch(
        {
          search$string |> search_product(search$max_pages)
        },
        error = function(e) {
          showNotification(
            paste("An error occurred:", e$message),
            type = "error"
          )
          NULL
        }
      )
      return(res)
    })

    proxy_dt$server(
      id = "fast_table",
      df = reactive({
        req(current_search())
        current_search() |> mutate(
          href = map_vec(
            .x = href,
            .f = ~ as.character(a(href = .x, .x)),
            .ptype = character()
          )
        )
      }),
      not_visible = NULL,
      short_cols = "href",
      reset_paging = TRUE,
      page_length = 50,
      not_searchable = "href",
      ordering = FALSE,
      callback = 1
    )
  })
}
