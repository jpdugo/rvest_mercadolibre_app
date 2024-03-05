box::use(
  shiny[
    eventReactive, reactive, tags, req, showNotification, withProgress, tagList, observeEvent
  ],
  shiny,
  waiter[useWaiter, waiter_show, waiter_hide, spin_chasing_dots],
  shinyjs[useShinyjs],
  shinyWidgets[useSweetAlert],
  glue[glue],
  bslib[layout_sidebar, card_header, sidebar, card, nav_panel],
  bsicons[bs_icon],
  dplyr[pull, slice, select],
)

box::use(
  app/view/mod_proxy_dt,
  app/view/mod_download_excel,
  app/logic/utils[format_href],
  app/logic/db_functions[get_search, get_search_results],
  app/view/mod_download_excel,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  nav_panel(
    title = "History",
    icon = bs_icon("clock-history"),
    card(
      card_header(""),
      layout_sidebar(
        sidebar = sidebar(
          id = "sidebar",
          width = "350px",
          shiny$actionButton(
            inputId = ns("refresh"),
            label = tags$div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              tags$span("Refresh", style = "flex-grow: 1; text-align: left;"),
              tags$span(bs_icon("arrow-counterclockwise"), style = "text-align: right;")
            )
          ),
          shiny$div(mod_proxy_dt$ui(ns("history"))),
          mod_download_excel$ui(ns("download_excel"))
        ),
        mod_proxy_dt$ui(ns("history_table"))
      )
    )
  )
}

#' @export
server <- function(id, con) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data <- eventReactive(input$refresh, {
      get_search(con)
    }, ignoreNULL = FALSE)

    selected_search <- mod_proxy_dt$server(
      id = "history",
      df = reactive({
        req(data())
        data() |> select(-SearchId)
      }),
      not_visible = NULL,
      short_cols = "Search",
      row_names = FALSE,
      reset_paging = FALSE,
      page_length = 100,
      not_searchable = NULL,
      ordering = FALSE,
      callback = NULL,
      small_font = c("Search", "Pages", "CreationDateTime"),
      dom = "f",
      colnames = NULL,
      clear_selection = TRUE
    )

    search_results_id <- eventReactive(
      list(selected_search$rows, input$refresh),
      {
        if (!is.null(selected_search$rows)) {
          slice(data(), selected_search$rows) |> pull(SearchId)
        }
      }
    )

    search_results_data <- reactive({
      req(search_results_id())
      get_search_results(con, search_results_id()) |>
        select(title = Title, href = Href) |>
        format_href()
    })

    mod_proxy_dt$server(
      id = "history_table",
      df = search_results_data,
      not_visible = NULL,
      short_cols = "href",
      reset_paging = TRUE,
      page_length = 50,
      not_searchable = "href",
      ordering = TRUE,
      callback = 1
    )

    mod_download_excel$server(
      id = "download_excel",
      data = search_results_data
    )

  })
}
