box::use(
  shiny[
    moduleServer, NS, eventReactive, reactive, a, req, showNotification, withProgress, tagList
  ],
  waiter[useWaiter, waiter_show, waiter_hide, spin_chasing_dots],
  shinyjs[useShinyjs],
  shinyWidgets[useSweetAlert],
  app/view/mod_search_sidebar,
  app/logic/scrape_functions[...],
  app/view/mod_proxy_dt,
  app/view/mod_download_excel,
  app/logic/utils[format_href],
  purrr[...],
  glue[glue],
  dplyr[...],
  bslib[...],
  bsicons[...]
)
#' Search Module
#'
#' Ui that encapsulates the search sidebar and the search results table modules.
#'
#' @export
ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "Search",
    # there is no other place to put dependencies, maybe in the title argument?
    useShinyjs(),
    useSweetAlert(theme = "borderless"),
    useWaiter(),
    icon = bs_icon("search"),
    card(
      card_header(""),
      layout_sidebar(
        sidebar = sidebar(
          id = "sidebar",
          width = "350px",
          mod_search_sidebar$ui(ns("search_sidebar")),
          mod_download_excel$ui(ns("download_excel"))
        ),
        mod_proxy_dt$ui(ns("fast_table"))
      )
    )
  )
}

#' Search Module
#'
#' Server function that encapsulates the search sidebar and the search results table modules.
#'
#' @param id
#' @export
server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      search <- mod_search_sidebar$server("search_sidebar")

      current_search <- eventReactive(list(
        search$string,
        search$reload
      ), {
        req(search$string)
        waiter_show(
          html = spin_chasing_dots(),
          color = "#2a2a2a"
        )
        res <- tryCatch(
          {
            withProgress(
              expr = {
                res <- search$string |> search_product(search$max_pages, shiny_progress = TRUE)
              },
              message = "Searching...",
              detail = "This may take a while",
              value = 0
            )
            showNotification("Calculation complete", type = "message")
            return(res)
          },
          error = function(e) {
            showNotification(
              paste("An error occurred:", e$message),
              type = "error"
            )
            waiter_hide()
            NULL
          }
        )
        return(res)
      })

      mod_proxy_dt$server(
        id = "fast_table",
        df = reactive({
          req(current_search())
          waiter_hide()
          current_search() |> format_href()
        }),
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
        data = current_search
      )

      current_search
    }
  )
}
