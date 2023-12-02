box::use(
  shiny[
    moduleServer, NS, eventReactive, reactive, a, req, showNotification, withProgress, tagList
  ],
  waiter[useWaiter, waiter_show, waiter_hide, spin_chasing_dots],
  app/view/search_sidebar,
  app/logic/scrape_functions[...],
  app/view/proxy_dt,
  app/view/download_excel,
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
  tagList(
    card(
      card_header(""),
      layout_sidebar(
        sidebar = sidebar(
          id = "sidebar",
          width = "350px",
          search_sidebar$ui(ns("search_sidebar")),
          download_excel$ui(ns("download_excel"))
        ),
        proxy_dt$ui(ns("fast_table"))
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

      search <- search_sidebar$server("search_sidebar")

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

      proxy_dt$server(
        id = "fast_table",
        df = reactive({
          req(current_search())
          waiter_hide()
          current_search() |> mutate(
            href = map_vec(
              .x = href,
              .f = ~ as.character(a(href = .x, .x, target = "_blank")),
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

      download_excel$server(
        id = "download_excel",
        data = current_search
      )

      current_search
    }
  )
}
