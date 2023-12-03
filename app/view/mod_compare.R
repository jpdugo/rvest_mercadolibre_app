box::use(
  shiny[
    NS, tagList, moduleServer, eventReactive, a, selectInput, observeEvent, req, reactive
  ],
  app/view/mod_upload_excel,
  app/view/mod_proxy_dt,
  purrr[map_vec],
  dplyr[mutate]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    mod_upload_excel$ui(ns("upload_excel")),
    mod_proxy_dt$ui(ns("upload_table"))
  )
}

#' @export
server <- function(id, current_search) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    upload <- mod_upload_excel$server("upload_excel")

    mod_proxy_dt$server(
        id = "upload_table",
        df = reactive({
          req(upload())
          upload() |> mutate(
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
  })
}
