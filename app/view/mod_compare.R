box::use(
  shiny[
    NS, tagList, moduleServer, eventReactive, a, selectInput, observeEvent, req, reactive,
    titlePanel
  ],
  app / view / mod_upload_excel,
  app / view / mod_proxy_dt,
  app / logic / utils[format_href],
  dplyr[mutate, anti_join],
  bslib[...],
  bsicons[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  card1 <- card(
    card_header(mod_upload_excel$ui(ns("upload_excel"))),
    mod_proxy_dt$ui(ns("upload_table"))
  )

  card2 <- card(
    card_header(titlePanel("New Publications")),
    mod_proxy_dt$ui(ns("new_publications"))
  )

  nav_panel(
    title = "Compare",
    icon = bs_icon("layout-split"),
    layout_column_wrap(
      width = 1 / 2,
      height = 300,
      card1,
      card2
    )
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
        upload() |> format_href()
      }),
      not_visible = NULL,
      short_cols = "href",
      reset_paging = TRUE,
      page_length = 50,
      not_searchable = "href",
      ordering = TRUE,
      callback = 1
    )

    new_publications <- reactive({
      req(current_search(), upload())
      current_search() |>
        anti_join(
          upload(),
          by = c("title")
        )
    })

    mod_proxy_dt$server(
      id = "new_publications",
      df = reactive({
        req(new_publications())
        new_publications() |> format_href()
      }),
      not_visible = NULL,
      short_cols = "href",
      reset_paging = TRUE,
      page_length = 50,
      not_searchable = "href",
      ordering = TRUE,
      callback = 1
    )
  })
}
