box::use(
  shiny[column, eventReactive, titlePanel, div, HTML],
  shiny,
  utils[read.csv],
  shinyjs[reset],
  shinyWidgets[sendSweetAlert],
  readxl[read_xlsx],
  assertr[verify, has_all_names, has_class],
  htmltools[tagQuery],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  file_input <- shiny$fileInput(
    inputId = ns("file1"),
    label = "",
    accept = c(".xlsx"),
    width = "100%"
  )

  shiny$tagList(
    shiny$fluidRow(
      column(6, titlePanel("Upload Excel")),
      column(6, div(
        tagQuery(file_input)$find("label")$filter(\(x, i) i == 1)$remove()$allTags()
      ))
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(
    id,
    function(input, output, session) {
      excel_uploaded <- eventReactive(input$file1, {
        tryCatch(
          {
            read_xlsx(path = input$file1$datapath) |>
              verify(has_all_names("title", "href")) |>
              (\(.x) verify(.x, nrow(.x) > 0))() |> # horrible with native pipe
              verify(has_class("title", "href", class = "character"))
          },
          error = function(e) {
            if (grepl("assertr stopped execution", e$message)) {
              warning(
                "The uploaded file has more than two columns so the action could not be completed"
              )
              sendSweetAlert(
                session = session,
                title = "Upload failed",
                type = "error",
                text = HTML(
                  "Verify that the uploaded file has at least one row
                   and no more than two character columns:
                   <br>- <b>title</b> <br>-  <b>href</b>"
                ),
                html = TRUE
              )
              reset("file1")
            } else {
              warning("An error occurred: ", e$message)
            }
            return(NULL)
          }
        )
      })

      excel_uploaded
    }
  )
}
