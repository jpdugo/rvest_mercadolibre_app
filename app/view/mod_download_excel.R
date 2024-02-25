box::use(
  shiny[observeEvent, icon, tagList],
  shiny,
  shinyjs[click, show, hide, hidden],
  tibble[tibble],
  openxlsx[write.xlsx],
  glue[glue],
  shinyWidgets[sendSweetAlert],
  shinyvalidate,
)

#' @title Download Excel
#'
#' @description
#' This module creates a download button that allows users to download a reactive
#' dataframe/tibble in Excel format
#'
#' @param id <\code{character}> Name of the id
#'
#' @export
#' @name download_excel-module
ui <- function(id) {
  ns <- shiny$NS(id)
  tagList(
    hidden(
      shiny$textInput(
        inputId     = ns("file_name"),
        label       = "File Name",
        placeholder = "Name of the file to download",
      )
    ),
    hidden(
      shiny$actionButton(
        inputId = ns("download"),
        label   = "Download Data",
        width   = "100%",
        icon    = icon("download")
      )
    ),
    shiny$downloadButton(outputId = ns("download_data"), label = "") |>
      shiny$tagAppendAttributes(
        style = "visibility: hidden;"
      )
  )
}

#' @param id \code{character}. Name of the id
#' @param data \code{data.frame}.
#'
#' @export
#' @rdname download_excel-module
server <- function(id, data) {
  shiny$moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$download, {
        if (!iv$is_valid()) {
          sendSweetAlert(
            session = session,
            title   = "Error",
            text    = "Please insert a valid name for the file",
            type    = "error"
          )
        } else {
          click("download_data")
        }
      })

      observeEvent(data(), {
        show("download")
        show("file_name")
      })

      output$download_data <- shiny$downloadHandler(
        filename = function() {
          glue(
            "{name}-{time}.xlsx",
            name = input$file_name,
            time = Sys.time()
          )
        },
        content = function(file) {
          write.xlsx(
            x         = data(),
            file      = file,
            colWidths = c(45, 10)
          )
        }
      )
      iv <- shinyvalidate$InputValidator$new()
      iv$add_rule("file_name", shinyvalidate$sv_required(message = "Required"))
      iv$enable()
    }
  )
}
