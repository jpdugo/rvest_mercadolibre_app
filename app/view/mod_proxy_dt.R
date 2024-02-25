box::use(
  shiny[
    tagList, reactive, observeEvent, is.reactive, tags, div, reactiveValues
  ],
  shiny,
  DT[renderDT, DTOutput, dataTableProxy, replaceData, datatable, formatCurrency, updateFilters, JS],
  glue[glue],
  shinyjs[hidden, click],
  stringr[str_detect],
  rlang[is_empty],
)

#' @title Create a DT Module
#'
#' @description Create a datatable module with a proxy object associated
#'
#' @param id \code{character}. Name of the id
#'
#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  tagList(
    div(
      id = ns("dt_element"),
      DTOutput(ns("table"))
    ),
    div(
      style = "visibility: hidden;",
      shiny$actionButton(ns("callback_button"), "")
    )
  )
}

#' @title Create a DT
#'
#' @description Create a datatable with a proxy object associated
#'
#' @param id \code{character}. Name of the id
#' @param df \code{dataframe}. Data
#' @param not_visible \code{numeric}. Indices of the columns to hide from displaying
#' @param reset_paging \code{boolean}. Reset the paging when data is reloaded
#' @param page_length \code{numeric}. Number of rows in each row
#' @param center \code{character}. Columns that will have its content centered
#' @param short_cols \code{character}. Columns that will have a max width, can also be numeric
#' @param row_names \code{boolean}. Show row names
#' @param not_searchable \code{character}. Names of the columns that will not be searchable
#' @param editable \code{boolean}. If the table is editable.
#' @param currency \code{character}. Names of the columns that will be formatted as currency
#' @param callback \code{boolean}. Activate a callback to listen for double clicks in a row
#' @param ordering \code{boolean}. ordering of rows
#' @param extensions \code{list}. List of extensions to be used in the table
#'
#' @export
server <- function(id,
                   df,
                   not_visible,
                   reset_paging,
                   page_length,
                   center = NULL,
                   short_cols = NULL,
                   row_names = TRUE,
                   not_searchable = NULL,
                   currency = NULL,
                   editable = FALSE,
                   callback = NULL,
                   ordering = TRUE,
                   extensions = list()) {
  stopifnot(is.reactive(df))
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      keys <- if (!is_empty(extensions) && any(str_detect(extensions, pattern = "KeyTable"))) {
        TRUE
      } else {
        FALSE
      }

      activate_table <- shiny$reactiveVal(FALSE)

      observeEvent(df(), {
        if (nrow(df()) > 0) {
          if (!activate_table()) {
            activate_table(TRUE)
          }
        } else {
          activate_table(FALSE)
        }
      })

      callback <- if (!is.null(callback)) {
        glue::glue(
          "table.on('dblclick', 'td',
            function() {{
              document.getElementById('{input_name_1}').click()
            }}
          );
          // Add a button to reset the table
          let $container = $(table.table().container()).find('div:first');
          $container.css('float', 'none')
          let $button = $(
            '<button class=\"btn btn-default action-button\">Reload Table</button>'
          ).css('float', 'left');
          $container.prepend($button);
          $button.on('click', function() {{
            Shiny.setInputValue('{input_name_2}', '', {{priority: 'event'}});
          }});",
          input_name_1 = ns("callback_button"),
          input_name_2 = ns("reload")
        ) |> JS()
      } else {
        JS("return table;")
      }

      observeEvent(activate_table(),
        {
          output$table <- renderDT({
            shiny$validate(
              shiny$need(activate_table(), "Nothing to show yet!")
            )

            input$reload

            datatable(
              data = shiny$isolate(df()),
              selection = list(
                mode     = "single",
                selected = NULL,
                target   = "row"
              ),
              escape = FALSE,
              style = "auto",
              extensions = extensions,
              options = list(
                dom = "frtip",
                keys = keys,
                rowReorder = FALSE,
                ordering = ordering,
                scrollX = TRUE,
                pageLength = page_length,
                columnDefs = list(
                  list(
                    targets = short_cols,
                    createdCell = JS(
                      "function(td) {
                         $(td).css({
                          'max-width': '150px',
                          'overflow': 'hidden',
                          'text-overflow': 'ellipsis',
                          'white-space': 'nowrap'
                        });
                      }"
                    )
                  ),
                  list(visible = FALSE, targets = not_visible),
                  list(className = "dt-center", targets = center),
                  list(targets = c(not_searchable), searchable = FALSE)
                )
              ),
              class = "row-border compact hover",
              rownames = row_names,
              filter = "none",
              editable = editable,
              callback = callback
            ) |>
              formatCurrency(currency)
          })
        },
        once = TRUE
      )

      dproxy <- dataTableProxy(outputId = "table")

      observeEvent(df(),
        {
          replaceData(
            proxy          = dproxy,
            data           = df(),
            clearSelection = FALSE,
            resetPaging    = reset_paging,
            rownames       = row_names # https://github.com/rstudio/DT/issues/403
          )

          updateFilters(dproxy, df())
        },
        ignoreInit = TRUE
      )

      return_values <- reactiveValues(
        rows = NULL,
        button = NULL,
        return_values = NULL
      )

      observeEvent(input$callback_button, {
        return_values$button <- input$callback_button
      })

      observeEvent(input$table_rows_selected,
        {
          return_values$rows <- input$table_rows_selected
        },
        ignoreNULL = FALSE
      )

      observeEvent(input$table_cell_edit, {
        return_values$cell_edit <- input$table_cell_edit
      })

      return_values
    }
  )
}
