box::use(
  dplyr[mutate],
  purrr[map_vec],
  shiny[a],
)

#' @export
format_href <- function(df) {
  df |> mutate(
    href = map_vec(
      .x = href,
      .f = ~ as.character(a(href = .x, .x, target = "_blank")),
      .ptype = character()
    )
  )
}
