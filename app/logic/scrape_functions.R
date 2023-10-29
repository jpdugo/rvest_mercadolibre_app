# 0 Modules ---------------------------------------------------------------------------------
box::use(
  dplyr[...],
  rvest[html_nodes, html_attr, html_elements, html_text, read_html],
  purrr[map, map_dfr, keep, set_names, list_rbind, exec, pluck],
  tibble[tibble, as_tibble],
  stringr[str_replace, str_c, str_trim],
  glue[glue],
  furrr[future_map],
  rlang[list2]
)

# 1 Selectors -------------------------------------------------------------------------------------

title <- "h2.ui-search-item__title"
image <- "img.ui-pdp-image.ui-pdp-gallery__figure__image"
href <- "a.ui-search-item__group__element.ui-search-link"
price <- glue(
  "{div_2} {span}",
  div_1 = "div.ui-pdp-container__row.ui-pdp-component-list.pr-16.pl-16",
  div_2 = "div.ui-pdp-price__second-line",
  span = "span.andes-money-amount__fraction"
)
next_button <- "a.andes-pagination__link.ui-search-link[title='Siguiente']"

# 2 Functions ---------------------------------------------------------------------------------

#' Apply an \link[rvest]{rvest} function to HTML elements selected by a CSS selector
#'
#' Returns a function that takes in the result of \code{read_html}, finds the
#' given CSS selector to it, and then applies the given function to the resulting
#' elements along with any extra arguments.
#'
#' @param selector \code{character} A CSS selector
#' @param fun \code{function} pipable function to \code{html_elements}
#' @param first_arg \code{character} name of the first argument to \code{fun}
#' @param extra_args \code{list} arguments to pass to \code{fun}
#' @return A function that takes in an HTML object and applies the given CSS
#'   selector to it, then applies the given function to the resulting elements
#'   along with any extra arguments.
selector_fun <- function(selector, fun, extra_args, first_arg = "x") {
  function(.html) {
    elements <- .html |>
      html_elements(selector)
    extra <- list2(!!first_arg := elements, !!!extra_args)
    exec(fun, !!!extra)
  }
}

get_next_button <- function(.html) {
  res <- selector_fun(next_button, html_attr, list("href"))(.html)
  if (length(res)) res else FALSE
}

get_text <- selector_fun(title, html_text, list(trim = FALSE))
get_price <- selector_fun(price, html_text, list(trim = FALSE))
get_href <- selector_fun(href, html_attr, list("href"))
get_data_zoom <- selector_fun(image, html_attr, list("data-zoom"))

#' Search for a product on MercadoLibre and scrape the results
#'
#' This function takes a search string and scrapes the search results page on MercadoLibre.
#'
#' @param search_string \code{character} The search string to use
#' @return \code{data.frame} A data frame with columns:
#' \itemize{
#' \item \code{title}: Titles of the publications
#' \item \code{href}: URLs of the publications
#' }
#' @export
search_product <- function(search_string) {
  search <- search_string |>
    str_trim() |>
    str_replace("\\s+", "\\+")
  link <- glue(
    "https://listado.mercadolibre.com.ar/{search}#D[A:{search}]",
    search = search
  )

  page_info <- list()
  i <- 0
  while (is.character(link)) {
    print(glue("Page: {link}"))
    i <- i + 1
    wp <- read_html(link)
    page_info[[i]] <- tibble(
      title = get_text(wp),
      href = get_href(wp)
    )

    link <- get_next_button(wp)
  }

  list_rbind(page_info)
}

#' Extract images and prices from publications
#'
#' This function takes a data frame with 'href' and 'title' columns, where 'href' contains URLs to
#' scrape. It uses the `future_map` function to scrape each URL in parallel (assuming a
#' multisession plan is set)
#'
#' @param data \code{data.frame} With two colums:
#' \itemize{
#'  \item \code{href}: URLs to scrape
#'  \item \code{title}: Titles of the publications
#' }
#' @return A tibble with columns:
#' \itemize{
#' \item \code{title}: Titles of the publications
#' \item \code{image}: URLs of the images
#' \item \code{price}: Prices of the publications
#' \item \code{href}: URLs of the publications
#' }
#' @export
get_images <- function(data) {
  results <- data$href |>
    future_map(
      .progress = TRUE, ~ {
        res <- read_html(.x)
        list(
          images = get_data_zoom(res),
          price = get_price(res)
        )
      }
    ) |>
    set_names(data$title) |>
    (\(x) {
      tibble::tibble(
        title = names(x),
        image = map(x, ~ pluck(.x, "images")),
        price = map(x, ~ pluck(.x, "price")),
        href = data$href
      )
    })()
}
