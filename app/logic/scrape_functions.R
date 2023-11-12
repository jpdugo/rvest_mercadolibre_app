# 0 Modules ---------------------------------------------------------------------------------
box::use(
  dplyr[...],
  rvest[html_nodes, html_attr, html_elements, html_text, read_html],
  purrr[map, map_dfr, keep, set_names, list_rbind, exec, pluck],
  tibble[tibble, as_tibble],
  stringr[str_replace_all, str_c, str_trim],
  glue[glue],
  furrr[future_map],
  rlang[list2],
  future[nbrOfWorkers],
  shiny[setProgress]
)

# 1 Selectors -------------------------------------------------------------------------------------

title <- "h2.ui-search-item__title"
image <- "img.ui-pdp-image.ui-pdp-gallery__figure__image"
href <- "a.ui-search-item__group__element.ui-search-link"
href_alt <- glue(
  "{div} {a}",
  div = "div.andes-carousel-snapped__slide",
  a = "a.ui-search-link"
)
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
#' @return Returns a function that takes in an HTML object and applies the given CSS
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
get_href_alternative <- selector_fun(href_alt, html_attr, list("href"))
get_data_zoom <- selector_fun(image, html_attr, list("data-zoom"))

#' Search for a product on MercadoLibre and scrape the results
#'
#' This function takes a search string and scrapes the search results page on MercadoLibre.
#'
#' @param search_string \code{character} The search string to use
#' @param max_pages \code{integer} The maximum number of pages to scrape
#' @return Returns \code{data.frame} A data frame with columns:
#' \itemize{
#' \item \code{title}: Titles of the publications
#' \item \code{href}: URLs of the publications
#' }
#' @export
search_product <- function(search_string, max_pages = 0, shiny_progress = FALSE) {
  # message to the console if the max_pages is not a positive integer
  if (!is.numeric(max_pages) || max_pages < 0) {
    warning("max_pages must be a positive integer, or 0 for no limit")
  }

  search <- search_string |>
    str_trim() |>
    str_replace_all("\\s+", "\\+")
  link <- glue(
    "https://listado.mercadolibre.com.ar/{search}#D[A:{search}]",
    search = search
  )

  page_info <- list()
  i <- 0
  while (is.character(link) && i < max_pages) {
    print(glue("Page: {link}"))
    i <- i + 1
    wp <- read_html(link)

    href_result <- get_href(wp)
    if (length(href_result) == 0) {
      href_result <- get_href_alternative(wp)
    }

    page_info[[i]] <- tibble(
      title = get_text(wp),
      href = href_result
    )

    link <- get_next_button(wp)

    if (shiny_progress) setProgress(value = i / if (max_pages == 0) 42 else max_pages)
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
#' @return Returns a tibble with columns:
#' \itemize{
#' \item \code{title}: Titles of the publications
#' \item \code{image}: URLs of the images
#' \item \code{price}: Prices of the publications
#' \item \code{href}: URLs of the publications
#' }
#' @export
search_product_extra <- function(data) {
  extract <- function(.x) {
    res <- read_html(.x)
    list(
      images = get_data_zoom(res),
      price = get_price(res)
    )
  }

  create_tibble <- function(x, data) {
    tibble::tibble(
      title = data$title,
      image = purrr::map(x, ~ purrr::pluck(.x, "images")),
      price = purrr::map(x, ~ purrr::pluck(.x, "price")),
      href = data$href
    )
  }

  fn <- if (nbrOfWorkers() == 1) map else future_map

  data$href |>
    fn(.progress = TRUE, extract) |>
    create_tibble(data)
}
