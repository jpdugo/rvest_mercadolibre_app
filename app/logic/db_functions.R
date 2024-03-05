box::use(
  DBI[dbGetQuery, dbExecute],
  glue[glue_sql, glue, glue_collapse],
  dplyr[pull, mutate],
  stringr[str_replace_all],
)

#' Register a search in the database
#'
#' This function registers a search in the database.
#'
#' @param con \code{S4} The database connection object.
#' @param search \code{character} The search to be registered.
#' @param pages \code{character} The number of pages associated with the search.
#' @param df \code{data.frame} The table containing the search results.
#'
#' @export
register_search <- function(con, search, pages, df) {
  UseMethod("register_search")
}

register_search.MySQLConnection <- function(con, search, pages, df) {
  sql <- glue_sql(
    "INSERT INTO Search
       (Search, Pages)
     VALUES
       ({search}, {pages})",
    .con = con
  )

  dbExecute(con, sql)

  last_id <- dbGetQuery(
    con,
    glue_sql(
      "SELECT
         SearchId
       FROM Search
       WHERE
          Search = {search}
       ORDER BY
         CreationDateTime DESC
       LIMIT 1",
      .con = con
    )
  )

  values <- with(df |> mutate(title = str_replace_all(title, "'", "''")), {
    sql_table <- glue(
      "({last_id |> pull(SearchId)}, '{title}', '{href}')"
    )
  }) |>
    glue_collapse(sep = ", ")

  sql_table <- glue(
    "INSERT INTO SearchResults
    (SearchId, Title, Href)
    VALUES
      {values}",
    values = values
  )

  res <- dbExecute(con, sql_table)

  message(glue("Registered {res} rows."))
}

register_search.NULL <- function(con, search, pages, df) {
  invisible(NULL)
}


#' get Search table from db
#'
#' @param con The database connection object.
#' @param search The search to be registered.
#'
#' @export
get_search <- function(con) {
  UseMethod("get_search")
}

get_search.MySQLConnection <- function(con) {
  sql <- glue_sql(
    "SELECT
       SearchId,
       Search,
       Pages,
       CreationDateTime
     FROM Search
     ORDER BY
       SearchId DESC;",
    .con = con
  )
  dbGetQuery(con, sql)
}

get_search.NULL <- function(con) {
  invisible(NULL)
}

#' get SearchResults table from db
#'
#' @param con The database connection object.
#' @param search_id Id to filter WHERE clause.
#'
#' @export
get_search_results <- function(con, search_id) {
  UseMethod("get_search_results")
}

get_search_results.MySQLConnection <- function(con, search_id) {
  sql <- glue_sql(
    "SELECT
       SearchResultsId,
       SearchId,
       Title,
       Href
     FROM SearchResults
     WHERE
       SearchId = {search_id};",
    .con = con
  )
  dbGetQuery(con, sql)
}

get_search_results.NULL <- function(con, search_id) {
  invisible(NULL)
}
