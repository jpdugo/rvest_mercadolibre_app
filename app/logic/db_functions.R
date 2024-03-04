box::use(
  DBI[dbGetQuery, dbExecute],
  glue[glue_sql, glue, glue_collapse],
  dplyr[pull],
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
  
  values <- with(df, {
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

  dbExecute(con, sql_table)

  message(glue("Registered {res} search."))
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
get_search <- function(con, search) {
  UseMethod("get_search")
}

get_search.MySQLConnection <- function(con, search) {
  sql <- glue_sql(
    "SELECT
       Search
     FROM Search
     ORDER BY
       SearchId DESC;",
    .con = con
  )
  dbGetQuery(con, sql)
}

get_search.NULL <- function(con, search) {
  invisible(NULL)
}
