box::use(
  DBI[dbGetQuery, dbExecute],
  glue[glue_sql, glue],
)

#' Register a search in the database
#'
#' This function registers a search in the database.
#'
#' @param con The database connection object.
#' @param search The search to be registered.
#'
#' @export
register_search <- function(con, search, pages) {
  UseMethod("register_search")
}

register_search.MySQLConnection <- function(con, search, pages) {
  sql <- glue_sql("INSERT INTO Search (Search, Pages) VALUES ({search}, {pages})", .con = con)
  res <- dbExecute(con, sql)
  message(glue("Registered {res} search."))
}

register_search.NULL <- function(con, search, pages) {
  invisible(NULL)
}


#' Register a search in the database
#'
#' This function registers a search in the database.
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
