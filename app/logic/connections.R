box::use(
  RMariaDB,
  DBI,
  config[get],
)

#' Connects to MySQL database
#'
#' This function connects to a MySQL database using the provided configuration value.
#' It retrieves the MySQL host from the configuration and uses it to establish a connection.
#'
#' @param value  <\code{character}> Passed to `config::get()` unless it is NULL.
#' @return A database connection object.
#' @export
connect_mysql <- function(config) {
  UseMethod("connect_mysql")
}

#' @export
connect_mysql.NULL <- function(config) {
  invisible(NULL)
}

#' @export
connect_mysql.list <- function(config) {
  on.exit({
    print(paste("Connected to:", config$mysql$host))
  })

  DBI$dbConnect(
    RMariaDB$MariaDB(),
    dbname   = "Publications",
    username = config$mysql$user,
    password = config$mysql$password,
    host     = config$mysql$host,
    port     = 3306
  )
}

connect_mysql.config <- function(config) {
  connect_mysql.list(as.list(config))
}
