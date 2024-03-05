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
connect_mysql <- function(value) {
  UseMethod("connect_mysql")
}

#' @export
connect_mysql.NULL <- function(value) {
  invisible(NULL)
}

#' @export
connect_mysql.character <- function(host) {
  print(paste("Connected to:", host))
  DBI$dbConnect(
    RMariaDB$MariaDB(),
    dbname   = "Publications",
    username = "user",
    password = "password",
    host     = host,
    port     = 3306
  )
}
