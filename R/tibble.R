# Les lectures DuckDB sont systematiquement exposees sous forme de tibbles.
.db_get_tibble = function(conn, statement, ...) {
  tibble::as_tibble(DBI::dbGetQuery(conn, statement, ...))
}
