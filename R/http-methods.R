nGET <- function(url, ...) {
  res <- httr::GET(url, ...)
  httr::stop_for_status(res)
  c8(res)
}
