nGET <- function(url, ...) {
  cli <- crul::HttpClient$new(url = url, opts = list(...))
  res <- cli$get()
  res$parse()
  #res <- httr::GET(url, ...)
  #httr::stop_for_status(res)
  #c8(res)
}
