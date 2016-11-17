nGET <- function(url, ...) {
  cli <- crul::HttpClient$new(url = url, opts = list(...))
  res <- cli$get()
  res$parse()
}
