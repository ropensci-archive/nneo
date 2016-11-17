nGET <- function(url, query = list(), ...) {
  cli <- crul::HttpClient$new(url = url, opts = list(...))
  res <- cli$get(query = query)
  res$parse()
}
