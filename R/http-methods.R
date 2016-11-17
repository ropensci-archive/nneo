nGET <- function(url, query = list(), ...) {
  cli <- crul::HttpClient$new(url = url, opts = list(...))
  res <- cli$get(query = query)
  errs(res)
  res$parse()
}

errs <- function(x) {
  if (x$status_code > 201) {
    xx <- jsonlite::fromJSON(x$parse())
    if ("error" %in% names(xx)) {
      # match by status code
      fun <- match_err(x$status_code)$new()
      fun$mssg <- xx$error$detail
      fun$do_verbose(x)
    } else {
      # if no error message in response, just general stop
      fauxpas::http(x)
    }
  }
}

match_err <- function(code) {
  tmp <- paste0("fauxpas::",
                grep("HTTP*", getNamespaceExports("fauxpas"), value = TRUE))
  fxns <- lapply(tmp, function(x) eval(parse(text = x)))
  codes <- vapply(fxns, function(z) z$public_fields$status_code, 1)
  fxns[[which(code == codes)]]
}
