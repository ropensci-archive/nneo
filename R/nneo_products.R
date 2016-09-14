#' Products
#'
#' @export
#' @param x (character) a product code
#' @template curl
#' @return \code{nneo_products} returns a tibble (data.frame), and
#' \code{nneo_product} returns a list
#' @examples \dontrun{
#' ## list products
#' nneo_products()
#'
#' ## get a product
#' res <- nneo_product("DP3.30018.001")
#' res$productDescription
#'
#' ### get many products
#' ids <- c("DP3.30018.001", "DP4.00001.001", "DP3.30025.001")
#' lapply(ids, nneo_product)
#' }
nneo_products <- function(...) {
  res <- neon_parse(
    nGET(
      file.path(neon_base(), "products"),
      ...
    )
  )
  tibble::as_data_frame(res$data)
}

#' @export
#' @rdname nneo_products
nneo_product <- function(x, ...) {
  res <- neon_parse(
    nGET(
      file.path(neon_base(), "products", x),
      ...
    )
  )
  res$data
}
