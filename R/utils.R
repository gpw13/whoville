#' @noRd
name_cols <- function() {
  data("countries", envir = environment())
  names(dplyr::select(countries, dplyr::contains("name")))
}

#' @noRd
country_code_types <- function() {
  data("countries", envir = environment())
  names(countries[,1:10])
}

#' @noRd
assert_logical <- function(x) {
  arg_name <- paste(sys.call()[-1])
  class_x <- class(x)
  if (!class(x) == "logical") {
    stop(sprintf("%s must be a character vector of length one, not %s",
                 arg_name,
                 class_x),
         call. = FALSE)
  }
  lngth <- length(x)
  if (lngth != 1) {
    stop(sprintf("%s must be of length one, not length %s",
                 arg_name,
                 lgnth),
         call. = FALSE)
  }
}

