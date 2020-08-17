#' @noRd
name_cols <- function() {
  utils::data("countries",
              envir = environment(),
              package = "whotilities")
  names(dplyr::select(countries, dplyr::contains("name")))
}

#' @noRd
country_code_types <- function() {
  utils::data("countries",
              envir = environment(),
              package = "whotilities")
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
                 lngth),
         call. = FALSE)
  }
}

