#' @noRd
name_cols <- function() {
  names(dplyr::select(whoville::countries, dplyr::contains("name") & !dplyr::contains("region")))
}

#' Country code types available
#'
#' `country_code_types()` provides a character vector of all country code types
#' available in the `countries` data frame.
#'
#' @return Character vector of country code types.
#' @examples
#' country_code_types()
#'
#' @export
country_code_types <- function() {
  nms <- names(whoville::countries)
  nms[1:match("gbd_code", nms)]
}

#' Years of WB IG classifications available
#'
#' `wb_ig_years()` provides a numeric vector of all years of World Bank Income
#' Group classifications available in the `countries` data frame.
#' @examples
#' wb_ig_years()
#'
#' @export
wb_ig_years <- function() {
  nms <- names(whoville::countries)
  nms <- nms[grepl("wb_ig_", nms)]
  as.numeric(gsub("[^0-9]", "", nms))
}

#' @noRd
assert_wb_ig_years <- function(x) {
  yrs <- wb_ig_years()
  if (!(length(x) == 1 & (x %in% yrs))) {
    stop(sprintf("`year` must be a valid 4 digit year for a WB IG classification, between %1.0f and %1.0f.",
                 min(yrs),
                 max(yrs)),
         call. = FALSE)
  }
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

