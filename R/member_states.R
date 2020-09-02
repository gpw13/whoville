#' Get membership status from ISO3 codes.
#'
#' `is_who_member()` takes in a vector of ISO3 codes and returns a logical vector
#' on whether that country is a WHO member state or not.
#'
#' @param iso3 Character vector of ISO3 codes.
#'
#' @return Logical vector.
#'
#' @export
is_who_member <- function(iso3) {
  utils::data("countries",
              envir = environment(),
              package = "whotilities")
  members <- countries[["who_member_state"]]
  idx <- match(iso3, countries[["iso3"]])
  members[idx] %in% TRUE
}

#' Get small state membership status from ISO3 codes.
#'
#' `is_small_member_state()` takes in a vector of ISO3 codes and returns a logical vector
#' on whether that country is a small WHO member state or not. In this instance,
#' small member states are defined as member states with a total population <=
#' 90,000 in mid-year 2018, according to the World Population Prospects.
#'
#' @param iso3 Character vector of ISO3 codes.
#'
#' @return Logical vector.
#'
#' @export
is_small_member_state <- function(iso3) {
  utils::data("countries",
              envir = environment(),
              package = "whotilities")
  members <- countries[["small_member_state"]]
  idx <- match(iso3, countries[["iso3"]])
  members[idx] %in% TRUE
}

#' Get large state membership status from ISO3 codes.
#'
#' `is_large_member_state()` takes in a vector of ISO3 codes and returns a logical vector
#' on whether that country is a large WHO member state or not. In this instance,
#' large member states are defined as member states with a total population >
#' 90,000 in mid-year 2018, according to the World Population Prospects.
#'
#' @param iso3 Character vector of ISO3 codes.
#'
#' @return Logical vector.
#'
#' @export
is_large_member_state <- function(iso3) {
  is_who_member(iso3) & !is_small_member_state(iso3)
}

#' Get ISO3 codes for WHO member states.
#'
#' `who_member_states()` returns ISO3 codes for WHO member states. By default,
#' it returns all countries, but can be subset to only small (less than 90,000
#' population) or large states. Useful to expand data frames to explicitly include
#' missing data for countries.
#'
#' @param include Subset of member states to return.
#'     * "all": All member states (the default)
#'     * "small": Small member states (2018 population < 90,000)
#'     * "large": Large member states (2018 population >= 90,000)
#'
#' @return A character vector of ISO3 codes.
#'
#' @export
who_member_states <- function(include = c("all", "small", "large")) {
  include <- rlang::arg_match(include)
  utils::data("countries",
              envir = environment(),
              package = "whotilities")
  x <- countries[["iso3"]]
  funs <- c("all" = "is_who_member",
            "small" = "is_small_member_state",
            "large" = "is_large_member_state")
  which_f <- funs[match(include, names(funs))]
  f <- match.fun(which_f)
  x[f(x)]
}
