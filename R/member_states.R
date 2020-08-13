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
  utils::data("countries", envir = environment())
  members <- countries[["who_member_state"]]
  idx <- match(iso3, countries[["iso3"]])
  members[idx] %in% TRUE
}

#' Get membership status from ISO3 codes.
#'
#' `is_small_member_state()` takes in a vector of ISO3 codes and returns a logical vector
#' on whether that country is a  small WHO member state or not. In this instance,
#' small member states are defined as member states with a 2018 total population <=
#' 90,000 in mid-year 2018, according to the World Population Prospects.
#'
#' @param iso3 Character vector of ISO3 codes.
#'
#' @return Logical vector.
#'
#' @export
is_small_member_state <- function(iso3) {
  utils::data("countries", envir = environment())
  members <- countries[["small_member_state"]]
  idx <- match(iso3, countries[["iso3"]])
  members[idx] %in% TRUE
}
