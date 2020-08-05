#' Get regions from ISO3 country codes.
#'
#' `iso3_to_regions()` takes in a vector of ISO3 codes and returns regions
#' specified by the user. Currently, this can be codes or English names for
#' UN regions/subregions, WHO regions, and World Bank Income Groups.
#'
#' @param iso3 Character vector of ISO3 codes.
#' @param region Type of region to return. Can be
#' any of "who_region" (default), "un_region", "un_subregion", or "wb_ig".
#' @param return Either "code" (default) or language code to specify the region
#' name. Only "en" currently available.
#'
#' @return Character vector.
#'
#' @export
iso3_to_regions <- function(iso3,
                           region = "who_region",
                           return = "code") {
  rlang::arg_match(region, c("who_region", "un_region", "un_subregion", "wb_ig"))
  rlang::arg_match(return, c("en", "code"))

  data("countries", envir = environment())
  rgx <- sprintf("^%s.*%s$", region, return)
  regions <- countries[[which(grepl(rgx, names(countries)))]]
  idx <- match(iso3, countries[["iso3"]])
  regions[idx]
}

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
  data("countries", envir = environment())
  members <- countries[["who_member_state"]]
  idx <- match(iso3, countries[["iso3"]])
  members[idx]
}

#' Check if ISO3 codes present in `countries` data frame.
#'
#' `valid_iso3()` takes in a vector of ISO3 codes and returns a logical vector
#' on whether that country code is present in `countries[["iso3"]]`
#'
#' @param iso3 Character vector of ISO3 codes.
#'
#' @return Logical vector.
#'
#' @export
valid_iso3 <- function(iso3) {
  data("countries", envir = environment())
  iso3 %in% countries[["iso3"]]
}

#' Get country names from ISO3 country codes.
#'
#' `iso3_to_names()` takes in a vector of ISO3 codes and returns names as
#' specified by the user. Currently, this can be United Nations names in
#' all 6 official languages or World Health Organization names in English,
#' Spanish, or French.
#'
#' @param iso3 Character vector of ISO3 codes.
#' @param org Official names of which organization to return. Can be
#' either "who" (default) or "un".
#' @param language A character value specifying the language of the country names.
#' Should be specified using the ISO2 language code. Defaults to "en", but matching
#' available for all 6 official WHO languages. Possible values are "en", "es",
#' "ar", "fr", "ru", and "zh". Only "en", "es" and "fr" available if org is
#' "who".
#'
#' @return Character vector.
#'
#' @export
iso3_to_names <- function(iso3,
                           org = "who",
                           language = "en") {
  rlang::arg_match(org, c("who", "un"))
  assert_who_language(org, language)

  data("countries", envir = environment())
  rgx <- sprintf("^%s_name_%s$", org, language)
  names <- countries[[which(grepl(rgx, names(countries)))]]
  idx <- match(iso3, countries[["iso3"]])
  names[idx]
}

#' @noRd
assert_who_language <- function(org, language) {
  if (org == "who") {
    if (!(language %in% c("en", "es", "fr"))) {
      stop('`language` must be one of "en", "es", or "fr" if `org` == "who"',
           call. = FALSE)
    }
  } else {
    rlang::arg_match(language, c("en", "es", "ru", "ar", "zh", "fr"))
  }
}

#' Get country codes from ISO3 country codes.
#'
#' `iso3_to_codes()` takes in a vector of ISO3 codes and returns a country code
#' vector specified by the user.
#'
#' @param iso3 Character vector of ISO3 codes.
#' @param code A character value specifying the type of country code to return.
#' All possible values available through `country_code_types()`.
#'
#' @return Character vector.
#'
#' @export
iso3_to_codes <- function(iso3, code) {
  rlang::arg_match(code, country_code_types())

  data("countries", envir = environment())
  codes <- countries[[code]]
  idx <- match(iso3, countries[["iso3"]])
  codes[idx]
}
