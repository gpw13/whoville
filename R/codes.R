#' Get regions from ISO3 country codes.
#'
#' `iso3_to_regions()` takes in a vector of ISO3 codes and returns regions
#' specified by the user. Currently, this can be codes or English names for
#' UN regions/subregions, WHO regions, and World Bank Income Groups.
#'
#' @param iso3 Character vector of ISO3 codes.
#' @param region Type of region to return. Can be
#' any of "who_region" (default), "un_region", "un_subregion", or "wb_ig".
#' @param year For World Bank Income Group only, specify the year to return.
#'
#' @return Character vector.
#'
#' @export
iso3_to_regions <- function(iso3,
                            region = "who_region",
                            year = max(wb_ig_years())) {
  rlang::arg_match(region, c("who_region", "un_region", "un_subregion", "wb_ig"))
  if (region == "wb_ig") {
    assert_wb_ig_years(year)
    rgx <- paste(region, year, sep = "_")
  } else {
    rgx <- region
  }

  utils::data("countries",
              envir = environment(),
              package = "whoville")

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
  utils::data("countries",
              envir = environment(),
              package = "whoville")
  members <- countries[["who_member_state"]]
  idx <- match(iso3, countries[["iso3"]])
  members[idx] %in% TRUE
}

#' Check if country codes are present in `countries` data frame.
#'
#' `valid_codes()` takes in a vector of country codes and returns a logical vector
#' on whether that country code is present in `countries[["iso3"]]`
#'
#' @param codes Character vector of country codes.
#' @param type A character value specifying the type of country code supplied.
#' All possible values available through `country_code_types()`.
#'
#' @return Logical vector.
#'
#' @export
valid_codes <- function(codes, type = "iso3") {
  rlang::arg_match(type, country_code_types())
  utils::data("countries",
              envir = environment(),
              package = "whoville")
  codes %in% countries[["type"]]
}

#' Get country names from ISO3 country codes.
#'
#' `iso3_to_names()` takes in a vector of ISO3 codes and returns names as
#' specified by the user. Currently, this can be World Health Organization names,
#' both short and formal, in all 6 official languages and United Nations names in
#' all 6 official languages.
#'
#' @param iso3 Character vector of ISO3 codes.
#' @param org Official names of which organization to return. Can be
#' either "who" (default) or "un".
#' @param type "short" (default) or "formal" name to return. Only used when org is "who".
#' @param language A character value specifying the language of the country names.
#' Should be specified using the ISO2 language code. Defaults to "en", but matching
#' available for all 6 official WHO languages. Possible values are "en", "es",
#' "ar", "fr", "ru", and "zh".
#'
#' @return Character vector.
#'
#' @export
iso3_to_names <- function(iso3,
                          org = c("who", "un"),
                          type = c("short", "formal"),
                          language = c("en", "es", "ru", "ar", "zh", "fr")) {
  org <- rlang::arg_match(org)
  language <- rlang::arg_match(language)
  if (org == "who") {
    type <- rlang::arg_match(type)
    org <- paste(org, type, sep = "_")
  }
  utils::data("countries",
              envir = environment(),
              package = "whoville")
  rgx <- sprintf("^%s_name_%s$", org, language)
  names <- countries[[which(grepl(rgx, names(countries)))]]
  idx <- match(iso3, countries[["iso3"]])
  names[idx]
}

#' Get country codes from ISO3 country codes.
#'
#' `iso3_to_codes()` takes in a vector of ISO3 codes and returns a country code
#' vector specified by the user.
#'
#' @param iso3 Character vector of ISO3 codes.
#' @param type A character value specifying the type of country code to return.
#' All possible values available through `country_code_types()`.
#'
#' @return Character vector.
#'
#' @export
iso3_to_codes <- function(iso3, type) {
  rlang::arg_match(type, country_code_types())

  utils::data("countries",
              envir = environment(),
              package = "whoville")
  codes <- countries[[type]]
  idx <- match(iso3, countries[["iso3"]])
  codes[idx]
}

#' Get ISO3 codes from other country codes.
#'
#' `codes_to_iso3()` takes in a vector of country codes and returns ISO3 codes.
#'
#' @param codes Character vector of country codes.
#' @inheritParams iso3_to_codes
#'
#' @return Character vector.
#'
#' @export
codes_to_iso3 <- function(codes, type) {
  rlang::arg_match(type, country_code_types())
  utils::data("countries",
              envir = environment(),
              package = "whoville")
  iso3 <- countries[["iso3"]]
  idx <- match(codes, countries[[type]])
  iso3[idx]
}
