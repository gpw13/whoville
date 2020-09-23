
<!-- README.md is generated from README.Rmd. Please edit that file -->

whoville
========

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/caldwellst/whotilities.svg?branch=master)](https://travis-ci.com/caldwellst/whotilities)
<!-- badges: end -->

The goal of whoville is to provide a package where useful and commonly
needed functionality is made available to R users working at the World
Health Organization. At the moment, this is a set of functions to work
with country codes and names, allowing easy conversion between names and
codes and access to regions and WHO member status.

Functions
---------

Some functions are built to help you turn names or country codes into
ISO3 codes:

-   `names_to_code()` is the workhorse function that helps match country
    names and returns standardized country codes.
-   `names_to_iso3()` is a wrapper around `names_to_code()` that
    automatically outputs ISO3 code.
-   `codes_to_iso3()` converts other country codes into ISO3 format.
-   `valid_codes()` lets you quickly check which country codes in a
    vector are recognized by the whoville package.

Other functions encourage a tidy R workflow where ISO3 codes are used as
the unique identifier for each country:

-   `iso3_to_regions()` takes in a vector of ISO3 codes and returns
    specified region values.
-   `iso3_to_codes()` takes in a vector of ISO3 codes and returns
    specified country codes.
-   `iso3_to_names()` takes in a vector of ISO3 codes and returns
    specified country names.
-   `is_who_member()` takes in a vector of ISO3 codes and checks if they
    are a WHO member state or not.

All of these functions are built on top of the `countries` data frame
also exported with the package and developed off of public datasets
provided by the World Health Organization and United Nations. Details
available through `?countries`.

Installation
------------

You can install whoville from Github with:

    devtools::install_github("caldwellst/whoville")

Usage
-----

If we have an unclean data frame with country names, we can use
`names_to_codes()` to match these to ISO3 codes. The function matches
the names vector across all possible names found in the `countries` data
frame. ISO3 codes for exact matches are always returned, but the user
has specific options for non-exact matches. They can be fuzzy matched
(the default), always made NA, or require user input to confirm fuzzy
matching results. Fuzzy matches always return a message to the user on
the confirmed match. More details available through `?names_to_codes`.

    library(whoville)

    names_to_code(c("Venezuela", "Arentina", "afghanist"))
    #> 'arentina' has no exact match. Closest name found was 'argentina'.
    #> 'afghanist' has no exact match. Closest name found was 'afghanistan'.
    #> [1] "VEN" "ARG" "AFG"

Since these functions are vectorized, we can easily use them in a normal
workflow, especially if we’re using the `tidyverse`. Below, we can clean
up our tidy names and get the correct UN region and income group for our
countries, as well as its name in Chinese:

    library(dplyr)
    df <- data.frame(c_names = c("Venezuela", "Arentina", "afghanist"))

    df %>%
      mutate(iso3 = names_to_code(c_names),
             un_region = iso3_to_regions(iso3, region = "un_region"),
             wb_ig = iso3_to_regions(iso3, region = "wb_ig"),
             name_zh = iso3_to_names(iso3, org = "un", language = "zh"))
    #>     c_names iso3 un_region wb_ig                name_zh
    #> 1 Venezuela  VEN        19   UMC 委内瑞拉玻利瓦尔共和国
    #> 2  Arentina  ARG        19   UMC                 阿根廷
    #> 3 afghanist  AFG       142   LIC                 阿富汗
