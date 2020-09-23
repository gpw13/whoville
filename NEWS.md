# whotilities 0.1.2

* Country data updated to use WHO xMart4 REF_MART data. This has created changes across all functions and in the package.
* WHO formal and short names now available in all languages
* Reduced the number of country codes available to ISO3, ISO2, ISO (numeric), WHO code, and M49
* Increased numbers of countries to 248
* Accessed World Bank Income Group data directly from World Bank website, now have income group classifications for 1987 - 2019 (corresponds to fiscal year 1989 - 2021)

# whotilities 0.1.1

* Added a `NEWS.md` file to track changes to the package.
* Added `who_member_states()` to quickly extract ISO3 codes for all WHO member states

# whotilities 0.1.0

* Initial release of package
* Publication of `countries` dataframe with codes, names, and regional categorizations
* Added functions to coerce names to codes (and vice-versa) released
* Added functions to get key country information using ISO3 codes
