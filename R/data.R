#' Country metadata from the WHO and UN.
#'
#' A dataset containing names, codes, and other attributes
#' for countries included in World Health Organization and
#' United Nations datasets.
#'
#' @format A data frame with `r nrow(countries)` rows and `r ncol(countries)` variables:
#' \describe{
#'   \item{iso3}{\href{https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3}{ISO3 code}}
#'   \item{iso2}{\href{https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2}{ISO2 code}}
#'   \item{ds}{\href{http://www.unece.org/trans/roadsafe/distinguishing_signs.html}{distinguishing sign of state of registration}}
#'   \item{fips}{USA government \href{https://en.wikipedia.org/wiki/List_of_FIPS_country_codes}{FIPS 10-4 code}}
#'   \item{ioc}{\href{https://en.wikipedia.org/wiki/List_of_IOC_country_codes}{International Olympic Committee code}}
#'   \item{itu}{\href{https://en.wikipedia.org/wiki/List_of_IOC_country_codes}{International Telecommunication Union code}}
#'   \item{marc}{\href{http://www.loc.gov/marc/countries/cou_home.html}{MARC 21 codes}}
#'   \item{wmo}{\href{https://cpdb.wmo.int/}{World Meteorological Organizantion code}}
#'   \item{who_code}{\href{https://apps.who.int/gho/data/node.metadata.COUNTRY?lang=en}{World Health Organization code}}
#'   \item{m49}{\href{https://en.wikipedia.org/wiki/UN_M49}{United Nations M49 code}}
#'   \item{who_member_state}{logical, is country a WHO member state?}
#'   \item{who_name_en}{WHO country name, English}
#'   \item{who_name_es}{WHO country name, Spanish}
#'   \item{who_name_fr}{WHO country name, French}
#'   \item{un_name_en}{UN country name, English}
#'   \item{un_name_ru}{UN country name, Russian}
#'   \item{un_name_fr}{UN country name, French}
#'   \item{un_name_es}{UN country name, Spanish}
#'   \item{un_name_ar}{UN country name, Arabic}
#'   \item{un_name_zh}{UN country name, Chinese}
#'   \item{alt_name_en}{Alternative (non-official) country name, English}
#'   \item{alt_name_2_en}{Another alternative (non-official) country name, English}
#'   \item{former_name_en}{Former country name, English}
#'   \item{who_region_en}{WHO region name, English}
#'   \item{who_region_code}{WHO region code}
#'   \item{wb_ig_2017_en}{World Bank Income Group name, 2017 classification}
#'   \item{wb_ig_2017_code}{World Bank Income Group code, 2017 classification}
#'   \item{un_region_en}{UN region name, English}
#'   \item{un_region_code}{UN region code}
#'   \item{un_subregion_en}{UN sub-region name, English}
#'   \item{un_subregion_code}{UN sub-region code}
#' }
#' @source \href{https://apps.who.int/gho/data/node.metadata.COUNTRY?lang=en}{World Health Organization country data}
#' @source \href{https://unstats.un.org/unsd/methodology/m49/overview/}{United Nations Country data}
"countries"

#' Country population data from the UN.
#'
#' A dataset containing ISO3 code, year, and gender disaggregated population
#' figures for countries, using the most recent United Nations, Department
#' of Economic and Social Affairs, Population Division estimates.
#'
#' Population data is based on the most recent estimates from the United Nations,
#' Department of Economic and Social Affairs, Population Division. For years
#' 2021 - 2100, the population model assumes medium fertility throughout that
#' time. See the
#' \href{https://population.un.org/wpp/DefinitionOfProjectionVariants/}{World
#' Population Prospect's definition of projection variants} for more details
#' on the assumptions and model used.
#'
#' @format A data frame with `r nrow(un_population)` rows and `r ncol(un_population)` variables:
#' \describe{
#'   \item{iso3}{\href{https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3}{ISO3 code}}
#'   \item{year}{Year, currently 1950 - 2100}
#'   \item{female}{Total population - female}
#'   \item{male}{Total population - male}
#'   \item{total}{Total population}
#' }
#' @source \href{https://population.un.org/wpp/Download/Standard/Population/}{United Nations, Department of Economic and Social Affairs, Population Division (2019). World Population Prospects 2019, Online Edition. Rev. 1.}
"un_population"
