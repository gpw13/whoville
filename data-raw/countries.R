library(tidyverse)
library(readxl)
library(wppdistro)
library(xmart4)
library(pdftools)

# Adding in WHO data

xmart_c <- xmart4_table("REFMART", "REF_COUNTRY") %>%
  transmute(iso3 = CODE_ISO_3,
            iso2 = CODE_ISO_2,
            iso_numeric = CODE_ISO_NUMERIC,
            who_code = CODE_WHO,
            who_short_name_en = NAME_SHORT_EN,
            who_formal_name_en = NAME_FORMAL_EN,
            who_short_name_ar = NAME_SHORT_AR,
            who_formal_name_ar = NAME_FORMAL_AR,
            who_short_name_es = NAME_SHORT_ES,
            who_formal_name_es = NAME_FORMAL_ES,
            who_short_name_fr = NAME_SHORT_FR,
            who_formal_name_fr = NAME_FORMAL_FR,
            who_short_name_ru = NAME_SHORT_RU,
            who_formal_name_ru = NAME_FORMAL_RU,
            who_short_name_zh = NAME_SHORT_ZH,
            who_formal_name_zh = NAME_FORMAL_ZH,
            who_member = WHO_LEGAL_STATUS %in% "M",
            sovereign_iso3 = SOVEREIGN_ISO_3,
            who_region = GRP_WHO_REGION)

# WB IG data
temp = tempfile(fileext = ".xls")
url = "http://databank.worldbank.org/data/download/site-content/OGHIST.xls"
download.file(url, temp, mode = "wb")

wb_ig <- readxl::read_xls(temp,
                          sheet = "Country Analytical History",
                          skip = 11,
                          col_names = c("iso3", "name", 1987:2019),
                          na = "..") %>%
  transmute(iso3 = iso3,
            across(matches("[0-9]{4}"),
                   ~case_when(
                     .x %in% c("L", "H") ~ paste0(.x, "IC"),
                     .x %in% c("LM", "UM") ~ paste0(.x, "C")
                   ),
                   .names = "wb_ig_{.col}"))

# Adding UN data

un_c <- read_excel("data-raw/un_countries.xlsx",
                   "english",
                   col_types = "text") %>%
  transmute(iso3 = `ISO-alpha3 Code`,
            m49 = `M49 Code`,
            un_ldc = !is.na(`Least Developed Countries (LDC)`),
            un_lldc = !is.na(`Land Locked Developing Countries (LLDC)`),
            un_sids = !is.na(`Small Island Developing States (SIDS)`),
            un_name_en = `Country or Area`,
            un_region = `Region Code`,
            un_region_name_en = `Region Name`,
            un_subregion = `Sub-region Code`,
            un_subregion_name_en = `Sub-region Name`)

# Pulling in names in different languages

ln_codes <- c("ru", "fr", "es", "ar", "zh")
ln_names <- c("russian", "french", "spanish", "arabic", "chinese")

for (i in 1:5) {
  df <- read_excel("data-raw/un_countries.xlsx",
                   ln_names[i],
                   col_types = "text") %>%
    select(m49 = `M49 Code`,
           !!sym(paste0("un_name_", ln_codes[i])) := `Country or Area`,
           !!sym(paste0("un_region_name_", ln_codes[i])) := `Region Name`,
           !!sym(paste0("un_subregion_name_", ln_codes[i])) := `Sub-region Name`)
  un_c <- left_join(un_c, df, by = "m49")
}

# Pulling in IHME countries and former country names

alt_c <- read_excel("data-raw/alt_countries.xlsx") %>%
  select(iso3,
         alt_name_en = altname,
         alt_name_2_en = altname2,
         alt_name_3_en = altname3,
         alt_name_4_en = altname4,
         alt_name_5_en = altname5,
         former_name_en = formername,
         former_name_2_en = formername2)

# Merging together

countries <- left_join(xmart_c, un_c, by = "iso3") %>%
  left_join(alt_c, by = "iso3") %>%
  left_join(wb_ig, by = "iso3") %>%
  select(iso3,
         iso2:who_code,
         m49,
         sovereign_iso3,
         who_member,
         un_ldc,
         un_lldc,
         un_sids,
         who_short_name_en:who_formal_name_zh,
         un_name_en,
         un_name_ru,
         un_name_fr,
         un_name_es,
         un_name_ar,
         un_name_zh,
         alt_name_en:former_name_2_en,
         who_region,
         un_region,
         un_subregion,
         un_region_name_en,
         un_subregion_name_en,
         un_region_name_ru,
         un_subregion_name_ru,
         un_region_name_fr,
         un_subregion_name_fr,
         un_region_name_es,
         un_subregion_name_es,
         un_region_name_ar,
         un_subregion_name_ar,
         un_region_name_zh,
         un_subregion_name_zh,
         wb_ig_1987:wb_ig_2019)

# Getting small member states information

small_countries <- wpp_population %>%
  filter(year == 2018,
         sex == "both",
         total < 90000) %>%
  pull(iso3)

who_member_small <- countries$iso3 %in% small_countries

countries <- countries %>%
  add_column(who_member_small,
             .after = "who_member")

# Adding in OECD data
# From World Bank link with ISO3 codes of all member states

oecd <- readxl::read_excel("data-raw/oecd.xlsx")
oecd_member <- countries$iso3 %in% oecd$ISO3

countries <- countries %>%
  add_column(oecd_member,
             .after = "who_member_small")

# Adding in high-income GBD countries
# File edited to just have the high-income countries

gbd <- readxl::read_excel("data-raw/gbd_sdi.xlsx",
                          skip = 1) %>%
  transmute(iso3 = whoville::names_to_iso3(Location))

gbd_high_income <- countries$iso3 %in% gbd$iso3

countries <- countries %>%
  add_column(gbd_high_income,
             .after = "oecd_member")

# Writing out result

usethis::use_data(countries, overwrite = TRUE)
