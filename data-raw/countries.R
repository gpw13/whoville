library(tidvyerse)
library(readxl)

# Adding in WHO data

who_c <- read_excel("data-raw/who_countries.xlsx") %>%
  transmute(iso3 = ISO,
            iso2 = ISO2,
            ds = DS,
            fips = FIPS,
            ioc = IOC,
            itu = ITU,
            marc = MARC,
            wmo = WMO,
            who_code = WHO,
            who_name_en = DisplayString,
            who_member_state = WHOLEGALSTATUS %in% "M",
            who_region_en = WHO_REGION,
            who_region_code = WHO_REGION_CODE,
            wb_ig_2017_en = WORLD_BANK_INCOME_GROUP,
            wb_ig_2017_code = WORLD_BANK_INCOME_GROUP_CODE,
            who_name_es = SHORTNAMEES,
            who_name_fr = SHORTNAMEFR)

# Adding UN data

un_c <- read_excel("data-raw/un_countries.xlsx",
                   "english",
                   col_types = "text") %>%
  select(iso3 = `ISO-alpha3 Code`,
         m49 = `M49 Code`,
         un_name_en = `Country or Area`,
         un_region_en = `Region Name`,
         un_region_code = `Region Code`,
         un_subregion_en = `Sub-region Name`,
         un_subregion_code = `Sub-region Code`)

# Pulling in names in different languages

ln_codes <- c("ru", "fr", "es", "ar", "zh")
ln_names <- c("russian", "french", "spanish", "arabic", "chinese")

for (i in 1:5) {
  df <- read_excel("data-raw/un_countries.xlsx",
                   ln_names[i],
                   col_types = "text") %>%
    select(m49 = `M49 Code`,
           !!sym(paste0("un_name_", ln_codes[i])) := `Country or Area`)
  un_c <- left_join(un_c, df, by = "m49")
}

# Pulling in IHME countries and former country names

alt_c <- read_excel("data-raw/alt_countries.xlsx") %>%
  select(iso3,
         alt_name_en = altname,
         alt_name_2_en = altname2,
         former_name_en = formername)

# Merging together

countries <- left_join(who_c, un_c, by = "iso3") %>%
  full_join(alt_c, by = "iso3") %>%
  select(iso3,
         iso2:who_code,
         m49,
         who_member_state,
         who_name_en,
         who_name_es,
         who_name_fr,
         un_name_en,
         un_name_ru:former_name_en,
         who_region_en:wb_ig_2017_code,
         un_region_en:un_subregion_code)

usethis::use_data(countries, overwrite = TRUE)
