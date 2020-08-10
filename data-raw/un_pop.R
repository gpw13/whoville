library(tidyverse)
library(readxl)
library(whotilities)

# Adding in UN pop data
# 2019 estimates

read_un_data <- function(path, sheet = NULL, gender) {
  read_excel(path, sheet = sheet, skip = 16) %>%
    filter(Type == "Country/Area",
           `Region, subregion, country or area *` != "Channel Islands") %>%
    mutate(iso3 = names_to_code(`Region, subregion, country or area *`)) %>%
    pivot_longer(matches("^[0-9]{4}$"),
                 names_to = "year",
                 values_to = gender) %>%
    transmute(iso3, year, !!sym(gender) := as.numeric(!!sym(gender)) * 1000)
}

# Female
un_pop_fem <- read_un_data("data-raw/un_pop_female.xlsx",
                           gender = "female") %>%
  bind_rows(read_un_data("data-raw/un_pop_female.xlsx",
                         sheet = "MEDIUM VARIANT",
                         gender = "female"))

# Male
un_pop_male <- read_un_data("data-raw/un_pop_female.xlsx",
                            gender = "male") %>%
  bind_rows(read_un_data("data-raw/un_pop_female.xlsx",
                         sheet = "MEDIUM VARIANT",
                         gender = "male"))

# Total

un_population <- full_join(un_pop_fem, un_pop_male, by = c("iso3", "year")) %>%
  mutate(total = male + female) %>%
  distinct

usethis::use_data(un_population, overwrite = TRUE)
