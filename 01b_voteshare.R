library(tidyverse)
library(glue)
library(readxl)
library(janitor)


placenames <- read_excel("data/input/dailykos/by-cd_placenames.xlsx") %>%
  rename_all(~str_to_lower(str_replace_all(str_replace_all(.x, "%", "pct"), "(\\.+|\\s|-)", "_"))) %>%
  mutate_if(is.numeric, ~.x/100) %>%
  rename(cd = district)

pres_08 <- read_excel("data/input/dailykos/by-cd_pres.xlsx", sheet = 2) %>%
  clean_names() %>%
  mutate(redist_cycle = "2002")

pres_kos <- read_excel("data/input/dailykos/by-cd_pres.xlsx", skip = 1) %>%
  select(1:9) %>%
  rename_all(~str_to_lower(str_replace_all(.x, "\\.+", "_"))) %>%
  mutate(party = str_remove_all(party, "[\\(\\)]")) %>%
  mutate_if(is.numeric, ~round(.x/100, 3))


cd_df <- pres_kos %>%
  select(cd, incumbent, inc_party = party, pct_trump = trump, pct_romney = romney, pct_mccain = mccain) %>%
  mutate(cd = str_replace(cd, "-AL", "-01")) %>%
  left_join(select(placenames, cd, descrip = geographic_description, place = largest_place))


write_rds(cd_df, "data/input/by-cd_info.Rds")

