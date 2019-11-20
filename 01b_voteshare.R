library(tidyverse)
library(glue)
library(readxl)


placenames <- read_excel("data/input/dailykos/by-cd_placenames.xlsx") %>%
  rename_all(~str_to_lower(str_replace_all(str_replace_all(.x, "%", "pct"), "(\\.+|\\s|-)", "_"))) %>%
  mutate_if(is.numeric, ~.x/100) %>%
  rename(cd = district)

pres_kos <- read_excel("data/input/dailykos/by-cd_pres.xlsx", skip = 1) %>%
  select(1:9) %>%
  rename_all(~str_to_lower(str_replace_all(.x, "\\.+", "_"))) %>%
  mutate(party = str_remove_all(party, "[\\(\\)]")) %>%
  mutate_if(is.numeric, ~round(.x/100, 3))


cd_df <- pres_kos %>%
  select(cd, incumbent, inc_party = party, pct_trump = trump, pct_romney = romney, pct_mccain = mccain) %>%
  left_join(select(placenames, cd, descrip = geographic_description, place = largest_place))


write_rds(cd_df, "data/input/by-cd_info.Rds")
write_csv(filter(cd_df, str_detect(cd, "TX")),
          "data/output/cces/sample-TX/by-cd_info.csv",
          na = "")

