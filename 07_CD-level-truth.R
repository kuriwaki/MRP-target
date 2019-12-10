library(tidyverse)

vf <- read_csv("data/input/L2/vfile-for-shiro.csv") %>%
  rename(st = vfile.state) %>%
  mutate(cd = str_c(st, "-", str_pad(congress.district, width = 2, pad = "0"))) %>%
  select(st, cd, everything()) %>%
  select(-congress.district)


vf_cd <- vf %>%
  filter(!is.na(cd)) %>%
  group_by(cd) %>%
  summarize(n_white = sum(n*(race == "European"), na.rm = TRUE),
            n_R = sum(n*(party == "Republican"), na.rm = TRUE),
            n_D = sum(n*(party == "Democratic"), na.rm = TRUE))

ggplot(vf_cd, aes(x = n_D)) +
  geom_histogram(color = "white")

vf_cd %>%
  arrange(n_D)
