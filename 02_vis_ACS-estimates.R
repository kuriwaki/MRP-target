library(tidyverse)
library(lemon)
library(ggthemes)
library(scales)


pop_cd_frac <- read_rds("data/output/by-CD_ACS_gender-age-education.Rds")

pop_cd_frac %>%
  filter(gender == 2,
         age == 5,
         educ == 6) %>%
  ggplot(aes(x = year, y  = frac_dist, group = cdid)) +
  # geom_vline(xintercept = 2011.5,  color = "indianred") +
  # annotate("text", x = 2011.8, y = Inf,
  #          label = "Redistricting /\nlabel-switching",
  #          color = "indianred",
  #          vjust = 1,
  #          hjust = 0) +
  geom_pointline(alpha = 0.15, pch = 16) +
  scale_y_continuous(limits = c(0, 0.035), labels = percent) +
  scale_x_continuous(breaks = 2012:2017) +
  labs(title = "Proportion of women 65+ with a post-grad degree in CD",
       y = "ACS Estimate",
       x = "",
       caption = "ACS estimates. Each year is ACS 1yr estimate.
       Lines connect Congressional District labels") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  theme_clean() +
  coord_capped_cart(bottom = "both",
                    left = "both")

ggsave("figures/by-CD_ACS_women-65plus-postgrad.pdf", w = 6, h = 4)
