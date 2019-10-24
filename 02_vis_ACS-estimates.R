library(tidyverse)
library(lemon)
library(ggthemes)


pop_frac <- read_rds("data/output/by-CD_ACS_gender-age-education.Rds")

pop_frac %>%
  filter(gender == "Female",
         age == "65 years and over",
         educ == "Graduate or professional degree") %>%
  ggplot(aes(x = year, y  = frac_dist, group = cdid)) +
  geom_vline(xintercept = 2011.5,  color = "indianred") +
  annotate("text", x = 2011.8, y = Inf,
           label = "Redistricting /\nlabel-switching",
           color = "indianred",
           vjust = 1,
           hjust = 0) +
  geom_pointline(alpha = 0.1, pch = 16) +
  scale_y_continuous(limits = c(0, 0.03)) +
  scale_x_continuous(breaks = 2010:2017) +
  labs(title = "Estimate of proportion of women 65+ with a post-grad degree",
       y = "Estiamted proportion of CD",
       x = "",
       caption = "ACS estimates. Each year is ACS' estiamte from the 5-year average.
       Lines connect Congressional District labels (not necessary the geography).") +
  theme_clean() +
  coord_capped_cart(bottom = "both",
                    left = "both")

ggsave("figures/by-CD_ACS_women-65plus-postgrad.pdf", w = 7, h = 4)
