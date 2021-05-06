library(tidyverse)
library(hrbrthemes)
library(ragg)

vb_matches <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

match_1 <- vb_matches

w_p1 <- match_1 %>%
  select(circuit:match_num, w_player1, contains("w_p1")) %>%
  rename(name = w_player1) %>%
  rename_at(vars(contains("w_p1_tot_")), ~str_replace(., "w_p1_tot_", "")) %>%
  rename(cob = w_p1_country) %>%
  rename_at(vars(contains("w_p1_")), ~str_replace(., "w_p1_", "")) %>%
  mutate(win = TRUE, player = 1)

w_p2 <- match_1 %>%
  select(circuit:match_num, w_player2, contains("w_p2")) %>%
  rename(name = w_player2) %>%
  rename_at(vars(contains("w_p2_tot_")), ~str_replace(., "w_p2_tot_", "")) %>%
  rename(cob = w_p2_country) %>%
  rename_at(vars(contains("w_p2_")), ~str_replace(., "w_p2_", "")) %>%
  mutate(win = TRUE, player = 2)

l_p1 <- match_1 %>%
  select(circuit:match_num, l_player1, contains("l_p1")) %>%
  rename(name = l_player1) %>%
  rename_at(vars(contains("l_p1_tot_")), ~str_replace(., "l_p1_tot_", "")) %>%
  rename(cob = l_p1_country) %>%
  rename_at(vars(contains("l_p1_")), ~str_replace(., "l_p1_", "")) %>%
  mutate(win = FALSE, player = 1)

l_p2 <- match_1 %>%
  select(circuit:match_num, l_player2, contains("l_p2")) %>%
  rename(name = l_player2) %>%
  rename_at(vars(contains("l_p2_tot_")), ~str_replace(., "l_p2_tot_", "")) %>%
  rename(cob = l_p2_country) %>%
  rename_at(vars(contains("l_p2_")), ~str_replace(., "l_p2_", "")) %>%
  mutate(win = FALSE, player = 2)

X <- bind_rows(w_p1, w_p2, l_p1, l_p2) %>%
  pivot_longer(attacks:digs, names_to = "stat")

agg_png(here::here("beach-volleyball.png"), res = 300, height = 8, width = 7.43, units = "in")

p <- X %>%
  filter(name == "Reid Priddy") %>%
  filter(date > "2018-01-01") %>%
  ggplot(aes(date, value, colour = stat)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~stat, scales = "free", ncol = 2) +
    theme_ipsum_pub() +
    theme(legend.position = "none") +
    labs(
      title = "Reid Priddy's AVP Statistics Trend",
      subtitle = "Examine Reid Priddy's match stat progression over time."
    )

print(p)

dev.off()

