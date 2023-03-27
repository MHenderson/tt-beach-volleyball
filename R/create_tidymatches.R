create_tidymatches <- function(X) {

  w_p1 <- X %>%
    select(circuit:match_num, w_player1, contains("w_p1")) %>%
    rename(name = w_player1) %>%
    rename_at(vars(contains("w_p1_tot_")), ~str_replace(., "w_p1_tot_", "")) %>%
    rename(cob = w_p1_country) %>%
    rename_at(vars(contains("w_p1_")), ~str_replace(., "w_p1_", "")) %>%
    mutate(win = TRUE, player = 1)

  w_p2 <- X %>%
    select(circuit:match_num, w_player2, contains("w_p2")) %>%
    rename(name = w_player2) %>%
    rename_at(vars(contains("w_p2_tot_")), ~str_replace(., "w_p2_tot_", "")) %>%
    rename(cob = w_p2_country) %>%
    rename_at(vars(contains("w_p2_")), ~str_replace(., "w_p2_", "")) %>%
    mutate(win = TRUE, player = 2)

  l_p1 <- X %>%
    select(circuit:match_num, l_player1, contains("l_p1")) %>%
    rename(name = l_player1) %>%
    rename_at(vars(contains("l_p1_tot_")), ~str_replace(., "l_p1_tot_", "")) %>%
    rename(cob = l_p1_country) %>%
    rename_at(vars(contains("l_p1_")), ~str_replace(., "l_p1_", "")) %>%
    mutate(win = FALSE, player = 1)

  l_p2 <- X %>%
    select(circuit:match_num, l_player2, contains("l_p2")) %>%
    rename(name = l_player2) %>%
    rename_at(vars(contains("l_p2_tot_")), ~str_replace(., "l_p2_tot_", "")) %>%
    rename(cob = l_p2_country) %>%
    rename_at(vars(contains("l_p2_")), ~str_replace(., "l_p2_", "")) %>%
    mutate(win = FALSE, player = 2)

  bind_rows(w_p1, w_p2, l_p1, l_p2) %>%
    pivot_longer(attacks:digs, names_to = "stat")

}
