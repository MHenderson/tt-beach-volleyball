library(targets)

tar_option_set(
  packages = c("dplyr", "ggplot2", "readr", "stringr", "tibble", "tidyr"),
    format = "rds"
)

list(
  tar_target(
       name = vb_matches,
    command = read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv", guess_max = 76000)
  ),
  tar_target(
       name = tidymatches,
    command = {
      
      w_p1 <- vb_matches |>
	select(circuit:match_num, w_player1, contains("w_p1")) |>
	rename(name = w_player1) |>
	rename_at(vars(contains("w_p1_tot_")), ~str_replace(., "w_p1_tot_", "")) |>
	rename(cob = w_p1_country) |>
	rename_at(vars(contains("w_p1_")), ~str_replace(., "w_p1_", "")) |>
	mutate(win = TRUE, player = 1)

      w_p2 <- vb_matches |>
	select(circuit:match_num, w_player2, contains("w_p2")) |>
	rename(name = w_player2) |>
	rename_at(vars(contains("w_p2_tot_")), ~str_replace(., "w_p2_tot_", "")) |>
	rename(cob = w_p2_country) |>
	rename_at(vars(contains("w_p2_")), ~str_replace(., "w_p2_", "")) |>
	mutate(win = TRUE, player = 2)

      l_p1 <- vb_matches |>
	select(circuit:match_num, l_player1, contains("l_p1")) |>
	rename(name = l_player1) |>
	rename_at(vars(contains("l_p1_tot_")), ~str_replace(., "l_p1_tot_", "")) |>
	rename(cob = l_p1_country) |>
	rename_at(vars(contains("l_p1_")), ~str_replace(., "l_p1_", "")) |>
	mutate(win = FALSE, player = 1)

      l_p2 <- vb_matches |>
	select(circuit:match_num, l_player2, contains("l_p2")) |>
	rename(name = l_player2) |>
	rename_at(vars(contains("l_p2_tot_")), ~str_replace(., "l_p2_tot_", "")) |>
	rename(cob = l_p2_country) |>
	rename_at(vars(contains("l_p2_")), ~str_replace(., "l_p2_", "")) |>
	mutate(win = FALSE, player = 2)

      bind_rows(w_p1, w_p2, l_p1, l_p2) |>
	pivot_longer(attacks:digs, names_to = "stat")

    }
  ),
  tar_target(
       name = priddy_matches,
    command = tidymatches |> filter(name == "Reid Priddy") |> filter(date > "2018-01-01")      
  ),
  tar_target(
       name = priddy_plot,
    command = {

              base_size = 12
                  font1 = "Cardo"
      background_colour = "#eff2f7"
            text_colour = "#10192d"

      ggplot(priddy_matches, aes(date, value, colour = stat)) +
        geom_point(size = .5) +
        geom_smooth(size = .5, alpha = .1) +
        facet_wrap(~stat, scales = "free", ncol = 2) +
        labs(
             title = "Reid Priddy's AVP Statistics Trend",
          subtitle = "Source: bigtimestats.blog | Graphic: Matthew Henderson"
        ) +
	theme(
	  plot.margin       = margin(20, 10, 20, 10),
	  panel.background  = element_rect(fill = background_colour, colour = NA),
	  plot.background   = element_rect(fill = background_colour, colour = NA),
	  legend.background = element_rect(fill = background_colour),
	  strip.background  = element_rect(fill = background_colour),
	  plot.title        = element_text(colour = text_colour, size = 26, hjust = 1, family = font1, margin = margin(5, 0, 20, 0)),
	  plot.subtitle     = element_text(colour = text_colour, size = base_size, hjust = 1, family = font1, margin = margin(5, 0, 10, 0)),
	  plot.caption      = element_text(colour = text_colour, size = base_size, hjust = 0.5, family = font1),
	  legend.title      = element_text(colour = text_colour, size = base_size, hjust = 0.5, family = font1),
	  strip.text        = element_text(colour = text_colour, size = base_size, hjust = 0.5, family = font1, margin = margin(5, 0, 5, 0)),
	  legend.position   = "none",
	  axis.title.x      = element_blank(),
	  axis.title.y      = element_blank(),
	  axis.text.x       = element_blank(),
	  axis.text.y       = element_blank(),
	  axis.ticks.x      = element_blank(),
	  axis.ticks.y      = element_blank(),
	  panel.grid.major  = element_blank(),
	  panel.grid.minor  = element_blank()
	)
    }
  ),
  tar_target(
       name = save_priddy_plot,
     format = "file",
    command = {
      ggsave(
            plot = priddy_plot,
	filename = "plot/beach-volleyball.png",
          height = 4000,
           width = 3000,
           units = "px"
      )
      "plot/beach-volleyball.png"
    }
  )
)
