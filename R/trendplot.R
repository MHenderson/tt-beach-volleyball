trendplot <- function(X) {
  ggplot(X, aes(date, value, colour = stat)) +
    geom_point(size = .5) +
    geom_smooth(size = .5, alpha = .1) +
    facet_wrap(~stat, scales = "free", ncol = 2) +
    labs(
      title = "Reid Priddy's AVP Statistics Trend",
      subtitle = "Source: bigtimestats.blog | Graphic: Matthew Henderson"
    ) +
    theme_mjh()
}
