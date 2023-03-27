save_plot <- function(plot_object, plot_path) {
  agg_png(plot_path, res = 300, height = 8, width = 7.43, units = "in")
  print(plot_object)
  dev.off()
}
