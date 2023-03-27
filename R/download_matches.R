download_matches <- function(csv_path) {
  download.file("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv",
                csv_path)
  return(csv_path)
}
