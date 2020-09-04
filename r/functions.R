load_rda <- function(f) {
  load(f)
  tweets_dl
}

render_site <- function() {
  file.rename(from = "graph.html", to = "docs/graph.html")
  fs::dir_delete("graph_files")
  rmarkdown::render_site("docs")
}