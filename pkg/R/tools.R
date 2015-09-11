#' Open Biol-297 website in the Viewer pane
#'
#' @param url A string containing the URL to open.  Defaults to Biol-297 homepage.
#'
#' @return There is no return value.
#' @export
#'
view <- function(url = "http://rna.wlu.edu/bio297") {
  rstudio::viewer(url)
}