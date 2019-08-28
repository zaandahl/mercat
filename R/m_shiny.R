#' m_interactive
#' Launch MERCAT interactive in your browser.
#' 
#' @examples
#' \dontrun{
#' m_interactive()
#' }
#' @export
m_interactive <- function() {
  app_dir <- system.file("shiny-examples", "shinycat", package = "mercat")
  if (app_dir == "") {
    stop("Could not find example directory. Try re-installing `mercat`.",
         call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}