#' Launch the IEHFC Shiny Application
#'
#' @details This function starts the shiny application contained in the package.
#' @export
iehfc_app <- function() {
  # Assuming you moved the www directory to inst/app/www
  shiny::addResourcePath(prefix = "res", directoryPath = system.file("app/www", package = "iehfc"))

  global_path <- system.file("R", "global.R", package = "iehfc")
  ui_path <- system.file("R", "iehfc_ui.R", package = "iehfc")
  server_path <- system.file("R", "iehfc_server.R", package = "iehfc")

  # Source the files
  if (file.exists(global_path)) source(global_path)
  ui <- if (file.exists(ui_path)) source(ui_path, local = TRUE)$value
  server <- if (file.exists(server_path)) source(server_path, local = TRUE)$value

  # Start the app
  shinyApp(ui = ui, server = server)

  }


