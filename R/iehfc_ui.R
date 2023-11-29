#' User Interface for IEHFC Shiny Application
#'
#' Defines the user interface for the IEHFC Shiny application, using a navbar layout
#' with multiple panels for introduction, data upload, check selection and setup, and outputs.
#' Additional information can be accessed through the "More" dropdown menu.
#'
#' @importFrom shiny fluidPage navbarPage tabPanel uiOutput navbarMenu tags
#' @importFrom bslib bs_theme bs_add_rules
#' @importFrom shinyjs useShinyjs
#' @importFrom sass sass_file
#' @export
iehfc_app_ui <- function() {
  shiny::fluidPage(
    useShinyjs(),
    shiny::navbarPage(
      title = "iehfc",
      # Initialize shinyjs

      shiny::tabPanel(
        "Introduction",
        # Assuming introduction_tab is a function that creates the UI for this tab
        introduction_tab()
      ),
      shiny::tabPanel(
        "Upload Data",
        id = "upload_tab",  # Give an ID for reference
        shiny::uiOutput("upload_tab")
      ),
      shiny::tabPanel(
        "Check Selection and Setup",
        id = "setup_tab",  # Give an ID for reference
        shiny::uiOutput("setup_tab")
      ),
      shiny::tabPanel(
        "Outputs",
        id = "output_tab",  # Give an ID for reference
        shiny::uiOutput("output_tab")
      ),
      shiny::navbarMenu("More",
                        shiny::tabPanel(shiny::tags$a("Guides", href = "https://www.github.com")),
                        shiny::tabPanel(shiny::tags$a("About", href = "https://www.github.com")),
                        shiny::tabPanel(shiny::tags$a("Github", href = "https://www.github.com"))
      ),
      theme = bs_theme() %>%
        bs_add_rules(
          sass::sass_file(system.file("www/custom.css", package = "iehfc"))
        )
    )
  )
}
