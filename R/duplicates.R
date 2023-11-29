#' Duplicate Data Quality Checks
#'
#' Identifies duplicate entries in the dataset based on a specified ID variable.
#' It provides a dataset containing only the duplicate entries for further analysis.
#'
#' @param input Shiny server input object containing the variable selections.
#' @param hfc_dataset Reactive expression providing the dataset for analysis.
#' @importFrom shiny reactive bindEvent req renderDT
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by filter ungroup select all_of
#' @importFrom rlang sym
#' @export
duplicate_id_var <- shiny::reactive({
  req(input$duplicate_id_select_var)
  input$duplicate_id_select_var
})

duplicate_extra_vars <- shiny::reactive({
  req(input$duplicate_extra_vars_select_var)
  input$duplicate_extra_vars_select_var
})

duplicate_dataset <- shiny::reactive({
  req(duplicate_id_var(), duplicate_extra_vars(), hfc_dataset())

  hfc_dataset() %>%
    group_by(!!sym(duplicate_id_var())) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    select(all_of(c(duplicate_id_var(), duplicate_extra_vars())))
}) %>%
  bindEvent(input$run_hfcs)

#' Render Duplicate Data Table
#'
#' Renders a DataTable output of the duplicate entries in the dataset for the UI.
#'
#' @param output Shiny server output object.
#' @param duplicate_dataset Reactive expression of the duplicate dataset.
#' @export
output$duplicate_table <- renderDT(
  duplicate_dataset(), fillContainer = TRUE
)

