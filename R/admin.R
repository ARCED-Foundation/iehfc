#' Administrative Unit-Level Data Quality Checks
#'
#' This set of reactive expressions computes various summaries of data quality checks
#' at the administrative unit level. It includes total submissions, complete submissions,
#' and daily submissions, with visualizations if date information is available.
#'
#' @param input The Shiny server input object containing the selections from UI.
#' @param hfc_dataset A reactive expression providing the dataset for analysis.
#' @importFrom shiny reactive bindEvent req renderDT renderPlotly
#' @import magrittr
#' @importFrom dplyr left_join group_by summarize ungroup across any_of mutate case_when
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate parse_date_time as.Date
#' @importFrom rlang sym
#' @importFrom ggplot2 ggplot geom_line labs theme_minimal theme
#' @importFrom plotly ggplotly highlight_key highlight
#' @importFrom tibble tibble
#' @export
  admin_var <- shiny::reactive({
      input$admin_var_select_var
  })

  admin_super_vars <- shiny::reactive({
      input$admin_super_vars_select_var
  })

  admin_date_var <- shiny::reactive({
      input$admin_date_var_select_var
  })

  admin_complete_var <- shiny::reactive({
      input$admin_complete_var_select_var
  })

  admin_total_subs_dataset <- shiny::reactive({
      hfc_dataset() %>%
          group_by(
              across(any_of(admin_super_vars())), !!sym(admin_var())) %>%
          summarize(
              num_submissions = n()) %>%
          ungroup()
  }) %>%
  bindEvent(input$run_hfcs)

  admin_complete_subs_dataset <- shiny::reactive({
      if(admin_complete_var() != "") {
          hfc_dataset() %>%
              group_by(
                  across(any_of(admin_super_vars())), !!sym(admin_var())
              ) %>%
              summarize(
                  num_complete_submissions = sum(
                      across(admin_complete_var(), ~ .x == 1 | .x == "Yes"), na.rm = TRUE
                  )
              ) %>%
              ungroup()
      } else {
          tibble() %>% # So that it merges without error, but does not add information
              mutate(
                  !!admin_var() := case_when(
                      class(hfc_dataset()[[admin_var()]]) == "character" ~ list(NA_character_),
                      class(hfc_dataset()[[admin_var()]]) == "integer"   ~ list(NA_integer_),
                      class(hfc_dataset()[[admin_var()]]) == "numeric"   ~ list(NA_real_),
                      TRUE                                               ~ list(NA)
                  ) %>%
                  unlist()
              )
      }
  }) %>%
  bindEvent(input$run_hfcs)

  admin_daily_subs_dataset <- shiny::reactive({
      if(admin_date_var() != "") {
          hfc_dataset() %>%
              # Attempt to format date. This may need to be added to depending on reasonable formats to expect
              mutate(
                  date_var_formatted = lubridate::parse_date_time(
                      !!sym(admin_date_var()), c("Y-m-d", "m/d/Y", "d/m/Y")
                  ) %>%
                  as.Date()
              ) %>%
              group_by(
                  across(any_of(admin_super_vars())), !!sym(admin_var()), date_var_formatted
              ) %>%
              summarize(
                  num_submissions = n()
              ) %>%
              ungroup() %>%
              pivot_wider(
                  names_from  = date_var_formatted,
                  values_from = num_submissions
              )
      } else {
          tibble() %>% # So that it merges without error, but does not add information
              mutate(
                  !!admin_var() := case_when(
                      class(hfc_dataset()[[admin_var()]]) == "character" ~ list(NA_character_),
                      class(hfc_dataset()[[admin_var()]]) == "integer"   ~ list(NA_integer_),
                      class(hfc_dataset()[[admin_var()]]) == "numeric"   ~ list(NA_real_),
                      TRUE                                               ~ list(NA)
                  ) %>%
                  unlist()
              )
      }
  }) %>%
  bindEvent(input$run_hfcs)

  admin_daily_subs_plot <- shiny::reactive({
      plot_data <- hfc_dataset() %>%
          # Attempt to format date. This may need to be added to depending on reasonable formats to expect
          mutate(
              date_var_formatted = lubridate::parse_date_time(
                  !!sym(admin_date_var()), c("Y-m-d", "m/d/Y", "d/m/Y")
              ) %>%
                  as.Date()
          ) %>%
          group_by(
              across(any_of(admin_super_vars())), !!sym(admin_var()), date_var_formatted
          ) %>%
          summarize(
              num_submissions = n()
          ) %>%
          ungroup() %>%
          group_by(
              across(any_of(admin_super_vars())), !!sym(admin_var())
          ) %>%
          mutate(
              cumul_submissions = cumsum(
                  ifelse(is.na(num_submissions), 0, num_submissions) # Need to do this because cumsum() doesn't have an 'na.rm' argument
              )
          ) %>%
          ungroup()

      # Set up highlighting individual admins

      admin_daily_subs_ggplot <- plot_data %>%
          mutate(
              !!admin_var() := factor(!!sym(admin_var()))
          ) %>%
          group_by(
              !!sym(admin_var())
          ) %>%
          highlight_key(
              as.formula(
                  paste0("~", admin_var())
              )
          ) %>%
          ggplot() +
          geom_line(
              aes(x = date_var_formatted, y = cumul_submissions, color = !!sym(admin_var()))
          ) +
          labs(
              x = "Date",
              y = "Cumulative # of Submissions"
          ) +
          theme_minimal() +
          theme(
              legend.position = "none"
          )

      admin_daily_subs_ggplotly <- ggplotly(admin_daily_subs_ggplot, tooltip = c("color", "y"))

      highlight(admin_daily_subs_ggplotly, on = "plotly_hover", off = "plotly_doubleclick")

  })

  admin_subs_dataset <- shiny::reactive({
      admin_total_subs_dataset() %>%
          left_join(admin_complete_subs_dataset()) %>%  # Works because is empty tibble if not "complete" variable is selected
          left_join(admin_daily_subs_dataset())
  }) %>%
  bindEvent(input$run_hfcs)

  output$admin_subs_table <- renderDT(
      admin_subs_dataset(), fillContainer = TRUE
  )

  output$admin_daily_subs_plot_rendered <- renderPlotly(
      admin_daily_subs_plot()
  )
