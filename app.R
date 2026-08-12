library(bslib)
library(tidyverse)
library(reactable)
library(shiny)

# Flagler color palette
flagler_palette <- c(
  "#A90533",  # Crimson
  "#FFB500",  # Gold
  "#DAD35E",  # Light gold
  "#1DADAF",  # Teal
  "#0C4050"   # Dark teal
)

flagler_primary   <- flagler_palette[1]
flagler_gold      <- flagler_palette[2]
flagler_lightgold <- flagler_palette[3]
flagler_teal      <- flagler_palette[4]
flagler_darkteal  <- flagler_palette[5]

ui <- page_sidebar(
  title = "Sample Proportion Distribution",

  theme = bs_theme(
    primary = flagler_primary,
    secondary = flagler_darkteal,
    info = flagler_teal,
    warning = flagler_gold,
    "navbar-bg" = flagler_primary
  ),

  tags$head(
    tags$style(
      HTML(
        sprintf(
          "
          /* Navbar */
          .navbar {
            background-color: %1$s !important;
            color: white !important;
          }

          .navbar .navbar-brand {
            color: white !important;
            font-weight: 600;
          }

          /* Sidebar labels */
          .sidebar .control-label,
          .sidebar .form-label {
            color: black;
            font-weight: 600;
          }

          /* Sidebar divider */
          .sidebar hr {
            border-top: 1px solid black;
          }

          /* Primary button */
          .btn-primary {
            background-color: %1$s !important;
            border-color: %1$s !important;
            color: white !important;
          }

          .btn-primary:hover,
          .btn-primary:focus {
            background-color: %5$s !important;
            border-color: %5$s !important;
          }

          /* Form focus */
          .form-control:focus,
          .form-select:focus {
            border-color: %4$s !important;
            box-shadow: 0 0 0 0.2rem rgba(29, 173, 175, 0.20) !important;
          }

          /* Checkbox */
          .form-check-input:checked {
            background-color: %1$s !important;
            border-color: %1$s !important;
          }

          /* Reactable header */
          .rt-thead .rt-th {
            background-color: %5$s !important;
            color: white !important;
            font-weight: 600;
          }

          /* Reactable striped rows */
          .rt-tbody .rt-tr:nth-child(even) {
            background-color: rgba(218, 211, 94, 0.10);
          }

          /* Reactable row hover */
          .rt-tbody .rt-tr:hover {
            background-color: rgba(29, 173, 175, 0.10);
          }
          ",
          flagler_primary,
          flagler_gold,
          flagler_lightgold,
          flagler_teal,
          flagler_darkteal
        )
      )
    )
  ),

  sidebar = sidebar(
    textInput(
      "group1_name",
      "Group 1:",
      value = "Group 1",
      placeholder = "e.g., Success"
    ),

    textInput(
      "group2_name",
      "Group 2:",
      value = "Group 2",
      placeholder = "e.g., Failure"
    ),

    hr(),

    numericInput(
      "pop_prop",
      "Population Proportion (Group 1):",
      value = 0.4,
      min = 0.1,
      max = 0.9,
      step = 0.1
    ),

    numericInput(
      "sample_size",
      "Sample Size (n):",
      value = 30,
      min = 10,
      max = 100,
      step = 10
    ),

    numericInput(
      "num_samples",
      "Number of Samples:",
      value = 500,
      min = 100,
      max = 1000,
      step = 100
    ),

    actionButton(
      "simulate",
      "Generate Samples",
      class = "btn-primary",
      width = "100%"
    ),

    hr(),

    checkboxInput(
      "show_normal",
      "Show Normal Curve",
      value = TRUE
    )
  ),

  # Main plot area with overlaid cards
  div(
    style = "position: relative; height: 500px;",

    card(
      card_header(
        "Distribution of Sample Proportions",
        style = paste0(
          "background-color: ", flagler_primary,
          "; color: white;"
        )
      ),

      plotOutput(
        "distribution_plot",
        height = "400px"
      ),

      style = "height: 500px;"
    ),

    div(
      style = paste0(
        "position: absolute;",
        " top: 60px;",
        " right: 60px;",
        " z-index: 10;",
        " width: 200px;"
      ),

      card(
        div(
          style = paste0(
            "text-align: center;",
            " padding: 8px;",
            " border-bottom: 1px solid black;"
          ),

          h5(
            textOutput(
              "pop_prop_display",
              inline = TRUE
            ),
            style = paste0(
              "color: ", flagler_primary,
              "; margin: 0;"
            )
          ),

          p(
            "Population Proportion",
            style = paste0(
              "margin: 2px 0 0 0;",
              " font-size: 11px;",
              " color: black;"
            )
          )
        ),

        div(
          style = "text-align: center; padding: 8px;",

          h5(
            textOutput(
              "sample_mean_display",
              inline = TRUE
            ),
            style = paste0(
              "color: ", flagler_teal,
              "; margin: 0;"
            )
          ),

          p(
            "Mean Sample Proportion",
            style = paste0(
              "margin: 2px 0 0 0;",
              " font-size: 11px;",
              " color: black;"
            )
          )
        ),

        style = paste0(
          "background-color: rgba(255,255,255,0.95);",
          " border: 2px solid black;",
          " border-radius: 8px;",
          " box-shadow: 0 2px 4px rgba(0,0,0,0.1);"
        )
      )
    )
  ),

  # Sample outcomes table
  card(
    card_header(
      "Sample Outcomes",
      style = paste0(
        "background-color: ", flagler_primary,
        "; color: white;"
      )
    ),

    reactableOutput("samples_table")
  )
)

server <- function(input, output, session) {

  # Store simulation results
  sample_props    <- reactiveVal(NULL)
  sample_counts   <- reactiveVal(NULL)
  sample_outcomes <- reactiveVal(NULL)

  # Update population proportion label dynamically
  observe({
    new_label <- paste0(
      "Population Proportion (",
      input$group1_name,
      "):"
    )

    updateNumericInput(
      session,
      "pop_prop",
      label = new_label
    )
  })

  # Helper to generate outcomes as strings using first letters of group names
  make_outcomes_strings <- function(
    num_samples,
    sample_size,
    p,
    g1,
    g2
  ) {

    g1_letter <- toupper(substr(g1, 1, 1))
    g2_letter <- toupper(substr(g2, 1, 1))

    lapply(
      seq_len(num_samples),
      function(i) {

        x <- rbinom(
          sample_size,
          1,
          p
        )

        paste(
          ifelse(
            x == 1,
            g1_letter,
            g2_letter
          ),
          collapse = ", "
        )
      }
    )
  }

  # Generate initial data
  observe({
    if (is.null(sample_props())) {
      simulation <- tibble(
        count = rbinom(input$num_samples, input$sample_size, input$pop_prop)
      ) |> 
      mutate(proportion = count / input$sample_size)

      sample_counts(simulation$count)
      sample_props(simulation$proportion)

      sample_outcomes(
        make_outcomes_strings(
          input$num_samples,
          input$sample_size,
          input$pop_prop,
          input$group1_name,
          input$group2_name
        )
      )
    }
  })

  # Run simulation when button is clicked
  observeEvent(input$simulate, {
    simulation <- tibble(
      count = rbinom(input$num_samples, input$sample_size, input$pop_prop)
    ) |> 
    mutate(
      proportion = count / input$sample_size
    )

    sample_counts(simulation$count)
    sample_props(simulation$proportion)

    sample_outcomes(
      make_outcomes_strings(
        input$num_samples,
        input$sample_size,
        input$pop_prop,
        input$group1_name,
        input$group2_name
      )
    )
  })

  # Display values for the info box
  output$pop_prop_display <- renderText({
    round(input$pop_prop, 3)
  })

  output$sample_mean_display <- renderText({
    req(sample_props())
    round(mean(sample_props()), 3)
  })

# Create the main plot
output$distribution_plot <- renderPlot({

  req(sample_props())

  bin_width <- 1 / input$sample_size

  hist_data <- hist(
    sample_props(),
    breaks = seq(
      -bin_width / 2,
      1 + bin_width / 2,
      by = bin_width
    ),
    plot = FALSE
  )

  plot_data <- tibble(
    xmin = head(hist_data$breaks, -1),
    xmax = tail(hist_data$breaks, -1),
    frequency = hist_data$counts
  )

  max_count <- max(plot_data$frequency)

  p <- ggplot(
    tibble(props = sample_props()),
    aes(x = props)
  ) +
    geom_histogram(
      binwidth = bin_width,
      fill = flagler_teal,
      color = flagler_darkteal,
      boundary = -bin_width / 2
    ) +
    scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.1)
    ) +
    labs(
      x = paste(
        "Sample Proportion of",
        input$group1_name
      ),
      y = "Frequency"
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(
        size = 11,
        color = "black"
      ),
      axis.title = element_text(
        size = 12,
        color = "black"
      )
    ) +
    geom_vline(
      xintercept = input$pop_prop,
      color = flagler_primary,
      linetype = "dashed",
      linewidth = 1
    ) +
    geom_vline(
      xintercept = mean(sample_props()),
      color = flagler_darkteal,
      linetype = "dashed",
      linewidth = 1
    ) +
    annotate(
      "polygon",
      x = c(
        input$pop_prop - 0.01,
        input$pop_prop + 0.01,
        input$pop_prop
      ),
      y = c(
        -max_count * 0.08,
        -max_count * 0.08,
        -max_count * 0.02
      ),
      fill = flagler_primary,
      color = flagler_primary
    ) +
    annotate(
      "polygon",
      x = c(
        mean(sample_props()) - 0.01,
        mean(sample_props()) + 0.01,
        mean(sample_props())
      ),
      y = c(
        -max_count * 0.08,
        -max_count * 0.08,
        -max_count * 0.02
      ),
      fill = flagler_darkteal,
      color = flagler_darkteal
    )

  if (input$show_normal) {

    theoretical_mean <- input$pop_prop

    theoretical_sd <- sqrt(
      input$pop_prop *
        (1 - input$pop_prop) /
        input$sample_size
    )

    x_vals <- seq(
      max(
        0,
        theoretical_mean - 4 * theoretical_sd
      ),
      min(
        1,
        theoretical_mean + 4 * theoretical_sd
      ),
      length.out = 1000
    )

    normal_curve <- tibble(
      x = x_vals
    ) |>
      mutate(
        y = dnorm(
          x,
          mean = theoretical_mean,
          sd = theoretical_sd
        ) *
          length(sample_props()) *
          bin_width
      )

    p <- p +
      geom_line(
        data = normal_curve,
        aes(
          x = x,
          y = y
        ),
        color = "black",
        linewidth = 1.5,
        alpha = 0.9
      )
  }

  p
})


# Create the samples table using reactable
output$samples_table <- renderReactable({

  req(
    sample_props(),
    sample_counts(),
    sample_outcomes()
  )

  table_data <- tibble(
    Sample_Num = seq_along(sample_props()),
    Outcomes = unlist(sample_outcomes()),
    Group1_Count = sample_counts(),
    Sample_Proportion = sample_props()
  ) |>
    mutate(
      Group2_Count = input$sample_size - Group1_Count
    ) |>
    dplyr::select(
      Sample_Num,
      Outcomes,
      Group1_Count,
      Group2_Count,
      Sample_Proportion
    )

  col_names <- list(
    Sample_Num = "Sample #",
    Outcomes = "Sample Outcome",
    Group1_Count = paste(
      input$group1_name,
      "Count"
    ),
    Group2_Count = paste(
      input$group2_name,
      "Count"
    ),
    Sample_Proportion = paste(
      "Proportion of",
      input$group1_name
    )
  )

  reactable(
    table_data,

    columns = list(
      Sample_Num = colDef(
        name = col_names$Sample_Num,
        align = "center",
        width = 90
      ),

      Outcomes = colDef(
        name = col_names$Outcomes,
        align = "left",
        minWidth = 280,
        style = list(
          fontSize = "11px",
          fontFamily = "monospace",
          whiteSpace = "nowrap"
        )
      ),

      Group1_Count = colDef(
        name = col_names$Group1_Count,
        align = "center",
        width = 120
      ),

      Group2_Count = colDef(
        name = col_names$Group2_Count,
        align = "center",
        width = 120
      ),

      Sample_Proportion = colDef(
        name = col_names$Sample_Proportion,
        align = "center",
        width = 150,
        format = colFormat(
          digits = 4
        )
      )
    ),

    defaultPageSize = 100,
    height = 320,
    striped = TRUE,
    bordered = TRUE,
    searchable = FALSE
  )
})
}

shinyApp(
  ui = ui,
  server = server
)