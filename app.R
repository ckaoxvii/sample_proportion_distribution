library(shiny)
library(bslib)
library(ggplot2)
library(reactable)

ui <- page_sidebar(
  title = "Sample Proportion Distribution",
  theme = bs_theme(primary = "#A90533"),
  tags$head(
    tags$style(
      HTML(".navbar {
        background-color: #A90533 !important;
        color: white !important;
      }
      .navbar .navbar-brand {
        color: white !important;
      }")
    )
  ),
  sidebar = sidebar(
    textInput("group1_name", "Group 1:", 
              value = "Group 1", placeholder = "e.g., Success"),
    textInput("group2_name", "Group 2:", 
              value = "Group 2", placeholder = "e.g., Failure"),
    hr(),
    numericInput("pop_prop", "Population Proportion (Group 1):", 
                 value = 0.4, min = 0.1, max = 0.9, step = 0.1),
    numericInput("sample_size", "Sample Size (n):", 
                 value = 30, min = 10, max = 100, step = 10),
    numericInput("num_samples", "Number of Samples:", 
                 value = 500, min = 100, max = 1000, step = 100),
    actionButton("simulate", "Generate Samples", class = "btn-primary", width = "100%"),
    hr(),
    checkboxInput("show_normal", "Show Normal Curve", value = TRUE)
  ),
  
  # Main plot area with overlaid cards
  div(
    style = "position: relative; height: 500px;",
    card(
      card_header(
        "Distribution of Sample Proportions",
        style = "background-color: #A90533; color: white;"
      ),
      plotOutput("distribution_plot", height = "400px"),
      style = "height: 500px;"
    ),
    div(
      style = "position: absolute; top: 60px; right: 60px; z-index: 10; width: 200px;",
      card(
        div(
          style = "text-align: center; padding: 8px; border-bottom: 1px solid #eee;",
          h5(textOutput("pop_prop_display", inline = TRUE), style = "color: #A90533; margin: 0;"),
          p("Population Proportion", style = "margin: 2px 0 0 0; font-size: 11px;")
        ),
        div(
          style = "text-align: center; padding: 8px;",
          h5(textOutput("sample_mean_display", inline = TRUE), style = "color: #2D2D2D; margin: 0;"),
          p("Mean Sample Proportion", style = "margin: 2px 0 0 0; font-size: 11px;")
        ),
        style = "background-color: rgba(255, 255, 255, 0.95); border: 1px solid #ddd; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"
      )
    )
  ),
  
  # Sample outcomes table
  card(
    card_header(
      "Sample Outcomes",
      style = "background-color: #A90533; color: white;"
    ),
    reactableOutput("samples_table")
  )
)

server <- function(input, output, session) {
  # Store simulation results
  sample_props    <- reactiveVal(NULL)
  sample_counts   <- reactiveVal(NULL)
  sample_outcomes <- reactiveVal(NULL)   # NEW: per-sample sequences
  
  # Update population proportion label dynamically
  observe({
    new_label <- paste0("Population Proportion (", input$group1_name, "):")
    updateNumericInput(session, "pop_prop", label = new_label)
  })
  
  # Helper to generate outcomes as strings using first letters of group names
  make_outcomes_strings <- function(num_samples, sample_size, p, g1, g2) {
    g1_letter <- toupper(substr(g1, 1, 1))
    g2_letter <- toupper(substr(g2, 1, 1))
    lapply(seq_len(num_samples), function(i) {
      x <- rbinom(sample_size, 1, p)
      paste(ifelse(x == 1, g1_letter, g2_letter), collapse = ", ")
    })
  }
  
  # Generate initial data
  observe({
    if (is.null(sample_props())) {
      counts <- rbinom(input$num_samples, input$sample_size, input$pop_prop)
      props  <- counts / input$sample_size
      sample_props(props)
      sample_counts(counts)
      sample_outcomes(
        make_outcomes_strings(input$num_samples, input$sample_size, input$pop_prop,
                              input$group1_name, input$group2_name)
      )
    }
  })
  
  # Run simulation when button is clicked
  observeEvent(input$simulate, {
    counts <- rbinom(input$num_samples, input$sample_size, input$pop_prop)
    props  <- counts / input$sample_size
    sample_props(props)
    sample_counts(counts)
    sample_outcomes(
      make_outcomes_strings(input$num_samples, input$sample_size, input$pop_prop,
                            input$group1_name, input$group2_name)
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
    max_count <- max(hist(sample_props(), breaks = 20, plot = FALSE)$counts)
    
    p <- ggplot(data.frame(props = sample_props()), aes(x = props)) +
      geom_histogram(binwidth = 0.025, fill = "skyblue", color = "blue", alpha = 0.8) +
      scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
      labs(x = paste("Sample Proportion of", input$group1_name), y = "Frequency") +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        panel.background = element_rect(fill = "#f8f8f8", color = NA),
        plot.background = element_rect(fill = "#f8f8f8", color = NA),
        panel.grid.major = element_line(color = "#e8e8e8", size = 0.5),
        panel.grid.minor = element_line(color = "#e8e8e8", size = 0.3)
      ) +
      geom_segment(aes(x = input$pop_prop, xend = input$pop_prop, y = 0, yend = Inf), 
                   color = "#A90533", linetype = "dashed", size = 1) +
      geom_segment(aes(x = mean(sample_props()), xend = mean(sample_props()), y = 0, yend = Inf), 
                   color = "#2D2D2D", linetype = "dashed", size = 1) +
      annotate("polygon", 
               x = c(input$pop_prop - 0.01, input$pop_prop + 0.01, input$pop_prop),
               y = c(-max_count * 0.08, -max_count * 0.08, -max_count * 0.02),
               fill = "#A90533", color = "#A90533") +
      annotate("polygon", 
               x = c(mean(sample_props()) - 0.01, mean(sample_props()) + 0.01, mean(sample_props())),
               y = c(-max_count * 0.08, -max_count * 0.08, -max_count * 0.02),
               fill = "#2D2D2D", color = "#2D2D2D")
    
    if (input$show_normal) {
      theoretical_mean <- input$pop_prop
      theoretical_sd <- sqrt(input$pop_prop * (1 - input$pop_prop) / input$sample_size)
      num_points <- max(500, 1000)
      range_multiplier <- max(3, 4)
      x_min <- max(0, theoretical_mean - range_multiplier * theoretical_sd)
      x_max <- min(1, theoretical_mean + range_multiplier * theoretical_sd)
      x_vals <- seq(x_min, x_max, length.out = num_points)
      normal_curve <- data.frame(
        x = x_vals,
        y = dnorm(x_vals, mean = theoretical_mean, sd = theoretical_sd) * 
            length(sample_props()) * 1 / 20
      )
      p <- p + 
        geom_line(data = normal_curve, aes(x = x, y = y), 
                  color = "#A90533", size = 1.5, alpha = 0.8)
    }
    
    p
  })
  
  # Create the samples table using reactable (WITH Outcomes column)
  output$samples_table <- renderReactable({
    req(sample_props(), sample_counts(), sample_outcomes())
    
    table_data <- data.frame(
      "Sample_Num" = seq_along(sample_props()),
      "Outcomes" = unlist(sample_outcomes()),         # NEW: per-sample sequence
      "Group1_Count" = sample_counts(),
      "Group2_Count" = input$sample_size - sample_counts(),
      "Sample_Proportion" = sample_props(),
      stringsAsFactors = FALSE
    )
    
    col_names <- list(
      Sample_Num = "Sample #",
      Outcomes = "Sample Outcome",
      Group1_Count = paste(input$group1_name, "Count"),
      Group2_Count = paste(input$group2_name, "Count"),
      Sample_Proportion = paste("Proportion of", input$group1_name)
    )
    
    reactable(
      table_data,
      columns = list(
        Sample_Num = colDef(name = col_names$Sample_Num, align = "center", width = 90),
        Outcomes = colDef(
          name = col_names$Outcomes,
          align = "left",
          minWidth = 280,
          style = list(fontSize = "11px", fontFamily = "monospace", whiteSpace = "nowrap")
        ),
        Group1_Count = colDef(name = col_names$Group1_Count, align = "center", width = 120),
        Group2_Count = colDef(name = col_names$Group2_Count, align = "center", width = 120),
        Sample_Proportion = colDef(
          name = col_names$Sample_Proportion,
          align = "center",
          width = 150,
          format = colFormat(digits = 4)
        )
      ),
      defaultPageSize = 10,
      height = 320,
      striped = TRUE,
      bordered = TRUE,
      searchable = FALSE,
      pageSizeOptions = c(10, 25, 50)
    )
  })
}

shinyApp(ui = ui, server = server)