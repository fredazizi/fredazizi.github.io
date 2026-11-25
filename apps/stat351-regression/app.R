# STAT 351: Simple Linear Regression, browser-based through Shinylive/webR.
library(shiny)
library(readxl)
source("regression.R", local = TRUE)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body {background:#fbfbfa;color:#232323}
    .container-fluid {max-width:1200px;padding-top:16px}
    h2 {border-bottom:4px solid #ed9628;padding-bottom:12px}
    .well, pre {background:white;border:1px solid #ddd}
    pre {padding:18px;white-space:pre-wrap}
    .btn-primary {background:#bb6f1b;border-color:#bb6f1b}
    .table-wrap {overflow-x:auto}
    .shiny-output-error-validation {color:#9c311c}
  "))),
  titlePanel("STAT 351 | Simple Linear Regression"),
  p("Upload a dataset, choose x and y, select interval levels, then click Run regression."),
  fluidRow(
    column(4, wellPanel(
      h4("1. Upload a dataset"),
      fileInput("file", "CSV or Excel (.xlsx)", accept = c(".csv", ".xlsx")),
      uiOutput("sheet_ui"),
      tags$hr(),
      h4("2. Choose numerical columns"),
      selectInput("x", "x (explanatory variable)", choices = NULL),
      selectInput("y", "y (response variable)", choices = NULL),
      tags$hr(),
      h4("3. Set intervals"),
      numericInput("ci_level", "Confidence level for coefficient and mean CIs",
                   value = 0.95, min = 0.01, max = 0.999, step = 0.01),
      numericInput("pi_level", "Confidence level for prediction intervals",
                   value = 0.95, min = 0.01, max = 0.999, step = 0.01),
      textAreaInput("new_x", "x values for intervals (optional)", value = "",
                    placeholder = "For example: 2000, 2500", rows = 3),
      helpText("Separate values with commas or spaces. Leave blank to use each distinct observed x value."),
      actionButton("run", "Run regression", class = "btn-primary")
    )),
    column(8,
      h4("Data preview"),
      div(class = "table-wrap", tableOutput("preview")),
      textOutput("data_status"),
      tags$hr(),
      textOutput("analysis_status"),
      uiOutput("results")
    )
  )
)

server <- function(input, output, session) {
  extension <- reactive({
    req(input$file)
    tolower(tools::file_ext(input$file$name))
  })
  sheets <- reactive({
    req(input$file)
    if (extension() != "xlsx") return(character())
    tryCatch(readxl::excel_sheets(input$file$datapath), error = function(e) {
      validate(need(FALSE, paste("Could not read workbook:", conditionMessage(e))))
    })
  })
  output$sheet_ui <- renderUI({
    if (length(sheets())) selectInput("sheet", "Worksheet", choices = sheets())
  })
  dataset <- reactive({
    req(input$file)
    sheet <- NULL
    if (extension() == "xlsx") {
      req(input$sheet)
      validate(need(input$sheet %in% sheets(), "Choose a worksheet."))
      sheet <- input$sheet
    }
    tryCatch(read_regression_data(input$file$datapath, input$file$name, sheet),
             error = function(e) {
               validate(need(FALSE, paste("Could not read file:", conditionMessage(e))))
             })
  })
  observeEvent(dataset(), {
    df <- dataset()
    num <- names(df)[vapply(df, is.numeric, logical(1))]
    updateSelectInput(session, "x", choices = num,
                      selected = if (length(num)) num[1] else character())
    updateSelectInput(session, "y", choices = num,
                      selected = if (length(num) >= 2) num[2] else character())
  })
  output$preview <- renderTable(head(dataset(), 6),
                                rownames = FALSE, striped = TRUE, spacing = "xs")
  output$data_status <- renderText({
    df <- dataset()
    num <- sum(vapply(df, is.numeric, logical(1)))
    paste0(nrow(df), " rows; ", ncol(df), " columns; ", num, " numeric columns.",
           if (num < 2L) " At least two numeric columns are required." else "")
  })

  result <- reactiveVal(NULL)
  # Changing settings clears the previous fit so its intervals cannot be
  # mistaken for output from the new choices. Calculations run only on Run.
  observeEvent(list(input$file, input$sheet, input$x, input$y,
                    input$ci_level, input$pi_level, input$new_x), {
    result(NULL)
  }, ignoreNULL = FALSE, priority = 100)
  observeEvent(input$run, {
    df <- dataset()
    result(tryCatch(fit_regression(df, input$x, input$y, input$ci_level,
                                   input$pi_level, input$new_x),
                    error = function(e) e))
  })
  analysis <- reactive({
    ans <- result()
    req(!is.null(ans))
    validate(need(!inherits(ans, "error"),
                  if (inherits(ans, "error")) conditionMessage(ans) else ""))
    ans
  })
  output$analysis_status <- renderText({
    ans <- analysis()
    sprintf("Rows used: %d; rows omitted (missing or non-finite x/y): %d.", ans$n, ans$omitted)
  })
  output$results <- renderUI({
    ans <- analysis()
    tabsetPanel(
      tabPanel("R output", h4("Linear model"), verbatimTextOutput("lm_output")),
      tabPanel("Coefficient CIs", h4(sprintf("%g%% coefficient confidence intervals", 100 * ans$ci_level)),
               verbatimTextOutput("coefficient_output")),
      tabPanel("Mean CIs and prediction intervals",
               h4(sprintf("Mean CI: %g%%    Prediction interval: %g%%", 100 * ans$ci_level, 100 * ans$pi_level)),
               div(class = "table-wrap", tableOutput("interval_table")),
               textOutput("interval_count"), downloadButton("download_intervals", "Download intervals (CSV)")),
      tabPanel("Fitted line and intervals", plotOutput("fit_plot", height = "520px")),
      tabPanel("Diagnostics", plotOutput("diagnostics", height = "760px"))
    )
  })
  output$lm_output <- renderPrint({
    ans <- analysis()
    model_summary <- summary(ans$model)
    # Show the actual selected-variable formula rather than a helper variable.
    model_summary$call <- call("lm", formula = formula(ans$model), data = quote(data))
    print(model_summary)
  })
  output$coefficient_output <- renderPrint({ print(analysis()$coefficient_ci) })
  output$interval_table <- renderTable({
    head(analysis()$intervals, 100)
  }, digits = 4, rownames = FALSE, striped = TRUE, spacing = "s")
  output$interval_count <- renderText({
    n <- nrow(analysis()$intervals)
    if (n > 100) sprintf("Showing the first 100 of %d rows. Download the CSV for all rows.", n)
  })
  output$download_intervals <- downloadHandler(
    filename = function() "regression-intervals.csv",
    content = function(file) write.csv(analysis()$intervals, file, row.names = FALSE)
  )
  output$fit_plot <- renderPlot({
    ans <- analysis()
    x <- ans$data[[ans$x_name]]; y <- ans$data[[ans$y_name]]
    gx <- ans$grid[[ans$x_name]]
    plot(x, y, xlab = ans$x_name, ylab = ans$y_name, pch = 19,
         col = "#254d70", ylim = range(y, ans$grid_ci, ans$grid_pi, finite = TRUE))
    matlines(gx, ans$grid_pi[, c("lwr", "upr")], col = "#7d8793", lty = 3, lwd = 2)
    matlines(gx, ans$grid_ci[, c("lwr", "upr")], col = "#bb6f1b", lty = 2, lwd = 2)
    lines(gx, ans$grid_ci[, "fit"], col = "#142a43", lwd = 2)
    legend("topleft", c("Fitted line", sprintf("%g%% mean CI", 100 * ans$ci_level),
                       sprintf("%g%% prediction interval", 100 * ans$pi_level)),
           col = c("#142a43", "#bb6f1b", "#7d8793"), lty = c(1, 2, 3), lwd = 2, bty = "n")
  }, res = 96)
  output$diagnostics <- renderPlot({
    ans <- analysis()
    old <- par(mfrow = c(2, 2), mar = c(5, 4.5, 3, 1.5), oma = c(0, 0, 2, 0))
    on.exit(par(old))
    for (which in c(1, 2, 3, 5)) {
      tryCatch(plot(ans$model, which = which, ask = FALSE), error = function(e) {
        plot.new(); text(0.5, 0.5, paste("Plot unavailable:", conditionMessage(e)), cex = 0.8)
      })
    }
  }, res = 96)
}

shinyApp(ui, server)
