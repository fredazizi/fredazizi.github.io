# STAT 351 — Two-Sample and Paired t-Tests
# Browser-based via Shinylive/webR. The student sees unmodified t.test() output.
library(shiny)
library(readxl)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body {background:#fbfbfa;color:#232323}
    .container-fluid {max-width:1100px;padding-top:16px}
    h2 {border-bottom:4px solid #ed9628;padding-bottom:12px}
    .well, pre {background:white;border:1px solid #ddd}
    pre {padding:18px;white-space:pre-wrap}
    .btn-primary {background:#bb6f1b;border-color:#bb6f1b}
  "))),
  titlePanel("STAT 351 | Two population t-test Calculator"),
  p("Upload a spreadsheet, select columns and test settings, then read R's statistical output."),
  fluidRow(
    column(4, wellPanel(
      h4("1. Upload a dataset"),
      fileInput("file","Excel (.xlsx, .xls) or CSV",
                accept=c(".xlsx",".xls",".csv")),
      uiOutput("sheet_ui"),
      tags$hr(),
      h4("2. Choose two numerical columns"),
      selectInput("first","Column 1 (Group 1 / first measurement)",choices=NULL),
      selectInput("second","Column 2 (Group 2 / second measurement)",choices=NULL),
      helpText("The difference is always Column 1 minus Column 2."),
      tags$hr(),
      h4("3. Select the procedure"),
      radioButtons("test","Test type",choices=c(
        "Independent: unequal variances (Welch)"="welch",
        "Independent: equal variances (pooled)"="pooled",
        "Paired observations"="paired")),
      radioButtons("alternative","Alternative hypothesis",choices=c(
        "Two-sided: difference is not 0"="two.sided",
        "Right-tailed: difference is greater than 0"="greater",
        "Left-tailed: difference is less than 0"="less")),
      numericInput("conf_level","Confidence level",value=0.95,
                   min=0.5,max=0.999,step=0.01),
      actionButton("run","Run t-test",class="btn-primary")
    )),
    column(8,
      h4("Data preview"),
      tableOutput("preview"),
      textOutput("counts"),
      tags$hr(),
      h4("R output"),
      p("This is the output of R's t.test():"),
      verbatimTextOutput("result",placeholder=TRUE),
      tags$hr(),
      tags$small("Paired measurements must appear in matching rows; missing pairs are removed together. For independent groups, missing values are removed separately.")
    )
  )
)

server <- function(input, output, session) {
  extension <- reactive({
    req(input$file)
    tolower(tools::file_ext(input$file$name))
  })
  output$sheet_ui <- renderUI({
    req(input$file)
    if (extension() %in% c("xlsx","xls")) {
      selectInput("sheet","Worksheet",
                  choices=readxl::excel_sheets(input$file$datapath))
    }
  })
  dataset <- reactive({
    req(input$file)
    ext <- extension()
    df <- tryCatch({
      if (ext %in% c("xlsx","xls")) {
        req(input$sheet)
        as.data.frame(readxl::read_excel(input$file$datapath,
                                          sheet=input$sheet,
                                          .name_repair="unique"))
      } else if (ext=="csv") {
        read.csv(input$file$datapath,check.names=FALSE,
                 stringsAsFactors=FALSE)
      } else stop("Choose an .xlsx, .xls, or .csv file.")
    }, error=function(e) {
      validate(need(FALSE,paste("Could not read file:",conditionMessage(e))))
    })
    validate(need(ncol(df)>=2,"The file needs at least two columns."))
    df
  })
  observeEvent(dataset(),{
    df <- dataset()
    num <- names(df)[vapply(df,is.numeric,logical(1))]
    if (length(num)>=2) {
      updateSelectInput(session,"first",choices=num,selected=num[1])
      updateSelectInput(session,"second",choices=num,selected=num[2])
    } else {
      updateSelectInput(session,"first",choices=character(0))
      updateSelectInput(session,"second",choices=character(0))
    }
  })
  output$preview <- renderTable(head(dataset(),6),
                                rownames=FALSE,striped=TRUE,spacing="xs")
  output$counts <- renderText({
    df <- dataset()
    req(input$first,input$second)
    validate(need(input$first %in% names(df) &&
                  input$second %in% names(df),"Choose numerical columns."))
    x <- df[[input$first]]
    y <- df[[input$second]]
    sprintf("Non-missing: %s = %d; %s = %d; complete pairs = %d",
            input$first,sum(is.finite(x)),
            input$second,sum(is.finite(y)),
            sum(is.finite(x)&is.finite(y)))
  })
  analysis <- eventReactive(input$run,{
    df <- dataset()
    a <- input$first; b <- input$second
    validate(need(!is.null(a)&&!is.null(b)&&a!=b,
                  "Select two different columns."),
             need(a %in% names(df)&&b %in% names(df),
                  "Columns changed; please select them again."),
             need(is.finite(input$conf_level)&&input$conf_level>0&&
                  input$conf_level<1,"Confidence level must be between 0 and 1."))
    x <- df[[a]]; y <- df[[b]]
    validate(need(is.numeric(x)&&is.numeric(y),"Choose numeric columns."))
    if (input$test=="paired") {
      keep <- is.finite(x)&is.finite(y)
      x <- x[keep]; y <- y[keep]
      validate(need(length(x)>=2,"Need at least two complete pairs."))
    } else {
      x <- x[is.finite(x)]; y <- y[is.finite(y)]
      validate(need(length(x)>=2 && length(y)>=2,
                    "Each group requires at least two values."))
    }
    ans <- tryCatch(
      stats::t.test(x,y,mu=0,alternative=input$alternative,
                    paired=(input$test=="paired"),
                    var.equal=(input$test=="pooled"),
                    conf.level=input$conf_level),
      error=function(e)e
    )
    validate(need(!inherits(ans,"error"),
                  if(inherits(ans,"error")) conditionMessage(ans) else TRUE))
    ans$data.name <- sprintf("%s and %s",a,b)
    ans
  })
  output$result <- renderPrint({
    req(input$run>0)
    print(analysis())
  })
}
shinyApp(ui,server)
