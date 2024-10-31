# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Dose-Response Regression Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV/XLS/XLSX File"),
      selectInput("group", "Group", choices = NULL),
      selectInput("dose", "Dose", choices = NULL),
      selectInput("response", "Response", choices = NULL),
      selectInput("regression_model", "Regression Model", choices = c("linear", "dose_response")),
      selectInput("dose_response_type", "Dose-Response Type (for dose response)", choices = c("stimulation", "inhibition")),
      checkboxInput("doseLog", "Is `Dose` column log10-transformed?", FALSE),
      checkboxInput("xLog", "Log10 transform X-axis?", FALSE),
      checkboxInput("facet", "Facet", FALSE),
      actionButton("analyze", "Run Analysis")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("regression_plot")),
        tabPanel("Analysis", verbatimTextOutput("analysis_results"))
      )
    )
  )
)

# Define the server logic for the Shiny app
server <- function(input, output, session) {

  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    if (ext == "csv") {
      read_csv(input$file$datapath)
    } else if (ext %in% c("xls", "xlsx")) {
      read_excel(input$file$datapath)
    } else {
      stop("Unsupported file format")
    }
  })

  observe({
    req(data())
    cols <- names(data())
    updateSelectInput(session, "group", choices = cols)
    updateSelectInput(session, "dose", choices = cols)
    updateSelectInput(session, "response", choices = cols)
  })

  # Updated regression_result
  regression_result <- eventReactive(input$analyze, {
    req(input$group, input$dose, input$response)
    stat_regression(
      data = data(),
      group = input$group,
      dose = input$dose,
      response = input$response,
      regression_model = input$regression_model,
      dose_response_type = input$dose_response_type,
      doseLog = input$doseLog
    )
  })

  output$analysis_results <- renderPrint({
    req(regression_result())
    regression_result()
  })

  # Updated regression_plot
  output$regression_plot <- renderPlot({
    req(input$group, input$dose, input$response)
    plot_regression(
      data = data(),
      group = input$group,
      dose = input$dose,
      response = input$response,
      regression_model = input$regression_model,
      dose_response_type = input$dose_response_type,
      facet = input$facet
    )
  })
}

# Run the application
shinyApp(ui, server)
