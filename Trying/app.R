library(shiny)
library(meta)

ui <- fluidPage(
  # Link to custom CSS file
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  titlePanel(
    div("Forest Plot and Drapery Plot Meta-Analysis", id = "title")
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",  # Apply CSS to style sidebar
      fileInput("file", "Upload CSV file", accept = ".csv"),
      
      checkboxInput("prediction", "Include Prediction Interval", value = TRUE),
      
      selectInput("layout", "Forest Plot Layout", choices = c("RevMan5", "JAMA")),
      
      radioButtons("method", "Choose Effect Size Input Method:",
                   choices = list("Direct TE and SE" = "direct", 
                                  "CI Lower and Upper" = "ci"))
    ),
    
    mainPanel(
      class = "main-panel",  # Apply CSS to style main panel
      plotOutput("forestPlot", height = "500px", width = "100%"),
      
      # Download buttons under each plot
      downloadButton("downloadForestPlot", "Download Forest Plot")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive data import
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Create the meta-analysis object once, based on user-selected method
  meta_result <- reactive({
    req(data())
    
    if (input$method == "direct") {
      # Method 1: Using TE and seTE directly
      metagen(
        TE = data()$ORR,
        seTE = (data()$ci_upper - data()$ci_lower) / (2 * 1.96),
        studlab = data()$Study,
        data = data(),
        comb.fixed = FALSE,
        comb.random = TRUE,
        prediction = input$prediction,
        method.tau = "REML"
      )
    } else {
      # Method 2: Using ci_lower and ci_upper directly
      metagen(
        TE = data()$ORR,
        lower = data()$ci_lower,
        upper = data()$ci_upper,
        studlab = data()$Study,
        data = data(),
        comb.fixed = FALSE,
        comb.random = TRUE,
        prediction = input$prediction,
        method.tau = "REML"
      )
    }
  })
  
  # Render forest plot
  output$forestPlot <- renderPlot({
    req(meta_result())
    result <- meta_result()
    forest(result, layout = input$layout)
  }, height = 500, width = "auto")
  
  # Render drapery plot
  
  
  # Download handler for the forest plot
  output$downloadForestPlot <- downloadHandler(
    filename = function() {
      paste("forest_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 680, height = 480, units = "px")
      forest(meta_result(), layout = input$layout)
      dev.off()
    }
  )
  
  # Download handler for the drapery plot
  
}

shinyApp(ui, server)
