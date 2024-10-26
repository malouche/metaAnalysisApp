
library(shiny)
library(meta)

ui <- fluidPage(
  # Link to custom CSS styling for floating sidebar and background plot
  tags$head(
    tags$style(HTML("
      /* Floating Sidebar */
      .sidebar {
        position: fixed;
        top: 10%;
        left: 2%;
        background-color: rgba(255, 255, 255, 0.8); /* Slightly transparent for floating effect */
        padding: 15px;
        border-radius: 8px;
        width: 20%;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
        z-index: 1000;
      }
      
      /* Fullscreen Background Plot */
      .main-panel {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100vh;
        z-index: -1;
        display: flex;
        justify-content: center;
        align-items: center;
      }
    "))
  ),

  titlePanel(div("Forest Plot Meta-Analysis", id = "title")),

  # Layout with floating sidebar and fullscreen main panel
  div(class = "sidebar",
      fileInput("file", "Upload CSV file", accept = ".csv"),
      checkboxInput("prediction", "Include Prediction Interval", value = TRUE),
      selectInput("layout", "Forest Plot Layout", choices = c("RevMan5", "JAMA")),
      radioButtons("method", "Choose Effect Size Input Method:",
                   choices = list("Direct TE and SE" = "direct", 
                                  "CI Lower and Upper" = "ci"))
  ),
  
  # Main panel for plot output as fullscreen background
  div(class = "main-panel",
      plotOutput("forestPlot", height = "100%", width = "100%")
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
  }, height = "auto", width = "auto")
  
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

shinyApp(ui = ui, server = server)
