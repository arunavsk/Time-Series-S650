library(shiny)
library(shinythemes)
ui <- fluidPage(
  theme = shinytheme("darkly"),
  navbarPage("App Title",
             tabPanel("Plots"),
             tabPanel("Theory"),
             navbarMenu("More",
                        tabPanel("About"),
                        "----",
                        "Others",
                        tabPanel("FAQs"),
                        tabPanel("Resources")
             )
  ),
  titlePanel("Understanding Noise Processes", windowTitle = 'Understanding Noise Processes'),
  'What the app is about',
  sidebarLayout(
    sidebarPanel(
      sliderInput('n', '# datapoints', value = 100, min = 50, max = 1000, step = 20),
      numericInput('p', '# AR components (p)',value = 1, min = 0),
      numericInput('q', '# MA components (q)',value = 0, min = 0),
      conditionalPanel(
        condition = "input.p != 0",
        uiOutput("ARCoeff")
      ),
      conditionalPanel(
        condition = "input.q != 0",
        uiOutput("MACoeff")
      ),
      # radioButtons("typeInput", "Time Domain/Frequency Domain",
      #              choices = c("Time Domain", "Frequency Domain"),
      #              selected = "Time Domain")
                 ),
    mainPanel(
      tabsetPanel(
        tabPanel("Raw Signal", plotOutput("TS"), textOutput('x')),
        tabPanel("Time Domain", plotOutput("acf"),
                 br(),br(),
                 plotOutput("pacf")),
        tabPanel("Spectral Analysis", 
                 sliderInput('filtersize', 'Filter size', value = 20, min = 10, max = 50, step = 5),
                 class = 'rightAlign',
                 plotOutput("spec"))
      )
  )
)
)
server <- function(input, output) {
  output$ARCoeff <- renderUI({
    
    
    lapply(1:input$p, function(i) {
      numericInput(paste('alpha',i,sep=''), paste('alpha',i) ,value = 0.5, min = 0)
    })
  })
  
  output$MACoeff <- renderUI({
    lapply(1:input$q, function(i) {
      numericInput(paste('beta',i,sep=''), paste('beta',i) ,value = 0.5, min = 0)
    })
  })

  arcoeff <- reactive({
    if (input$p == 0) {
      return(c(0))
    } else {
      coeff <- c()
      lapply(1:input$p, function(i) {
        coeff[i] = input[[paste0('alpha', as.character(i))]]
      })
      return(coeff)
    }
  })
  
  macoeff <- reactive({
    if (input$q == 0) {
      return(c(0))
    } else {
      coeff <- c()
      lapply(1:input$q, function(i) {
        coeff[i] = input[[paste0('beta', as.character(i))]]
      })
      return(coeff)
    }
  })
  
  # reactive({print(macoeff())})
  # reactive({print(arcoeff())})
  TSdata  <- reactive({
    arima.sim(n = input$n, model = list( 
                                    ar = arcoeff(),
                                    ma = macoeff()
                                    ),
                                    sd = 0.1)
  })
  
  output$x <- renderText({arcoeff()})
  
  output$TS <- renderPlot({
    plot(TSdata(), type = 'l')
  })
  
  output$acf <- renderPlot({
    acf(TSdata(), 20)
  })
  
  output$pacf <- renderPlot({
    pacf(TSdata(), 20)
  })
  output$spec <- renderPlot({
    spec.pgram(TSdata(),spans=c(input$filtersize,input$filtersize),taper=0,log="no")
  })
  }


shinyApp(ui = ui, server = server)
