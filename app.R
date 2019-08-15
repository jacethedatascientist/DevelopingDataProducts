library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("superhero"),
                pageWithSidebar(
                  # Application title
                  headerPanel("Body Mass Index (BMI) Calculator"),
                  
                  sidebarPanel(
                    uiOutput('resettable_input1'),
                    radioButtons('gender','Gender',
                                choices = c("Male", 
                                            "Female")),
                    actionButton('reset','Reset', class = 'btn-primary')
                  ), 
                  mainPanel(
                    tags$div(class = "jumbotron",
                             h4('The Body mass index (BMI) is a measure of body fat based on height and weight that applies to adult men and women.'),
                             h4('The World Health Organization (WHO) proposes a different classification for each gender.'),
                             uiOutput('resettable_input2')),
                    uiOutput('resettable_input3')
                  )
                ) 
)

server <- shinyServer(
  function(input, output, session) {
    
    BMI <- function(weight,height) {
      ifelse(input$metric == "English System (Pounds, Inches)", weight/(height^2)*703, weight/(height^2)*10000)
    }
    
    diagnostic_f <- function(weight,height){
      BMI_value <- ifelse(input$metric == "English System (Pounds, Inches)", weight/(height^2)*703, weight/(height^2)*10000)
      ifelse(input$gender == "Male",
             ifelse(BMI_value < 18.5,"Underweight",ifelse(BMI_value <25 ,"Normal Weight",ifelse(BMI_value <30 ,"Overweight","Obesity"))),
             ifelse(BMI_value < 16.5,"Underweight",ifelse(BMI_value <26 ,"Normal Weight",ifelse(BMI_value <31 ,"Overweight","Obesity"))))
    }
    
    output$estimation <- renderPrint({BMI(input$weight,input$height)})
    output$diagnostic <- renderPrint({diagnostic_f(input$weight,input$height)})
    output$weightunit <- renderPrint({ifelse(input$metric == "English System (Pounds, Inches)", "lbs", "kg")})
    output$heightunit <- renderPrint({ifelse(input$metric == "English System (Pounds, Inches)", "in", "cm")})
    output$resettable_input1 <- renderUI({
      times <- input$reset
      div(id=letters[(times %% length(letters)) + 1],
          selectInput(inputId = "metric", 
                      label = "Choose a Metric System:", 
                      choices = c("English System (Pounds, Inches)", 
                                  "International System (KG, CM)")))
    })
    output$resettable_input2 <- renderUI({
      times <- input$reset
      div(id=letters[(times %% length(letters)) + 1],
          tags$div(
            h4(ifelse(input$gender == "Male", 
                      "Below is the BMI equivalent for Males, regardless of age.",
                      "Below is the BMI equivalent for Females, regardless of age.")),
            tags$ul(
              tags$li(ifelse(input$gender == "Male",'BMI <18.5       : Underweight','BMI <16.5       : Underweight'),
                      tags$li(ifelse(input$gender == "Male",'BMI [18.5-24.9] : Normal weight','BMI [16.5-25.9] : Normal weight')),
                      tags$li(ifelse(input$gender == "Male",'BMI [25-29.9]   : Overweight','BMI [26-30.9]   : Overweight')),
                      tags$li(ifelse(input$gender == "Male",'BMI >=30        : Obesity','BMI >=31        : Obesity')))))
      )})
    output$resettable_input3 <- renderUI({
      times <- input$reset
      div(id=letters[(times %% length(letters)) + 1],
          h4('Please enter the appropriate value below:'),
          tags$div(style = "vertical-align:top;",
                   fluidRow(
                     column(width = 2,br(),
                            tags$p(HTML('Insert your<br>weight'))),         
                     column(width = 2,
                            numericInput('weight', '', 80)),
                     column(style = "top-margin : 10px;",
                            width = 2,br(),verbatimTextOutput("weightunit")),
                     column(width = 2,br(),
                            tags$p(HTML('Insert your<br>height'))),         
                     column(width = 2,
                            numericInput('height', '', 190,
                                         max = 300, min = 20, step = 1)),
                     column(style = "top-margin : 10px;",
                            width = 2,br(),verbatimTextOutput("heightunit")))), 
          tags$br(),
          tags$div(style = "vertical-align:top;",
                   fluidRow(
                     column(width = 2,br(),
                            h4('Your BMI is:')), 
                     column(style = "top-margin : 10px;",
                            width = 4,br(),verbatimTextOutput("estimation")),
                     column(width = 2,br(),
                            p(HTML('It means that<br>you are:'))),
                     column(style = "top-margin : 10px;",
                            width = 4,br(),strong(verbatimTextOutput("diagnostic")))))
      )
    })
  }
)

shinyApp(ui, server)