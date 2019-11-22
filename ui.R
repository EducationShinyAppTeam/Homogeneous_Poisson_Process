library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyDND)
library(plotly)
library(dplyr)
library(shinycssloaders)

header = dashboardHeader(title = "Homogeneous Poisson Process", titleWidth = 350,
                         tags$li(class = "dropdown",
                                 tags$a(href = "https://shinyapps.science.psu.edu/",
                                        icon("home", lib = "font-awesome"))),
                         tags$li(class = "dropdown",
                                 actionLink("info", icon("info"), class = "myClass"))
                         )

sidebar = dashboardSidebar(
  sidebarMenu(id = 'tabs', 
              menuItem('Prerequisites', tabName = 'prerequisite', icon = icon('book')),
              menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
              menuItem('Explore', tabName = "exp", icon = icon('wpexplorer'))
              #menuItem("Get some Practice", tabName = "game", icon = icon('gamepad'))
              )
)

body = dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")
  ),
  
  useShinyjs(),
  
  tabItems(
    tabItem(
      tabName = 'prerequisite', withMathJax(),
      
      h3(strong('Background: Homogeneous Poisson Process')),br(),
      h4("If a point process, {\\(N(t);t>=0\\)} has a parameter of the form \\(\\lambda t\\), with independent increments and with \\(N(t)\\) being Poisson for all \\(t\\) with an average of \\(\\lambda t\\),
          \\(\\lambda\\) is a constant that represents the rate(or intensity), then this point process is called 
         a homogeneous Poisson process."),
      br(),
      
      h4(strong("Properties: ")),
      
      h4(tags$li("\\(N(0)=0\\)")),
      
      h4(tags$li("The expected value of \\(N(t)\\) is \\(\\lambda t\\) and the expected value of \\(N(t+s)-N(t)=\\lambda s\\).")),
      
      h4(tags$li("Suppose \\(N(t)\\) follows a homogeneous Poisson process between time 0 and \\(T\\) with \\(t_{i}\\) being the time when the \\(i^{th}\\) event occurs. Then, given \\(N(T)\\),
                  the \\(t_{i}\\)'s follow the distribution of order statistics from a uniform variable between 0 and \\(T\\). 
                 ")),
      
      h4(tags$li("The interarrival times \\(t_{i+1}-t_{i}\\) for each \\(i\\) follows exponential 
                 distribution with rate \\(\\lambda\\).")),
      tags$img(src = 'Homo2.png', width = "750px", height = "200px"),
      
      br(),
      div(style = "text-align: center",bsButton("goover", "Go to the overview", icon("bolt"), size = "medium"))
      
    ),
    
    tabItem(tabName = "overview",
            tags$a(href='http://stat.psu.edu/',tags$img(src='PS-HOR-RGB-2C.png', align = "left", width = 180)),
            br(),br(),br(),
            
            h3(tags$b("About:")),
            
            withMathJax(),
            h4("In this app you will explore the relationship between \\(N(t)\\) and \\(t_{k}\\) through the simulation
               of data"),
            
            br(),
            h3(tags$b("Instructions:")),
            
            h4(
              tags$li("By sliding the bar for the number of events and the interarrival rate, you will see how the Poisson 
                      process will be changed."),
              tags$li("You can view the \\(N(t)\\) plotted against time as well as the residuals and the interarrival times")
            ),
            div(style = "text-align: center", bsButton(inputId = "bsButton1", label = "G O !",icon = icon('bolt'), size = 'median')),
            br(),
            
            h3(tags$b("Acknowledgements:"),
               h4("This app was coded and developed by Shubo Sun and Johnson (Shunqi) Zhang."),
               h4("Special thanks to Dr. Pearl for giving useful and supportive suggestions throughout the program."))
            ),
    
    ########################### Exploration activity ################################
    tabItem(tabName = "exp",
            fluidPage(
              titlePanel("Simulation plot for Homogeneous Poisson Process"),
              sidebarLayout(
                sidebarPanel(
                  h3(strong("Design: ")),
                  checkboxInput("designcheckbox","Show design info:", TRUE),
                  uiOutput("design"),
                  sliderInput("lambda", "Interarrival Rate",
                              min = 0.1, max = 10, value = 0.1, step = 0.1),
                  sliderInput("nevent", "# of events up to t",
                              min = 1, max = 150, value = 100, step = 1),
                  sliderInput("path", "# of residual paths",
                              min = 1, max = 5, value = 1, step = 1),
                  checkboxInput("densitycheckbox","Show True Density curve", TRUE),
                  
                  actionButton("resample", "Sample", icon("retweet")),
                  bsPopover("new","Note","By clicking on this button, new 100 events will be generated.",
                            trigger="hover",placement="right"),br(),
                  
                  br()
                ),
                
                mainPanel(
                          plotOutput("homopois",height = "350px") %>% withSpinner(color="#0dc5c1"),
                          bsPopover("homopois","Sample Homogeneous Poisson Plot","This is the plot of the sample you generated on Homogeneous Poisson Plot.",
                                    trigger="hover",placement="top"),br(),
                          plotOutput("resipath",height = "350px") %>% withSpinner(color="#0dc5c1"),
                          bsPopover("resipath","Residuals Plot for Homogeneous Poisson Process",
                                    "This is the residuals plot for Homogeneous Poisson Process. ",
                                    trigger = "hover", placement = "top"),br(),
                          plotOutput("interarrival", height = "400px") %>% withSpinner(color="#0dc5c1"),
                          bsPopover("interarrival","Interarrival Times Distribution",
                                    "This plot shows the fitted density curve(s) for interarrival times from sampled processes.The thick black line shows the true density curve.",
                                    trigger = "hover", placement = "top"),br(),
                          br(),
                          textOutput("feedback"),
                          tags$head(tags$style("#feedback{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                            )
                          ))
              )
            )
    )
    # tabItem(tabName = "game",
    #         tabsetPanel(id = "game",
    #                     tabPanel(title = h4(strong("Instructions")), value = "instr",
    #                              fluidPage(theme = 'Muted',
    #                                        titlePanel('Instructions to Answering the Practice Questions'),
    #                                        h3(p('Click on the GO! button to start the game.')),
    #                                        h3(p('Select from the dropdown menu the answer you think correct.')),
    #                                        br(),
    #                                        div(style = "text-align: center", 
    #                                            bsButton(inputId = "bsButton4",label = "GO!", icon('bolt'),  size = "median"))
    #                             )
    #                     ),
    #                     
    #                     tabPanel(title = h4(strong("Multiple Choices")), value = "fib",
    #                              fluidRow(
    #                                width = 12, style = 'color: #000000; background-color: #ffffff', htmlOutput('question')
    #                              ),
    #                              
    #                              #uiOutput("picture", height = "400px") %>% withSpinner(color="#0dc5c1"),
    #                              
    #                              fluidRow(
    #                                box(width = 12, style = 'color: #000000; background-color: #ff8080', htmlOutput('questionChoice'))
    #                              ),
    #                              
    #                              conditionalPanel('input.submitX != 0',
    #                                               htmlOutput('challengeFeedback'),
    #                                               htmlOutput('textFeedback')),
    #                              
    #                              br(),
    #                              
    #                              div(style = "text-align: center",
    #                                  bsButton(inputId = 'submitX', label = 'Check Answer',size = 'median'),
    #                                  bsButton(inputId = 'nextX', label = 'Next',size = 'median'))
    #                              )
    #                     )
    #         )
    
))

shinyUI(dashboardPage(header, sidebar, body))
