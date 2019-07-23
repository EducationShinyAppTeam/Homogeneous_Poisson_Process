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
              menuItem('Explore', tabName = "exp", icon = icon('wpexplorer')),
              menuItem("Get some Practice", tabName = "game", icon = icon('gamepad'))
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
      h4("If a point process has a parameter of the form \\(\\lambda t\\), with t represents the time and 
          \\(\\lambda\\) is a constant that represents the rate(or intensity), then this point process is called 
         a homogeneous poisson process."),
      br(),
      
      h4(strong("Properties: ")),
      
      h4(tags$li("\\(N(0)=0\\)")),
      
      h4(tags$li("The expected value of N(t) is \\(\\lambda t\\).")),
      
      h4(tags$li("Suppose N(t) follows a homogeneous poisson process over a specific time period of T. Suppose 
                 we separate T into different time point \\(t_{i}\\) with i=0,1,2,..., then all \\(t_{i}\\) 
                 independently and identically follow uniform distribution within (0,T).")),
      
      h4(tags$li("The interarrival time \\(t_{i+1}-t_{i}\\) follows for each i follows exponential 
                 distribution with rate \\(\\lambda\\).")),
      tags$img(src = 'Homo2.png', width = "384px", height = "200px"),
      
      br(),
      div(style = "text-align: center",bsButton("goover", "Go to the overview", icon("bolt"), size = "medium"))
      
    ),
    
    tabItem(tabName = "overview",
            tags$a(href='http://stat.psu.edu/',tags$img(src='PS-HOR-RGB-2C', align = "left", width = 180)),
            br(),br(),br(),
            
            h3(tags$b("About:")),
            
            withMathJax(),
            h4("In this app you will explore the relationship between N(t) and \\(t_{k}\\) through the simulation
               of data"),
            
            br(),
            h3(tags$b("Instructions:")),
            
            h4(
              tags$li("In this app, you will explore a homogeneous poisson process based on the simulation of 
                      exponential distribution."),
              tags$li("By sliding the bar of the number of events and the interarrival rate, you will see how poisson 
                      process will be changed."),
              tags$li("From the plot you can see the differences between poisson process and other 
                      distributions."),
              tags$li("Read the instructions for checking the distributions you would like to see, and 
                      then click on Go button to get started!")
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
                              min = 1, max = 200, value = 100, step = 1),
                  sliderInput("path", "# of residual paths",
                              min = 1, max = 5, value = 1, step = 1),
    
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
                                    "This plot shows the distribution of interarrival times. From the plot it is
                                    easy to see that interarrival times roughly follow exponential distribution.",
                                    trigger = "hover", placement = "top"),br(),
                          br(),
                          textOutput(""))
              )
            )
    ),
    tabItem(tabName = "game",
            tabsetPanel(id = "game",
                        tabPanel(title = h4(strong("Instructions")), value = "instr",
                                 fluidPage(theme = 'Muted',
                                           titlePanel('Instructions to Answering the Practice Questions'),
                                           h3(p('Click on the GO! button to start the game.')),
                                           h3(p('Select from the dropdown menu the answer you think correct.')),
                                           br(),
                                           div(style = "text-align: center", 
                                               bsButton(inputId = "bsButton4",label = "GO!", icon('bolt'),  size = "median"))
                                )
                        ),
                        
                        tabPanel(title = h4(strong("Multiple Choices")), value = "fib",
                                 fluidRow(
                                   width = 12, style = 'color: #000000; background-color: #ffffff', htmlOutput('question')
                                 ),
                                 
                                 #uiOutput("picture", height = "400px") %>% withSpinner(color="#0dc5c1"),
                                 
                                 fluidRow(
                                   box(width = 12, style = 'color: #000000; background-color: #ff8080', htmlOutput('questionChoice'))
                                 ),
                                 
                                 conditionalPanel('input.submitX != 0',
                                                  htmlOutput('challengeFeedback'),
                                                  htmlOutput('textFeedback')),
                                 
                                 br(),
                                 
                                 div(style = "text-align: center",
                                     bsButton(inputId = 'submitX', label = 'Check Answer',size = 'median'),
                                     bsButton(inputId = 'nextX', label = 'Next',size = 'median'))
                                 )
                        )
            )
    
))

shinyUI(dashboardPage(header, sidebar, body))