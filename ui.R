library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(dplyr)
library(shinycssloaders)

# App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "Homogeneous Poisson"
APP_DESCP  <<- "This app explores the homogeneous poisson process through simulation."

# End App Meta Data------------------------------------------------------------

dashboardPage(
dashboardHeader(title = "Poisson Process", 
                         titleWidth = 250,
                         tags$li(class = "dropdown", 
                                 actionLink("info", icon("info"))),
                         tags$li(class = "dropdown",
                                 tags$a(href='https://shinyapps.science.psu.edu/',
                                        icon("home")))),

dashboardSidebar(
  width=250,
  sidebarMenu(id = 'tabs', 
              menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
              menuItem('Prerequisites', tabName = 'prerequisite', icon = icon('book')),
              menuItem('Explore', tabName = "exp", icon = icon('wpexplorer')),
              menuItem("Game", tabName = "Game", icon = icon("gamepad")),
              menuItem("References", tabName = "References", icon = icon("leanpub"))
              ),
tags$div(
  class = "sidebar-logo",
  boastUtils::psu_eberly_logo("reversed")
)),

dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css",
              href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
  ),
  useShinyjs(),
  
  # Overview Tab
  tabItems(
    tabItem(tabName = "overview",
            h1("Homogeneous Poisson Process"),
            
            withMathJax(),
            p("In this app you will explore the relationship between \\(N(t)\\) 
            and \\(t_{k}\\) through the simulation of data"),
            
            br(),
            h2("Instructions"),
            tags$ol(
              tags$li("By sliding the bar for the number of events and the 
              interarrival rate, you will see how the Poisson process will be 
                      changed."),
              tags$li("You can view the \\(N(t)\\) plotted against time as well 
                      as the residuals and the interarrival times")),
            div(style = "text-align: center", bsButton(inputId = "bsButton1", 
                                                       label = "GO!",
                                                       icon = icon('bolt'), 
                                                       size = 'large')),
            br(),
            
            h2("Acknowledgements"),
               p("This app was coded and developed by Shubo Sun and Johnson 
                 (Shunqi) Zhang."),
               p("Special thanks to Dr. Pearl for giving useful and supportive 
                 suggestions throughout the program.")
    ),
    
    # Prerequisites tab
    tabItem(
      tabName = 'prerequisite', withMathJax(),
      
      h2('Prerequisites'),br(),
      p("If a point process, \\(\\left\\{N^*(t);\\;t\\geq0\\right\\}\\) has a parameter of the form 
      \\(\\lambda t\\), with independent increments and with \\(N(t)\\) being 
      Poisson for all ", tags$em("t"), " with an average of \\(\\lambda t\\), \\(\\lambda\\) 
      is a constant that represents the rate(or intensity), then this point 
      process is called a homogeneous Poisson process."),
      br(),
      
      h3("Properties"),
      tags$ul(
        tags$li("\\(N(0)=0\\)"),
        tags$li("The expected value of \\(N(t)\\) is \\(\\lambda t\\) and the 
                expected value of \\(N(t+s)-N(t)=\\lambda s\\)."),
        tags$li("Suppose \\(N(t)\\) follows a homogeneous Poisson process 
        between time 0 and \\(T\\) with \\(t_{i}\\) being the time when the 
        \\(i^{th}\\) event occurs. Then, given \\(N(T)\\), the \\(t_{i}\\)'s 
        follow the distribution of order statistics from a uniform variable 
        between 0 and \\(T\\)."),
        tags$li("The interarrival times \\(t_{i+1}-t_{i}\\) for each \\(i\\) 
        follows exponential distribution with rate \\(\\lambda\\).")),
      tags$img(src = 'Homo2.png', width = "750px", height = "200px"),
      br(),
      div(style = "text-align: center",bsButton("goover", "GO!", 
                                                icon("bolt"), 
                                                size = "large"))
    ),
    
    # Explore tab
    tabItem(tabName = "exp",
            fluidPage(
              titlePanel("Simulation Plot for Homogeneous Poisson Process"),
              p("In this section, you will explore the poisson process. Use the 
              sliders to vary the interarrival rate for the process, the number 
              of events observed, and the number of paths, independent 
              experiments, to run. Observe how the process changes as the 
                interarrival rate changes and as the number of observed events 
                changes."),
              sidebarLayout(
                sidebarPanel(
                  h2("Design"),
                  checkboxInput("designcheckbox","Show design info:", TRUE),
                  uiOutput("design"),
                  sliderInput("lambda", "Interarrival rate",
                              min = 0.1, max = 10, value = 0.1, step = 0.1),
                  sliderInput("nevent", "Number of events up to t",
                              min = 1, max = 150, value = 100, step = 1),
                  sliderInput("path", "Number of residual paths",
                              min = 1, max = 5, value = 1, step = 1),
                  checkboxInput("densitycheckbox","Show true density curve", TRUE),
                  
                  actionButton("resample", "Sample", icon("retweet")),
                  bsPopover("new",
                            "Note",
                            "By clicking on this button, new 100 events will be 
                            generated.",
                            trigger="hover",placement="right"),
                  br(),
                  br()),
                mainPanel(
                          "Note: Individual paths are coded by color.",
                          plotOutput("homopois",height = "350px") %>% 
                            withSpinner(color="#0dc5c1"),
                          tags$script(HTML(
                            "$(document).ready(function() {
                        document.getElementById('homopois').setAttribute('aria-label',
                        `This plot shows the path taken by the run generated for 
                        the problem.`)
                      })"
                          )),
                          plotOutput("resipath",height = "350px") %>% 
                            withSpinner(color="#0dc5c1"),
                          tags$script(HTML(
                            "$(document).ready(function() {
                        document.getElementById('resipath').setAttribute('aria-label',
                        `This plot shows the path taken by the run generated for 
                        the problem.`)
                      })"
                          )),
                          plotOutput("interarrival", height = "400px") %>% 
                            withSpinner(color="#0dc5c1"),
                          tags$script(HTML(
                            "$(document).ready(function() {
                        document.getElementById('interarrival').setAttribute('aria-label',
                        `This plot shows an approximation of the density of the 
                        times between arrivals of the current run.`)
                      })"
                          )),
                          br(),
                          textOutput("feedback"),
                          tags$head(tags$style("#feedback{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                            )))))
            ),
    # Game Tab
    tabItem(tabName="Game", 
            tabsetPanel(id = "game",
                        tabPanel(title = "Instructions", value = "instr",
                                 fluidPage(theme = 'Muted',
                                           titlePanel('Instructions to Answering 
                                                      the Practice Questions'),
                                           p('Click on the GO! button to start 
                                             the game.'),
                                           p("Play the game in either game or 
                                           practice mode. Practice mode has two 
                                           versions: guess the lambda or guess 
                                           the expected time until the next arrival. 
                                           Game mode will give you 10 questions, 
                                           the first 5 guessing lambda and the 
                                           second 5 guessing the time until the 
                                           next arrival. True lambda values will be 
                                           in the range from 1 to 10."), 
                                           p('Use the input box to input your 
                                           answer to the prompt. If your from 0 
                                             to 4. A score of 4 means you guessed 
                                             between the maximum likelihood guess 
                                             and true value. As you get further 
                                             from this range, your score will 
                                             decrease.'),
                                           
                                           br(),
                                           div(style = "text-align: center",
                                               bsButton(inputId = "bsButton4",
                                                        label = "GO!", 
                                                        icon('bolt'),  
                                                        size = "median")))
                                 ),
                        tabPanel(title = "Practice Mode", value = "fib",
                                 fluidRow(
                                   column(
                                     radioButtons("practiceMode", 
                                                  "Choose a Question Type",
                                                  choiceValues=c("lambda", 
                                                                 "time"), 
                                                  choiceNames=c("Lambda", 
                                                                "Expected Time 
                                                                until Next Event"),
                                                  inline=T),
                                     #textOutput("questionPrompt"),
                                     fluidRow(
                                       column(width=6, 
                                              numericInput('challengeChoice', 
                                                    label = 'Guess the value for lambda.', 
                                                    value=0),
                                              bsButton(inputId = 'submitX', 
                                                       label = 'Check Answer',
                                                       size = 'median'),
                                              bsButton(inputId = 'nextX', 
                                                       label = 'Next',
                                                       size = 'median')
                                              ),
                                       column(width = 6, 
                                     
                                     textOutput('textFeedback'),
                                     textOutput("trueAns"),
                                     textOutput("MLguess"),
                                     textOutput("MLerror"),
                                       )),
                                   width=8),
                           column(width=2,
                                  textOutput("score"),
                                  textOutput("ML")),
                           column(width = 2, actionButton("resetPractice", "Reset"))
                         ),
                         plotOutput("gamePlot2"),  
                      htmlOutput("gamePracticePlotAlt"),
                ),
                tabPanel(title = "Game Mode", value = "time",
                         fluidRow(
                           column(
                             conditionalPanel(condition="output.showGame",
                                              fluidRow(
                                               column(width=8, 
                                                      numericInput('challengeChoiceT', 
                                                             label = 'Guess the value for lambda.',
                                                             value=0),
                                                      bsButton(inputId = 'submitT', 
                                                               label = 'Check Answer',
                                                               size = 'median'),
                                                      bsButton(inputId = 'nextT', 
                                                               label = 'Next',
                                                               size = 'median'),
                                                      br(), br()),
                                              column(width =4, 
                                                     br(),
                                                     textOutput('textFeedbackT'),
                                              textOutput("trueAnsT"),
                                              textOutput("MLguessT"),
                                              textOutput("MLerrorT")))),
                                  width=7),
                           column(width=3,
                                  textOutput("scoreT"),
                                  textOutput("MLT"),
                                  textOutput("nQuestionsRemaining")),
                           column(width=2, 
                                  actionButton("startTimedGame", "Start Game"),
                                  actionButton("resetTimedGame", "Reset Game")
                                  )
                         ),
                         conditionalPanel(condition="output.showGame",
                                           plotOutput("plot2T"),
                      htmlOutput("gamePlotAlt"),
                                          )
                         )
                )
            ),
    # References tab
    tabItem(
      tabName = "References",
      withMathJax(),
      h2("References"),
      p(
        class = "hangingindent",
        "Attali, D. (2020), shinyjs: Easily Improve the User Experience of Your 
        Shiny Apps in Seconds, R package. Available from
        https://CRAN.R-project.org/package=shinyjs"),
      p(
        class = "hangingindent",
        "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny, 
        R package. Available from https://CRAN.R-project.org/package=shinyBS"),
      p(
        class = "hangingindent",
        "Carey, R. (2019), boastUtils: BOAST Utilities, R Package. Available from
          https://github.com/EducationShinyAppTeam/boastUtils"),
      p(
        class = "hangingindent",
        "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create 
        dashboards with 'Shiny', R Package. Available from 
        https://CRAN.R-project.org/package=shinydashboard"),
      p(
        class = "hangingindent",
        "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2019),  
        shiny: Web application framework for R, R Package. Available from 
        https://CRAN.R-project.org/package=shiny"),
      p(
        class = "hangingindent",
        "Perrier, V., Meyer, F., and Granjon, D. (2020), shinyWidgets: Custom 
        Inputs Widgets for Shiny, R package. Available from 
        https://CRAN.R-project.org/package=shinyWidgets"),
      p(
        class = "hangingindent",
        "Sali, A. and Attali, D. (2020), shinycssloaders: Add CSS Loading 
        Animations to 'shiny' Outputs, R package. Available from
        https://CRAN.R-project.org/package=shinycssloaders"),
      p(
        class = "hangingindent",
        "Wickham, H., François, R., Henry L., and Müller, K. (2020), dplyr: A 
        Grammar of Data Manipulation, R package. Available from 
        https://CRAN.R-project.org/package=dplyr"),
      p(
        class = "hangingindent",
        "Wickham, H. (2016), ggplot2: Elegant graphics for data analysis, R 
        Package, New York: Springer-Verlag. Available from 
        https://ggplot2.tidyverse.org"))))
)
