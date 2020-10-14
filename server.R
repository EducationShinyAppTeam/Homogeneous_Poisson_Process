library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyWidgets)
library(boastUtils)

shinyServer(function(input, output, session) {
  # Values for game
  # NOTE: Timed refers to the scored mode and the timer refers to the score counter
  params <- reactiveValues(constant=1, 
                           lambdatype="constant", 
                           slope=1, growth=1, 
                           coefficient=.05) 
  nevent <- 50 # number of events for game graphs
  score <- reactiveValues(val=0, 
                          prob=1, 
                          valT=0, 
                          probT=1, 
                          counter=10, 
                          bestGuess=0, 
                          bestGuessT=0) 
  timer <- reactiveValues(run=FALSE) # When game should be showing
  
  # Instructions button
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "This app explores the Poisson process to see how the distribution 
      of the count at time t, the interarrival times, and the deviation from 
      expectations behave as the intensity rate and observation time changes. 
      After reviewing the prerequisites, use the explore page to manipulate the 
      rate and number of samples. The first plot shows the samples, the second 
      the residuals, and the third the density of the interarrival times. Then 
      play the game to test your ability to guess lambda and interarrival rates 
      for various plots.",
      type = "info"
    )
  })
  
  # Go to explore button
  observeEvent(input$goover, {
    updateTabItems(session, "tabs", "exp")
  })
  
  # Go to prereqs button
  observeEvent(input$bsButton1, {
    updateTabItems(session, "tabs", "exp")
  })
  
  # Button to start game
  observeEvent(input$bsButton4, {
    updateTabItems(session, 'game', 'fib')
  })
  
  # Returns number of samples (reset if resample)
  n <- reactive({
    input$resample
    return(input$nevent)
  })
  
  # Return lambda (reset if resample)
  rate <- reactive({
    input$resample
    return(input$lambda)
  })
  
  # Return number of paths (reset if resample)
  p <- reactive({
    input$resample
    return(input$path)
  })
  
  # Explains design (given checkbox)
  output$design <- renderUI({
    if(input$designcheckbox){
      withMathJax("These plots were generated from simulations of a homogeneous 
                   Poisson Process which has exponential interarrival times with 
                   rate intensity \\(\\lambda\\).")}
  })
  
  #Simulate poisson process
  simulate <- reactive({
    m <- matrix(0, nrow = p(), ncol = n())
    for (j in 1:p()){
      x <- rexp(n(),rate())
      m[j,] <- cumsum(x)
    }
    m
  })
  
  # Plot the overall process
  output$homopois = renderPlot({
    # Set up matricies for x and y values (rows are paths, cols are samples)
    xValue = simulate()
    yValue = matrix(rep(1:n(), p()), nrow = p(), ncol = n(), byrow = T)

    # Create a data frame to use with ggplot
        names <- c("x1", "y1", "x2", "y2", "x3", "y3", "x4", "y4", "x5", "y5")
        df <- data.frame(x1=1:n(),y1=1:n(),x2=1:n(),y2=1:n(),x3=1:n(),y3=1:n(),
                       x4=1:n(),y4=1:n(),x5=1:n(),y5=1:n())
        for(j in 1:p()){
          df[,2*j-1] <- xValue[j,]
          df[,2*j] <- yValue[j,]
        }
        df <- df[,1:(2*p())]
        
        # Create plot
        plot <- ggplot(aes(x=x1, y=y1), data=df) +
          ggtitle("Homogeneous Poisson Process Plot") +
          xlim(range(xValue[1:p(),])) +
          ylim(range(yValue[1:p(),])) +
          xlab("Time (t)") +
          ylab("Number of events up to t (N(t))") +
          geom_path(aes(x=x1, y=y1), 
                    data=df, 
                    color = boastUtils::boastPalette[1]) +
          geom_point(aes(x=x1, y=y1), 
                     data=df, 
                     color = boastUtils::boastPalette[1], 
                     size=2) +
          theme(axis.text = element_text(size=18),
                plot.title = element_text(size=18, face="bold"),
                axis.title = element_text(size=18),
                panel.background = element_rect(fill = "white", color="black"),
                legend.position=c(.89,1.07),
                legend.text = element_text(size=14))
        # Add extra paths  
        for(j in 1:p()){
          plot <- plot + geom_path(aes_string(x=names[2*j-1], y=names[2*j]), 
                                 data = df, 
                                 color = boastPalette[ifelse(j==5, j+1, j)]) +
          geom_point(aes_string(x=names[2*j-1], y=names[2*j]), 
                     data = df, 
                     color = boastPalette[ifelse(j==5, j+1, j)], 
                     size = 2)
        }
    plot
  })
  
  # Residuals plot
  output$resipath <- renderPlot({
    # Create matrix for x and y values
    xValue <- simulate()
    yValue <- matrix(rep(1:n(), p()), nrow = p(), ncol = n(), byrow = T)
    yValue <- yValue - rate()*simulate()

    # Create data frame for ggplot
    names <- c("x1", "y1", "x2", "y2", "x3", "y3", "x4", "y4", "x5", "y5")
    df <- data.frame(x1=1:n(),y1=1:n(),x2=1:n(),y2=1:n(),x3=1:n(),y3=1:n(),
                   x4=1:n(),y4=1:n(),x5=1:n(),y5=1:n())
    for(j in 1:p()){
      df[,2*j-1] <- xValue[j,]
      df[,2*j] <- yValue[j,]
    }
    df <- df[,1:(2*p())]
    
    # Create actual plot
    plot <- ggplot(aes(x=x1, y=y1), data=df)+
      ggtitle("Residuals Plot")+
      xlim(range(xValue[1:p(),]))+
      ylim(range(yValue[1:p(),]))+
      xlab("Time (t)")+
      ylab("Residual value (N(t) - E(N(t)))")+
      geom_path(aes(x=x1, y=y1), 
                data=df, 
                color=boastUtils::boastPalette[1])+
      geom_point(aes(x=x1, y=y1), 
                 data=df, 
                 color=boastUtils::boastPalette[1], 
                 size=2) +
      theme(axis.text = element_text(size=18),
            plot.title = element_text(size=18, face="bold"),
            axis.title = element_text(size=18),
            panel.background = element_rect(fill = "white", color="black"),
            legend.position=c(.89,1.07),
            legend.text = element_text(size=14))+
            geom_hline(aes(yintercept=0, linetype="Zero"), 
                       show.legend=F, size=1) +
            scale_linetype_manual(name = "", values = c("dashed"))
    
    # Add extra paths
    for(j in 1:p()){
      plot <- plot + geom_path(aes_string(x=names[2*j-1], y=names[2*j]), 
                             data=df, 
                             color=boastPalette[ifelse(j==5, j+1, j)]) +
        geom_point(aes_string(x=names[2*j-1], y=names[2*j]), 
                   data=df, 
                   color=boastPalette[ifelse(j==5, j+1, j)], 
                   size=2)
    }
    plot
  })
  
  # Create interarrival time plot
  output$interarrival <- renderPlot({
    # Get simulated poisson data
      xValue <- simulate()
      
      arr <- cbind(matrix(0,nrow=p(),ncol=1),xValue) # Add column of 0s to xValue
      # Create 3 empty data frames
      interArr <- data.frame()
      Int <- data.frame()
      Group <- data.frame()
      
      
      for (i in 1:p()){ # For each path
        for (j in 1:n()){ # For each sample
          Int[j + (n())*(i - 1), 1] <- arr[i,j+1] - arr[i,j] # Assign interarrival time
          Group[j + (n())*(i - 1), 1] <- i # Mark path number
        }
      }
      # Combine the times and the path numbers
      interArr <- cbind(Int,Group)
      names(interArr) <- c("Int","Group")
      
      # Create actual plot
      plot1 <- ggplot(interArr, 
                      aes(x=Int, group=Group,color=as.factor(Group),adjust=2)) +
        theme_bw() +
        theme_classic() +
        ggtitle("Estimated Interarrival Time Distribution") +
        xlab("Interarrival Time") + 
        ylab("Estimated Density") +
        labs(fill = "Number of Path") +
        theme(panel.background = element_rect(fill = 'white', colour = 'black'),
              panel.grid.major = element_blank(),
              axis.text = element_text(size=18),
              plot.title = element_text(size=18, face="bold"),
              axis.title = element_text(size=18),
              panel.grid.minor = element_blank(),
              legend.text = element_text(size=14)
              ) +
        stat_density(geom = "line", 
                     size = 1, 
                     position = "identity",  
                     show.legend=F)+
        scale_y_continuous(expand = expansion(mult = c(0, 0.15), add = 0)) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.05), add = 0)) +
        scale_color_manual(values = boastUtils::boastPalette[c(1:4, 6)])
      
      # Add density curve if requested
      if(input$densitycheckbox){
        cutoffX <- qexp(p = 0.95,rate = rate())
        xSeq <- seq(0,cutoffX,length.out = p()*n())
        yTheo <- dexp(x = xSeq,rate = rate())
        
        plot1 <- plot1 + 
          geom_line(aes(xSeq,yTheo, linetype="True Density"),
                    colour = "black",
                    size = 1.5, 
                    show.legend=NA) +
        scale_linetype_manual(name = "",
                              values = c("solid"), 
                              guide = guide_legend(
                                override.aes = list(color = c("black"))))
      }
      plot1
    })
    
    # Warning message for single sample interarrival times  
    output$feedback <- renderPrint({
      if (n()==1) {
        cat("CAUTION: Need more than one event to estimate density of 
            interarrival times.")}
    })
    
  # GAME ----
  # Generate data to plot for game  
  data <- reactive({
    timeFun <- function(y, time)((y/params$constant)+time)
    
    # Set up matrices to hold all simulated values
    xValue <- matrix(0, nrow = 1, ncol = nevent)
    yValue <- matrix(0, nrow = 1, ncol = nevent)
    resiValue <- matrix(0, nrow = 1, ncol = nevent)
    
    # Run simulation for each path
    Y <- rexp(nevent,1)
    newtime <- 0
    x <- NULL
    i <- 1
    # Create data by moving through time
    while (i<(nevent+1)){
      time <- newtime
      x <- append(x,timeFun(Y[i], time))
      newtime <- timeFun(Y[i], time)
      i <- i+1
    }
    m <- x
    h <- 1:nevent
    int_lambda <- NULL
    # Take integrals of intensity function
    for (k in m){
      int_lambda <- append(int_lambda,
                         integrate(intensity(), lower = 0, upper = k)$value)
    }
    resi <- (h-int_lambda)
    xValue[1,] <- m
    yValue[1,] <- h
    resiValue[1,] <- resi
    # returns a data frame with the three values to be used in the various plots
    list(xValue=xValue, yValue=yValue, resiValue=resiValue)
  })
  
  # Gives the intensity function
  intensity <- reactive({
    function(t)(params$constant*t^0)
  })
  
  # Output for current score in practice or timed mode
  output$score <- renderText({
    paste("Current Score:", score$val)})
  output$scoreT <- renderText({
    paste("Current Score:", score$valT)})
  output$gamePlot2 <- renderPlot({makePlot()})
  output$plot2T <- renderPlot({makePlot()})
  makePlot <- reactive({    
    # Set up data
    xValue <- data()$xValue
    yValue <- data()$yValue
    point <- ceiling(nevent/3)
    x <- NULL
    y <- NULL
    grp <- NULL
    x <- c(x, xValue[1,])
    y <- c(y, yValue[1,])
    data <- data.frame(xValue=x, yValue=y)
    # Plot each path
    plot <- ggplot(aes(x=xValue, y=yValue), data=data) +
      geom_path()+
      geom_point(size=2) +
      ggtitle("Number of Events vs. Time")+
      xlab("Time (t)") +
      ylab("Number of events") +
      theme_bw()+
      theme(
        axis.text = element_text(size=18),
        plot.title = element_text(size=18, face="bold"),
        axis.title = element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none"
      )+
      scale_y_continuous(limits = c(0, max(yValue)*1.1), 
                         expand = expansion(mult = 0, add = c(0,0.05))) +
      scale_x_continuous(limits = c(0, max(xValue)*1.1), 
                         expand = expansion(mult = 0, add = c(0,0.05))) 
    
    plot
  })
  
  # Next button for practice mode
  observeEvent(input$nextX, {
    params$constant <- runif(1,1,10)   
    score$prob <- 1
    shinyjs::hideElement("challengeFeedback")
    shinyjs::hideElement('textFeedback')
    shinyjs::hideElement('trueAns')
    shinyjs::hideElement('MLerror')
    shinyjs::hideElement('MLguess')
    shinyjs::enable("submitX")
  })
  
  # What to do when game ends
  endGame <- reactive({
    timer$run <- FALSE
    sendSweetAlert(
      session = session,
      title = paste("Final Score:",score$valT),
      text = ifelse(score$valT<20, "Try again for a better score.", 
                    ifelse(score$valT<30, "Good work",
                           "Great Job!"))
    )
  })
  
  # Next button for timed mode
  observeEvent(input$nextT, {
    if(score$counter == 0){
      endGame()
    }
    if(score$counter > 5){
      updateNumericInput(session, "challengeChoiceT", 
                         label="Guess the Value for Lambda.")
    }
    else{updateNumericInput(session, "challengeChoiceT", 
                            label="Guess the Expected Time until Next Event")}
    params$constant <- runif(1,1,10)   
    
    score$probT <- 1
    shinyjs::hideElement('textFeedbackT')
    shinyjs::hideElement('trueAnsT')
    shinyjs::hideElement('MLerrorT')
    shinyjs::hideElement('MLguessT')
    shinyjs::enable("submitT")
    shinyjs::disable("nextT")
  })

  observeEvent(input$practiceMode, {
    if(input$practiceMode=="lambda"){
      updateRadioButtons(session, "challengeChoice", 
                         label="Guess the Value for Lambda")
    }
    else{
      updateRadioButtons(session, "challengeChoice", 
                         label="Guess the Expected Time until Next Event")
    }
  })
  # Submit button for timed mode
  observeEvent(input$submitX, {
    # If asking about lambda values
    if(input$practiceMode=="lambda"){
      error <- round(abs(input$challengeChoice - params$constant),2)
      output$trueAns <- renderText({paste("True Lambda:", 
                                          round(params$constant, 
                                                2))})
      score$val <- score$val + checkScore(correct=params$constant, 
                                        ML = (data()$yValue[50]/data()$xValue[50]),
                                        guess = input$challengeChoice, invert=F)
      score$bestGuess <- score$bestGuess + 
        round(abs(data()$yValue[50]/data()$xValue[50] - params$constant),2) 
      output$MLerror <- renderText({paste("ML Error: ", 
                                          round(abs(
                                            data()$yValue[50]/data()$xValue[50] - 
                                              params$constant),2))})
      output$MLguess <- renderText({paste("ML Guess: ", 
                                          round(abs(
                                            data()$yValue[50]/data()$xValue[50]),
                                            2))})
    }
    # If asking about wait time to next arrival
    else{
      error <- round(abs(input$challengeChoice - 1/params$constant),2)
      output$trueAns <- renderText({paste("True Expected Time until Next Event:", 
                                        round(1/params$constant, 2))})
      score$val <- score$val + checkScore(correct=1/params$constant, 
                                        ML = 1/(data()$yValue[50]/data()$xValue[50]), 
                                        guess = input$challengeChoice, 
                                        invert=T)
      score$bestGuess <- score$bestGuess + 
        round(abs(1/(data()$yValue[50]/data()$xValue[50]) - 1/params$constant),2) 
      output$MLerror <- renderText({
        paste("ML Error:", round(
          abs(1/(data()$yValue[50]/data()$xValue[50]) - 1/params$constant),2))})
      output$MLguess <- renderText({
        paste("ML Guess:", round(abs( 
          1/(data()$yValue[50]/data()$xValue[50])),2))})
    }
    
    # resets output to amount of error
    output$textFeedback <- renderText ({ 
      paste("Error: ", error, "\n")
    })
    
    shinyjs::showElement("challengeFeedback")
    shinyjs::showElement('textFeedback')
    shinyjs::showElement("trueAns")
    shinyjs::showElement('MLerror')
    shinyjs::showElement('MLguess')
    shinyjs::disable("submitX")
  })
  
  # Checks answer against the maximum likelihood guess and the correct answer
  # Assigns a score accordingly
  checkScore <- function(correct, ML, guess, invert=F){
    if(!is.na(guess)){ # If guess is null, just give score of 0
      if(invert){
        correct <- 1/correct
        ML <- 1/ML
        guess <- 1/guess
      }
      # If guess is between (endpoints included) Maximum likelihood and correct
      if((guess<=ML && guess>=correct) || (guess>=ML && guess<=correct)){
        score <- 4
      }
      # If within .5 of range for score of 4
      else if(abs(guess-ML)<.5 || abs(guess-correct)<.5){
        score <- 3
      }
      # If within 1 of range for score of 4
      else if(abs(guess-ML)<1 || abs(guess-correct)<1){
        score <- 2
      }
      # If within 1.5 of range for score of 4
      else if(abs(guess-ML)<1.5 || abs(guess-correct)<1.5){
        score <- 1
      }
      # If very far from valid range
      else{
        score <- 0
      }}
    else{score <- 0}
    score
  }
  
  # Practice tab's prompt
  output$questionPrompt <- renderText({
    label <- ifelse(input$practiceMode=="lambda", 
                    "Lambda.", 
                    "the Expected Time until the Next Arrival.")
    paste0("Guess the Value for ", label)})
  
  # Submit button for timed mode
  observeEvent(input$submitT, {
    # Case for lambda problems
    if(score$counter == 5){
      updateRadioButtons(session, "challengeChoiceT", 
                         label="Guess the Expected Time until Next Event")
    }
    if(score$counter>5){
      error <- round(abs(input$challengeChoiceT - params$constant),2)
      score$valT <- score$valT + 
        checkScore(correct=params$constant,
                   ML = (data()$yValue[50]/data()$xValue[50]), 
                   guess = input$challengeChoiceT, invert=F)
      score$bestGuessT <- score$bestGuessT + 
        round(abs(
          data()$yValue[50]/data()$xValue[50] - params$constant), 2)
      output$trueAnsT <- renderText({paste("True Lambda:", 
                                         round(params$constant, 2))})
      output$MLerrorT <- renderText({
        paste("ML Error: ", round(abs(
          data()$yValue[50]/data()$xValue[50] - params$constant),2))})
      output$MLguessT <- renderText({
        paste("ML Guess: ", round(abs(
          data()$yValue[50]/data()$xValue[50]),2))})
    }
    # Expected wait time problems
    else{
      error <- round(abs(input$challengeChoiceT - 1/params$constant),2)
      output$trueAnsT <- renderText({paste("True Expected Time:", 
                                         round(1/params$constant, 2))})
      score$bestGuessT <- score$bestGuessT + 
        round(abs(1/(data()$yValue[50]/data()$xValue[50]) - 1/params$constant),2)
      score$valT <- score$valT + 
        checkScore(correct=1/params$constant,
                   ML = 1/(data()$yValue[50]/data()$xValue[50]), 
                   guess = input$challengeChoiceT, 
                   invert=T)
      output$MLerrorT <- renderText({
        paste("ML Error: ",round(abs(
          1/(data()$yValue[50]/data()$xValue[50]) - 1/params$constant),2))})
      output$MLguessT <- renderText({
        paste("ML Guess: ", round(abs(
          1/(data()$yValue[50]/data()$xValue[50])),2))})
      }
   
    # Update output to correct error
    output$textFeedbackT <- renderText ({ 
          paste("Error: ", error, "\n")
      })
    score$counter <- score$counter - 1

    shinyjs::showElement('textFeedbackT')
    shinyjs::showElement('trueAnsT')
    shinyjs::showElement('MLerrorT')
    shinyjs::showElement('MLguessT')
    shinyjs::enable("nextT")
    shinyjs::disable("submitT")
  })
  
  # Output for number of questions left
  output$nQuestionsRemaining <- renderText({
    paste("Questions Remaining: ", score$counter, " ")
    })
  
  # Button for starting the timed mode
  observeEvent(input$startTimedGame, {
    timer$run <- TRUE
    score$valT <- 0
    score$probT <- 1
    score$counter <- 10
    click('nextT')
  })

  # Button to reset the timed game
  observeEvent(input$resetTimedGame, {
    shinyjs::enable("submitT")
    shinyjs::enable("nextT")
    shinyjs::enable("startTimedGame")
    updateNumericInput(session, "challengeChoiceT", 
                       label="Guess the Value for Lambda.")
    score$valT <- 0
    score$probT <- 1
    score$counter <- 10
  })
  
  # Button to reset practice mode
  observeEvent(input$resetPractice,{
    score$val <- 0
    score$bestGuess <- 0
    score$prob <- 1
    click("nextX")
  })
  
  # Used to tell UI when to show the actual game in game mode
  output$showGame <- reactive({timer$run})
  outputOptions(output, "showGame", suspendWhenHidden=FALSE)
  
  
  # Alt-text
  output$gamePracticePlotAlt <- renderUI({
    arrivalTimes <- toString(round(data()$xValue, 2))
    tags$script(HTML(
      paste0("$(document).ready(function() {
            document.getElementById('gamePlot2').setAttribute('aria-label',
            `This plot shows the path taken by the run generated for
            the problem. For this problem, the arrival times are ",  
             arrivalTimes, "`)})"
      )))
  })
  
  output$gamePlotAlt <- renderUI({
    arrivalTimes <- toString(round(data()$xValue, 2))
    tags$script(HTML(
      paste0("$(document).ready(function() {
            document.getElementById('plot2T').setAttribute('aria-label',
            `This plot shows the path taken by the run generated for
            the problem. For this problem, the arrival times are ",  
             arrivalTimes, "`)})"
      )))
  })
})
    
  
