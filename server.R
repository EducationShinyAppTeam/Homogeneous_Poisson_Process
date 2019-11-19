library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(plotly)
library(ggplot2)
library(shinycssloaders)

colors = c("#3CA2C8", "#10559A","#CC6BB1", "#F9C6D7","#DB4C77")

poisson.homo = function(n,lambda){
  x = rexp(n,lambda)
  m = cumsum(x)
  k = 0:n
  plot(stepfun(x=m,y=k), main = "Homogeneous Poisson Plot", xlab = "Time (t)", ylab = "# of events (N(t))")
  par(new=T)
}

resi.homo = function(n,lambda){
  x = rexp(n,lambda)
  m = cumsum(x)
  h = 0:(n-1)
  resi = lambda*m - h
  mean_resi = mean(resi)
  plot(m, resi, xlim=range(m), ylim=range(resi), xlab="t", ylab="N(t)-E(N(t))", 
       main = "Residuals Plot",pch=16)
  lines(m[order(m)], resi[order(m)], xlim=range(m), ylim=range(resi), pch=16 )
  abline(h = mean_resi, col = "red", lty = 3, lwd = 3)
}

shinyServer(function(input, output, session) {
  
  observeEvent(input$goover, {
    updateTabItems(session, "tabs", "overview")
  })
  
  observeEvent(input$bsButton1, {
    updateTabItems(session, "tabs", "exp")
  })
  
  observeEvent(input$bsButton4, {
    updateTabItems(session, 'game', 'fib')
  })
  
  n = reactive({
    input$resample
    return(input$nevent)
  })
  
  rate = reactive({
    input$resample
    return(input$lambda)
  })
  
  p = reactive({
    input$resample
    return(input$path)
  })
  
  output$design = renderUI({
    if(input$designcheckbox){
      withMathJax(h4("This plot was generated from an exponential prior distribution. X-axis represents t and Y-axis 
         represents N(t). In the plot, you can see that the rate (\\(\\lambda\\)) multiplied by time (t) roughly 
         equals to # of events up to t (N(t))."))
      
    }
  })
  
  output$homopois = renderPlot({
    
    x.value = matrix(0, nrow = p(), ncol = n())
    y.value = matrix(0, nrow = p(), ncol = n())
    for (j in 1:p()){
      x = rexp(n(),rate())
      m = cumsum(x)
      h = 1:n()
      for (i in 1:n()){
        x.value[j,i] = m[i] 
        y.value[j,i] = h[i] 
      }
    }
    
    for (i in 1:p()){
      if (i == 1){
        plot(x.value[i,], y.value[i,], xlim = range(x.value[i,]), ylim = range(y.value[i,]),
             xlab="Time (t)", ylab="# of events up to t (N(t))", main = "Homogeneous Poisson Process Plot",
             col = colors[i], 
              pch=16)
        m = x.value[i,]
        y = y.value[i,]
        lines(m[order(m)], y[order(m)], xlim=range(m), ylim=range(y), pch=16, lwd = 1.5, col = colors[i])
      }
      if (i > 1){
        plot(x.value[1:i,], y.value[1:i,], xlim = range(x.value[1:i,]), ylim = range(y.value[1:i,]),
             xlab="Time (t)", ylab="# of events up to t (N(t))", main = "Homogeneous Poisson Process Plot",
             col = colors[1:i],
             pch=16)
        
        for (k in 1:i){
          m = x.value[k,]
          y = y.value[k,]
          lines(m[order(m)], y[order(m)], xlim=range(m), ylim=range(y), pch=16, lwd = 1.5, col = colors[k])
        }
        
      }
      
    }
  })
  
  output$resipath = renderPlot({
    
    x.value = matrix(0, nrow = p(), ncol = n())
    resi.value = matrix(0, nrow = p(), ncol = n())
    for (j in 1:p()){
      x = rexp(n(),rate())
      m = cumsum(x)
      h = 1:n()
      resi = h - (rate())*m 
      for (i in 1:n()){
        x.value[j,i] = m[i] 
        resi.value[j,i] = resi[i] 
      }
    }
    
    for (i in 1:p()){
      if (i == 1){
        plot(x.value[i,], resi.value[i,], xlim = range(x.value[i,]), ylim = range(resi.value[i,]),
             xlab="Time (t)", ylab="Residual value (N(t) - E(N(t)))", main = "Residuals Plot", col = colors[i], pch=16)
        m = x.value[i,]
        resi = resi.value[i,]
        lines(m[order(m)], resi[order(m)], xlim=range(m), ylim=range(resi), pch=16, lwd = 1.5, col = colors[i])
      }
      if (i > 1){
        plot(x.value[1:i,], resi.value[1:i,], xlim = range(x.value[1:i,]), ylim = range(resi.value[1:i,]),
             xlab="Time (t)", ylab="Residual value (N(t) - E(N(t)))", main = "Residuals Plot", col = colors[1:i], pch=16)
        
        for (k in 1:i){
          m = x.value[k,]
          resi = resi.value[k,]
        lines(m[order(m)], resi[order(m)], xlim=range(m), ylim=range(resi), pch=16, lwd = 1.5, col = colors[k])
        }
        
      }
      
      abline(h = 0, col = "black", lty = 3, lwd = 3)
    }
      
    
  })
  
  output$interarrival = renderPlot({
    
    x.value = matrix(0, nrow = p(), ncol = n())
    
    for (j in 1:p()){
      x = rexp(n(),rate())
      m = cumsum(x)
      for (i in 1:n()){
        x.value[j,i] = m[i] 
      }
    }
    
    arr = cbind(matrix(0,nrow=p(),ncol=1),x.value)
    inter.arr = data.frame()
    Int = data.frame()
    Group = data.frame()
    
    for (i in 1:p()){
      for (j in 1:n()){
        Int[j + (n())*(i - 1), 1] = arr[i,j+1] - arr[i,j]
        Group[j + (n())*(i - 1), 1] = i
      }
      inter.arr = cbind(Int,Group)
    }

    names(inter.arr) = c("Int","Group")

    ggplot(inter.arr, aes(x=Int, group=Group,color=as.factor(Group),adjust=2)) +
      theme_bw()+theme_classic()+
      ggtitle("Interarrival Time Distribution") +
      xlab("Time") + ylab("Estimated Frequency")+labs(fill = "Number of Path")+
      theme(plot.title = element_text(hjust = 0.5, face = "bold",size=14),
            panel.background = element_rect(fill = 'white', colour = 'black'),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      stat_density(geom = "line", size = 1, position = "identity")+
      scale_y_continuous(expand = expand_scale(mult = c(0, 0.15), add = 0)) +
      scale_x_continuous(expand = expand_scale(mult = c(0, 0.05), add = 0)) +
      scale_color_manual(values = colors)
      
   })

  # question <- read.csv('homo.csv')
  # question$Question=as.character(question$Question)
  # question$A=as.character(question$A)
  # question$B=as.character(question$B)
  # question$C=as.character(question$C)
  # question$D=as.character(question$D)
  # question$Correct=as.character(question$Correct)
  # question$Feedback=as.character(question$Feedback)
  # sapply(question, class)
  # 
  # values <- reactiveValues()
  # 
  # output$question <- renderUI ({
  #   values$num <- sample(1:16, 1, replace = FALSE) 
  #   cquestion <- question$Question[values$num]
  #   if(values$num == 1){
  #     IG<-div(style = 'text-align: center', img(src = "1.jpg", width = "600px", height = "350px"))
  #   }
  #   
  #   if(values$num == 2){
  #     IG<-div(style = 'text-align: center', img(src = "2.jpg", width = "600px", height = "350px"))
  #   }
  #   
  #   if(values$num == 3){
  #     IG<-div(style = 'text-align: center', img(src = "3.jpg", width = "600px", height = "350px"))
  #   }
  #   
  #   if(values$num == 4){
  #     IG<-div(style = 'text-align: center', img(src = "4.jpg", width = "600px", height = "350px"))
  #   }
  #   
  #   if(values$num == 5){
  #     IG<-div(style = 'text-align: center', img(src = "5.jpg", width = "600px", height = "350px"))
  #   }
  #   
  #   if(values$num == 6){
  #     IG<-div(style = 'text-align: center', img(src = "6.jpg", width = "600px", height = "350px"))
  #   }
  #   
  #   if(values$num == 7){
  #     IG<-div(style = 'text-align: center', img(src = "7.jpg", width = "600px", height = "350px"))
  #   }
  #   
  #   if(values$num == 8){
  #     IG<-div(style = 'text-align: center', img(src = "8.jpg", width = "600px", height = "350px"))
  #   }
  #   
  #   if(values$num == 9){
  #     IG<-div(style = 'text-align: center', img(src = "9.jpg", width = "600px", height = "350px"))
  #   }
  #   
  #   if(values$num == 10){
  #     IG<-div(style = 'text-align: center', img(src = "10.jpg", width = "600px", height = "350px"))
  #   }
  #   
  #   if(values$num == 11){
  #     IG<-div(style = 'text-align: center', img(src = "11.jpg", width = "600px", height = "350px"))
  #   }
  #   
  #   if(values$num == 12){
  #     IG<-div(style = 'text-align: center', img(src = "12.jpg", width = "600px", height = "350px"))
  #   }
  #   
  #   if(values$num == 13){
  #     IG<-div(style = 'text-align: center', img(src = "13.jpg", width = "600px", height = "350px"))
  #   }
  #   
  #   if(values$num == 14){
  #     IG<-div(style = 'text-align: center', img(src = "14.jpg", width = "600px", height = "350px"))
  #   }
  #   
  #   if(values$num == 15){
  #     IG<-div(style = 'text-align: center', img(src = "15.jpg", width = "600px", height = "350px"))
  #   }
  #   
  #   if(values$num == 16){
  #     IG<-div(style = 'text-align: center', img(src = "16.jpg", width = "600px", height = "350px"))
  #   }
  #   
  #   out<-HTML(paste(cquestion, IG, sep = "<br/>"))
  #   out
  #   
  # })
  # 
  # #output$picture = renderUI({
  #   
  # #})
  # 
  # output$questionChoice <- renderUI ({
  #   selectInput('challengeChoice', label = '',
  #               choices = c("Please select your response",question$A[values$num], question$B[values$num], question$C[values$num], question$D[values$num])
  #   )
  # })
  # 
  # 
  # 
  # observeEvent(input$nextX, {
  #   values$num = sample(1:16, 1, replace = FALSE) 
  #   questionUpdate <- question$Question[values$num]
  #   questionUpdate
  # })
  # 
  # observeEvent(input$submitX, {
  #   if (input$challengeChoice == question$Correct[values$num]) {
  #     output$challengeFeedback <- renderUI ({
  #       div(style = "text-align: center", img(src = 'correct.png', height = 100, width = 100))
  #     })
  #     output$textFeedback <- renderUI ({ #UI
  #       div(style = "text-align: center", tags$h4('Congratulations!'))
  #     })
  #   }
  #   else {
  #     output$challengeFeedback <- renderUI ({
  #       div(style = "text-align: center", img(src = 'incorrect.png', height = 100, width = 100))
  #     })
  #     output$textFeedback <- renderUI ({
  #       div(style = "text-align: center", tags$h4(question$Feedback[values$num]))
  #     })
  #   }
  # })
  # 
  # observeEvent(input$nextX, {
  #   shinyjs::hideElement("challengeFeedback")
  #   shinyjs::hideElement('textFeedback')
  # })
  # 
  # observeEvent(input$submitX, {
  #   shinyjs::showElement("challengeFeedback")
  #   shinyjs::showElement('textFeedback')
  # })
  
})

