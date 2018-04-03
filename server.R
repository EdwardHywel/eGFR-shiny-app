library(shiny)
library(ggplot2)
library(knitr)
library(dplyr)


load("data/sqrt_final_model.rda")
load("data/RandomOrderData.rda")
source("data/CKD-EPI_model_functions.R")

DeBois <- function(Ht, Wt){
  0.007184 * Ht^0.725 * Wt^0.425
}

shinyServer(function(input, output){
  
  data <- reactive({
    if(input$HtUnit == "met"){
      Ht = input$Ht
    }
    if(input$HtUnit == "imp"){
      ht_inch = as.numeric(input$inch) + as.numeric(input$feet)*12
      Ht = 2.54*ht_inch
    }
    if(input$WtUnit == "met"){
      Wt = input$Wt
    }
    if(input$WtUnit == "imp"){
      wt_pounds = as.numeric(input$pounds) + as.numeric(input$stone)*14
      Wt = 0.453592*wt_pounds
    }
    if(input$CreatUnit == "umol"){
      Creat <- input$Creat/88.4
    } else if(input$CreatUnit == "mg"){
      Creat <- input$Creat
    }
    
    new_data <- data.frame(Creat = as.numeric(Creat), 
                           Age=as.numeric(input$Age), 
                           Wt=as.numeric(Wt), 
                           Ht=as.numeric(Ht), 
                           Sex=input$Sex)
    new_data$log_Creat <- log(new_data$Creat)
    new_data$SufA <- DeBois(new_data$Ht, new_data$Wt)
    new_data
  })
  
  prob <- reactive({
    prediction <- predict(object = sqrt_full, newdata = data(), se.fit = T)
    
    df <- sqrt_full$df.residual
    se.fit <-prediction$se.fit
    sigma <- summary(sqrt_full)$sigma
    se.pred <- sqrt(se.fit^2 + sigma^2)
    fit <- prediction$fit
    testValue <- sqrt(input$TestValue)
    
    if(input$LesGre == "below"){
      prob <- pt((fit - testValue)/se.pred, df =df, lower.tail = F)
    } else if(input$LesGre == "above"){
      prob <- pt((fit - testValue)/se.pred, df =df, lower.tail = T)
    }
    prob
  })
  

  output$BSA_statment<- renderText({ 
    data = data()
    paste0("The body surface area for both models was calculated as ", round(data$SufA, 2), " m\u00B2 using the DuBois formula\u00B3 and the input data provided.")
  }) 
  
  output$JWpred <- renderUI({
    prediction <- predict(object = sqrt_full, newdata = data(), interval = "prediction", level = input$Conf/100)^2
    div(class="alert alert-success", style="font-size: 20px; width: 250px; text-align: left; font-weight:bold", paste(round(prediction[1],2), "mL/min"))
  })
  
  output$JW_CI1 <- renderText({
    paste0("The ", input$Conf, "% confidence interval for this predicted value is:")
  })
  
  output$JW_CI2 <- renderText({
    prediction <- predict(object = sqrt_full, newdata = data(), interval = "prediction", level = input$Conf/100)^2
    paste0(round(prediction[2],2), "-", round(prediction[3],2), " mL/min")
  })

  
  output$CKDpred <- renderPrint({
    data = data()
    prediction <- Original_CKD_model_adjusted(data$Sex, data$Creat, data$Age, BSA = data$SufA)
    div(class="alert alert-success", style="font-size: 20px; width: 250px; text-align: left", paste(round(prediction,2), "mL/min"))
  })
  

  
  # output$JWprob1 <- renderUI({
  #   if (!input$Hyptest) return()
  #   paste0("The probability that the true GFR value is ", input$LesGre, " a threshold value of ", input$TestValue, " is")
  # })
  # 
  # output$JWprob2 <- renderUI({
  #   if (!input$Hyptest) return()
  #   div(class="alert alert-info", style="font-size: 20px; width: 250px; text-align: left; margin-bottom: 0", round(prob(), 3))
  # })
  # 
  # output$JWprob3 <- renderUI({
  #   if (!input$Hyptest) return()
  #     paste0("Therefore out of 100 patients with the same input values we estimate that ", round(prob(), 3)*100, " would have a GFR value ", input$LesGre, " the threshold value of ", input$TestValue)
  # })
  
  output$JWprob1 <- renderUI({
    if (!input$Hyptest) return()
    paste0("The model estimates that out of 100 patients with the same input values ")
  })
  
  output$JWprob2 <- renderUI({
    if (!input$Hyptest) return()
    div(class="alert alert-info", style="font-size: 20px; width: 250px; text-align: left; margin-bottom: 0; font-weight:bold", round(prob(), 4)*100)
  })
  
  output$JWprob3 <- renderUI({
    if (!input$Hyptest) return()
    paste0("are expected to have a GFR value ", input$LesGre, " the threshold value of ", 
           input$TestValue, ". This is based on the probability of ", round(prob(), 4), " that for these given input values the true GFR value is ", 
           input$LesGre, " a threshold value of ", input$TestValue, ".")
  })
  

  
  ##############################################################################
  
  output$hist1 <- renderPlot({
    newdata = data()
    p <- RandomOrderData %>% 
      ggplot(aes(x=Creat)) + geom_histogram(aes(y=..density..), fill="#D9D9D9", colour="black", bins = 25, size=.5) + 
      geom_vline(xintercept = newdata$Creat, colour="red", size=rel(2)) + 
      geom_vline(xintercept = max(RandomOrderData$Creat), alpha=.5) +
      geom_vline(xintercept = min(RandomOrderData$Creat), alpha=.5) + 
      xlab("Blood serum creatinine [mg/dL]") + 
      theme_bw(base_size = 18)
    p
  })
  
  output$hist2 <- renderPlot({
    newdata = data()
    p <- RandomOrderData %>% 
      ggplot(aes(x=Age)) + geom_histogram(aes(y=..density..), fill="#D9D9D9", colour="black", bins = 25, size=.5) + 
      geom_vline(xintercept = newdata$Age, colour="red", size=rel(2)) + 
      geom_vline(xintercept = max(RandomOrderData$Age), alpha=.5) +
      geom_vline(xintercept = min(RandomOrderData$Age), alpha=.5) + 
      xlab("Age [years] ") + 
      theme_bw(base_size = 18)
    p
  })
  
  output$hist3 <- renderPlot({
    newdata = data()
    p <- RandomOrderData %>% 
      ggplot(aes(x=SufA)) + geom_histogram(aes(y=..density..), fill="#D9D9D9", colour="black", bins = 25, size=.5) + 
      geom_vline(xintercept = newdata$SufA, colour="red", size=rel(2)) + 
      geom_vline(xintercept = max(RandomOrderData$SufA), alpha=.5) +
      geom_vline(xintercept = min(RandomOrderData$SufA), alpha=.5) + 
      xlab(expression(paste("Body surface area [", m^2, "]"))) + 
      theme_bw(base_size = 18) 
    p
  })
  
  output$hist4 <- renderPlot({
    newdata = data()
    p <- RandomOrderData %>% 
      ggplot(aes(x=Ht)) + geom_histogram(aes(y=..density..), fill="#D9D9D9", colour="black", bins = 25, size=.5) + 
      geom_vline(xintercept = newdata$Ht, colour="red", size=rel(2)) + 
      geom_vline(xintercept = max(RandomOrderData$Ht), alpha=.5) +
      geom_vline(xintercept = min(RandomOrderData$Ht), alpha=.5) + 
      xlab(expression(paste("Height [cm]"))) + 
      theme_bw(base_size = 18) 
    p
  })
  
  output$hist5 <- renderPlot({
    newdata = data()
    p <- RandomOrderData %>% 
      ggplot(aes(x=Wt)) + geom_histogram(aes(y=..density..), fill="#D9D9D9", colour="black", bins = 25, size=.5) + 
      geom_vline(xintercept = newdata$Wt, colour="red", size=rel(2)) + 
      geom_vline(xintercept = max(RandomOrderData$Wt), alpha=.5) +
      geom_vline(xintercept = min(RandomOrderData$Wt), alpha=.5) + 
      xlab(expression(paste("Weight [kg]"))) + 
      theme_bw(base_size = 18) 
    p
  })
  
  
  ##############################################################################
  
  
  
}
)




