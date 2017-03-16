library(shiny)
library(ggplot2)
library(knitr)
library(dplyr)


load("data/sqrt_final_model.rda")
load("data/DataForShiny.rda")
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
    
    new_data <- data.frame(Creat = Creat, 
                           Age=as.numeric(input$Age), 
                           Wt=as.numeric(Wt), 
                           Ht=as.numeric(Ht), 
                           Sex=input$Sex)
    new_data$log_Creat <- log(new_data$Creat)
    new_data$SufA <- DeBois(new_data$Ht, new_data$Wt)
    new_data
  })
  
  ##############################################################################
  
  # output$text1 <- renderText({ 
  #   data = data()
  #   paste("Blood serum creatinine:", round(data$Creat, 2), "mg/dL")
  # })
  # 
  # output$text2 <- renderText({ 
  #   data = data()
  #   paste("Age:", data$Age, "years")
  # })
  # 
  # output$text3 <- renderText({ 
  #   data = data()
  #   paste("Height:", round(data$Ht,0), "cm")
  # })
  # 
  # output$text4 <- renderText({ 
  #   data = data()
  #   paste("Weight:", round(data$Wt, 0), "kg")
  # })
  # 
  # output$text5 <- renderText({ 
  #   data = data()
  #   if(data$Sex=="M"){
  #     sex = "Male"
  #   } else {
  #     sex = "Female"
  #   }
  #   paste("Sex:", sex)
  # })
  # 
  # output$text6 <- renderText({ 
  #   data = data()
  #   paste0("Body surface area: ", round(data$SufA, 2), " m\u00B2 (Calculated using the DuBois formula\u00B9)")
  # })
  # 
  output$text6.5<- renderText({ 
    data = data()
    paste0("The body surface area for both models was calculated as ", round(data$SufA, 2), " m\u00B2 using the DuBois formula\u00B9 and the input data provided.")
  }) 
  
  output$text7 <- renderUI({
    prediction <- predict(object = sqrt_full, newdata = data(), interval = "prediction", level = input$Conf/100)^2
    tagList(
      tags$strong(paste(round(prediction[1],2), "mL/min"))
    )
  })
  
  output$text8 <- renderText({
    paste0("The ", input$Conf, "% confidence interval for this predicted value is:")
  })
  
  output$text9 <- renderText({
    prediction <- predict(object = sqrt_full, newdata = data(), interval = "prediction", level = input$Conf/100)^2
    paste0(round(prediction[2],2), "-", round(prediction[3],2), " mL/min")
  })
  
  output$text10 <- renderPrint({
    data = data()
    prediction <- Original_CKD_model_adjusted(data$Sex, data$Creat, data$Age, BSA = data$SufA)

    tagList(
      tags$strong(paste(round(prediction,2), "mL/min"))
    )
  })
  
  ##############################################################################
  
  output$hist1 <- renderPlot({
    newdata = data()
    p <- thedata %>% 
      ggplot(aes(x=Creat)) + geom_histogram(aes(y=..density..), fill="#D9D9D9", colour="black", bins = 25, size=.5) + 
      geom_vline(xintercept = newdata$Creat, colour="red", size=rel(2)) + 
      geom_vline(xintercept = max(thedata$Creat), alpha=.5) +
      geom_vline(xintercept = min(thedata$Creat), alpha=.5) + 
      xlab("Blood serum creatinine [mg/dL]") + 
      theme_bw(base_size = 18)
    p
  })
  
  output$hist2 <- renderPlot({
    newdata = data()
    p <- thedata %>% 
      ggplot(aes(x=Age)) + geom_histogram(aes(y=..density..), fill="#D9D9D9", colour="black", bins = 25, size=.5) + 
      geom_vline(xintercept = newdata$Age, colour="red", size=rel(2)) + 
      geom_vline(xintercept = max(thedata$Age), alpha=.5) +
      geom_vline(xintercept = min(thedata$Age), alpha=.5) + 
      xlab("Age [years] ") + 
      theme_bw(base_size = 18)
    p
  })
  
  output$hist3 <- renderPlot({
    newdata = data()
    p <- thedata %>% 
      ggplot(aes(x=SufA)) + geom_histogram(aes(y=..density..), fill="#D9D9D9", colour="black", bins = 25, size=.5) + 
      geom_vline(xintercept = newdata$SufA, colour="red", size=rel(2)) + 
      geom_vline(xintercept = max(thedata$SufA), alpha=.5) +
      geom_vline(xintercept = min(thedata$SufA), alpha=.5) + 
      xlab(expression(paste("Body surface area [", m^2, "]"))) + 
      theme_bw(base_size = 18) 
    p
  })

  
}
)




