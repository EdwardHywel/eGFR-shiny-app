library(shiny)
library(ggplot2)
library(knitr)
library(dplyr)


load("data/sqrt_final_model.rda")
CamGFR_v2 <- readRDS("data/CamGFR_reffitted.rds")
load("data/RandomOrderData.rda")
source("data/CKD-EPI_model_functions.R")

DuBois <- function(Ht, Wt){
  0.007184 * Ht^0.725 * Wt^0.425
}

shinyServer(function(input, output){
  
  data <- reactive({
    if(input$HtUnit == "met"){
      Ht = input$Ht
    }
    if(input$HtUnit == "imp"){
      ht_inch = (input$inch) + (input$feet)*12
      Ht = 2.54*ht_inch
    }
    if(input$WtUnit == "met"){
      Wt = input$Wt
    }
    if(input$WtUnit == "imp"){
      wt_pounds = input$pounds + input$stone*14
      Wt = 0.453592*wt_pounds
    }
    if(input$CreatUnit == "umol"){
      Creat <- input$Creat_umol/88.4
    } else if(input$CreatUnit == "mg"){
      Creat <- input$Creat_mg
    }
    
    new_data <- 
      data.frame(Creat = Creat, Age=input$Age, Wt=Wt, 
                 Ht=Ht, Sex=input$Sex, Creatinine_type = input$CreatType) 
      new_data$SufA = DuBois(new_data$Ht, new_data$Wt)
      new_data$BSA = new_data$SufA
      new_data$log_Creat = log(new_data$Creat)
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
  
  ##############################################################################
  
  output$SufA_sentance<- renderText({ 
    data = data()
    paste0("The body surface area for both models was calculated as ", 
                    round(data$SufA, 2), 
                  "m<sup>2</sup> using the DuBois-DuBois formula<sup>5</sup> and the input data provided.")
    # HTML(paste0("The body surface area for both models was calculated as ", 
    #             round(data$SufA, 2), 
    #             " m\u00B2 using the DuBois formula<sup>4</sup>\u00B3 and the input data provided."))
  }) 
  

  
  output$CamGFR_estimate <- renderUI({
    
    data = data()
    
    prediction <- if(input$UseOld == T & input$CreatType == "Non_IDMS"){
      predict(object = sqrt_full, newdata = data, interval = "prediction", level = input$Conf/100)^2
    } else {
      predict(object = CamGFR_v2, newdata = data, interval = "prediction", level = input$Conf/100)^2
    }

    prediction <- as.vector(prediction)
    prediction <- if(input$units == "ml/min"){
      prediction
    } else {
      prediction*1.73/data$SufA
    }
      
    

    div(
      div(class="alert alert-success", style="display: inline-block; font-size: 20px;  text-align: left; margin-bottom: 0",
          tags$strong(paste(round(prediction[1],2))),
          paste0("(", round(prediction[2],2), "-", round(prediction[3],2), ")")),
      div(style="display: inline-block; font-size: 20px", input$units),
      div(paste0("where the brackets denote the ", input$Conf,
             "% confidence interval for this predicted value"))
    )
  })
  
  


  
  output$CKD_estimate <- renderPrint({
    data = data()
    if(input$units == "ml/min"){
      prediction <-  Original_CKD_model_adjusted(data$Sex, data$Creat, data$Age, BSA = data$SufA)
    } else {
      prediction <-  Original_CKD_model(data$Sex, data$Creat, data$Age)
    }
    
    div(
      div(class="alert alert-success", style="display: inline-block; font-size: 20px; text-align: left; margin-bottom: 0", 
          tags$strong(paste(round(prediction,2)))), 
      div(style="display: inline-block; font-size: 20px", input$units)
    )

  })
  
  output$LM_estimate <- renderPrint({
    data = data()
    if(input$units == "ml/min"){
      prediction <- LM_revised_adj_equation(Sex = data$Sex, Creat = data$Creat, 
                                            Age = data$Age, BSA = data$SufA)
    } else {
      prediction <- LM_revised_equation(Sex = data$Sex, Creat = data$Creat, 
                                        Age = data$Age)
    }
    
    div(
      div(class="alert alert-success", style="display: inline-block; font-size: 20px; text-align: left; margin-bottom: 0", 
          tags$strong(paste(round(prediction,2)))), 
      div(style="display: inline-block; font-size: 20px", input$units)
    )
    
  })
  
  
  
  output$p_below <- renderUI({
    if (!input$Hyptest) return()
    div(
      "The model estimates that out of 100 patients with the same input values ",
      div(class="alert alert-info", style="font-size: 20px; width: 100px; text-align: left; margin-bottom: 0", tags$strong(round(prob(), 4)*100)),
      paste0("are expected to have a GFR value ", input$LesGre, " the threshold value of ",
             input$TestValue, ". This is based on the probability of ", round(prob(), 4), " that for these given input values the true GFR value is ",
             input$LesGre, " a threshold value of ", input$TestValue)
      )
  })

  output$ex2 <- renderUI({
    if (!input$Hyptest) return()
    div(class="alert alert-info", style="font-size: 20px; width: 250px; text-align: left; margin-bottom: 0", round(prob(), 4)*100)
  })

  output$ex3 <- renderUI({
    if (!input$Hyptest) return()
    paste0("are expected to have a GFR value ", input$LesGre, " the threshold value of ",
           input$TestValue, ". This is based on the probability of ", round(prob(), 4), " that for these given input values the true GFR value is ",
           input$LesGre, " a threshold value of ", input$TestValue)
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
  # Multiple patient
  output$example_input <- renderTable({
    read.table("data/example_data.csv", sep = ",", header = T) %>%
      head(n = 5)
  })
  
  
  data_input <- reactive({
    
    if(input$Nonident == F){
      return(NULL)
    }
    
    inFile <- input$data_file
    
    if (is.null(inFile)){
      return(NULL)
    }       
    
    is_xlsx <- grepl(".xlsx", inFile$datapath) | grepl(".xls", inFile$datapath)
    
    data <- if(is_xlsx == F){
      read.table(inFile$datapath, sep = input$sep, header = T)
    } else {
      "Please convert the file to a .csv or .txt file first"
    }
    data
    
  })
    
  data_output <- reactive({

    # if(is.character(data_input())){
    #   return(data_input())
    # }

    data <- data_input()
    
    data_change_names <- data.frame(Ht = data$Height, Wt = data$Weight, 
                                    Sex = data$Gender, Creat = data$Creatinine, 
                                    Creatinine_type = data$CreatinineType, 
                                    SufA = DuBois(data$Height, data$Weight),
                                    Age = data$Age,
                                    log_Creat = log(data$Creatinine))
    
    # Estimate_CamGFR <- function(data, useOld = F){
    #   if(useOld == T){
    #     ifelse(data$Creatinine_type == "IDMS", 
    #            predict(object = CamGFR_v2, newdata = data,
    #                    interval = "prediction", level = input$ConfMulti/100)^2, 
    #            predict(object = sqrt_full, newdata = data,
    #                    interval = "prediction", level = input$ConfMulti/100)^2)
    #   } else {
    #     predict(object = CamGFR_v2, newdata = data,
    #             interval = "prediction", level = input$ConfMulti/100)^2
    #   }
    # }
    
    Estimate_CamGFR <- function(data, useOld = F){
      if(useOld == T){
        if(data$Creatinine_type == "IDMS"){
          predict(object = CamGFR_v2, newdata = data,
                  interval = "prediction", level = input$ConfMulti/100)^2
        } else {
          predict(object = sqrt_full, newdata = data,
                  interval = "prediction", level = input$ConfMulti/100)^2
        } 
      } else {
        predict(object = CamGFR_v2, newdata = data,
                interval = "prediction", level = input$ConfMulti/100)^2
      }
    }
   
    len = nrow(data_change_names)
    
    CamGFR_res <- matrix(NA, nrow = len, ncol = 3)
    
    for(i in 1:len){
      CamGFR_res[i,] = Estimate_CamGFR(data_change_names[i,], useOld = input$UseOldMulti)  
    }

    # CamGFR_res <- Estimate_CamGFR(data_change_names, useOld = input$UseOldMulti)
    #     
    # CamGFR_res <- predict(object = CamGFR_v2, newdata = data_change_names,
    #                       interval = "prediction", level = input$ConfMulti/100)^2

    # CamGFR_res_old <- predict(object = sqrt_full, newdata = data_change_names,
    #                       interval = "prediction", level = input$ConfMulti/100)^2
    
    data_output <- data_change_names 
    data_output$"CKD-EPI" <- 
      Original_CKD_model_adjusted(data_output$Sex, data_output$Creat, 
                                  data_output$Age, data_output$SufA)
    data_output$CamGFR <- CamGFR_res[,1]
    data_output$"CamGFR lower" <- CamGFR_res[,2]
    data_output$"CamGFR upper" <- CamGFR_res[,3]
    
    data_output <- data_output[,c("CamGFR", "CamGFR lower", "CamGFR upper", "CKD-EPI")]  
    data_output
    
  }) 
  
  
  output$input_file <- renderTable({
    data_input()  
    })
  
  
  output$output_file <- renderTable({
    data_output()
  })

    
  output$downloadData <- downloadHandler(
    filename = function() {
      "Predicted_GFR_datatable.csv"
      # paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_output(), file, row.names = FALSE)
    }
  )

    
}
)




