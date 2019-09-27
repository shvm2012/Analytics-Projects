# -*- coding: utf-8 -*-
# Created on Fri Jan 01 16:36:30 2019
# @author: shivam


### IEX Bidding price prediction tool

load_library <- function(lib_name = NULL){
  x <- substitute(lib_name)
  lib_name <- as.character(x)
  if(is.null(lib_name)){
    print("Error: Did not enter name of package/library")
  } else {
    # Try loading the library
    message(paste0("Loading ", lib_name))
    retval <- library(lib_name, character.only=TRUE, logical.return=TRUE)
    if(retval == FALSE){ # Library did not load... try installing then loading
      install.packages(lib_name, dependencies=TRUE)
      retval <- library(lib_name, character.only=TRUE, logical.return=TRUE)
    }
  }
}
message("Loading packages (and installing if they don't exist)...")
load_library(shiny)
load_library(ggplot2)
load_library(dplyr)
load_library(readxl)
load_library(writexl)
load_library(astsa)
load_library(xts)
load_library(forecast)
rm(list=ls())

ui <- fluidPage(
                   titlePanel("IEX Bidding price prediction model"),
                  
                   sidebarPanel(
                                fileInput("file1", "Choose Excel File",multiple = FALSE,accept = c(".xlsx")),
                                tags$hr(),
                                actionButton("goButton", "Run Model"),
                                downloadButton("downloadData", "Download predictions")
                                ),
                  
                   mainPanel(plotOutput("price_plot"),tableOutput("forecasted_prices"))
                )

price_forecast <- function(data){
  
  dat_xreg <- as.vector(as.matrix(data[(nrow(data)-9*96+1):nrow(data),]))
  nine_days <- list()
  
  for(i in 1:9){
    nine_days[[i]] <- dat_xreg[((96*(i-1))+1):(i*96)]
  }
  
  xreg_list <- list()
  
  for(i in 1:7){
    xreg_list[[i]] <- NA*seq(96)
    for(j in 1:96){
      xreg_list[[i]][j] <- mean(c(nine_days[[i]][j], nine_days[[i+1]][j], nine_days[[i+2]][j]))
    }
  }
  
  xreg_train <- vector()
  xreg_forecast <- xreg_list[[7]]
  
  for(i in 1:6){
    xreg_train <- c(xreg_train,xreg_list[[i]])
  }
  
  
  data_ts = ts(data$'Rupee/unit', start = 1, frequency = 96) 
  train_ts = subset(data_ts,start = 1+length(data_ts)-6*96)
 
  lx <- BoxCox(train_ts,BoxCox.lambda(train_ts)) 
  
  model <- list()
  f <- 1
  
  for(p in 1:3){
    for(q in 1:3){
      for(P in 0:1){
        for(Q in 0:1){
          tryCatch(
            {suppressWarnings(
              {
                model[[f]] <- sarima(lx,p,1,q,P,1,Q,96,details = FALSE,Model = FALSE,xreg = xreg_train)
              }
            )
            },error=function(err){message("no hyperparameters are found")})
          
          message(p,",",1,",",q,",",P,",",1,",",Q," : model built")
          
          f<-f+1
        }
      }
    }
  }
  
  
  for(i in 1:length(model)){
    if(is.null(model[[i]]))
    {model[[i]][[5]] <- 0}
  }
  
  v <- vector()
  
  for(i in 1:length(model)){
    v[i] <- model[[i]][[5]]
  }
  
  (min_aicc <- which.min(v))
  
  
  ####
  Q <- rep(c(0,1),18)
  D <- rep(1,36)
  P <- rep(c(0,1),each = 2,9)
  q <- rep(c(1,2,3),each = 4,3)
  d <- rep(1,36)
  p <- rep(c(1,2,3),each = 12)
  params_df <- data.frame(p = p, d = d,  q = q , P = P, D = D, Q = Q)
  
  ####
  
  (sp <- as.vector(params_df[min_aicc,]))
  
  pred <- sarima.for(lx,n.ahead = 96,sp[1,1],sp[1,2],sp[1,3],sp[1,4],sp[1,5],sp[1,6],96,xreg = xreg_train, newxreg = xreg_forecast)
  
  invBoxCox <- function(x, lambda){
    if (lambda == 0) exp(x) else (lambda*x + 1)^(1/lambda)}
  
  manual_predictions <- invBoxCox(as.vector(pred$pred),BoxCox.lambda(train_ts)) #model predictions
  
# 1. Bid Recommendation slots
    
time_slots <- data.frame(index = 1:96, time_slot = c("00:00 - 00:15","00:15 - 00:30","00:30 - 00:45","00:45 - 01:00","01:00 - 01:15","01:15 - 01:30","01:30 - 01:45","01:45 - 02:00",
                                                     "02:00 - 02:15","02:15 - 02:30","02:30 - 02:45","02:45 - 03:00","03:00 - 03:15","03:15 - 03:30","03:30 - 03:45","03:45 - 04:00",
                                                     "04:00 - 04:15","04:15 - 04:30","04:30 - 04:45","04:45 - 05:00","05:00 - 05:15","05:15 - 05:30","05:30 - 05:45","05:45 - 06:00",
                                                     "06:00 - 06:15","06:15 - 06:30","06:30 - 06:45","06:45 - 07:00","07:00 - 07:15","07:15 - 07:30","07:30 - 07:45","07:45 - 08:00",
                                                     "08:00 - 08:15","08:15 - 08:30","08:30 - 08:45","08:45 - 09:00","09:00 - 09:15","09:15 - 09:30","09:30 - 09:45","09:45 - 10:00",                           
                                                     "10:00 - 10:15","10:15 - 10:30","10:30 - 10:45","10:45 - 11:00","11:00 - 11:15","11:15 - 11:30","11:30 - 11:45","11:45 - 12:00",
                                                     "12:00 - 12:15","12:15 - 12:30","12:30 - 12:45","12:45 - 13:00","13:00 - 13:15","13:15 - 13:30","13:30 - 13:45","13:45 - 14:00",                           
                                                     "14:00 - 14:15","14:15 - 14:30","14:30 - 14:45","14:45 - 15:00","15:00 - 15:15","15:15 - 15:30","15:30 - 15:45","15:45 - 16:00",
                                                     "16:00 - 16:15","16:15 - 16:30","16:30 - 16:45","16:45 - 17:00","17:00 - 17:15","17:15 - 17:30","17:30 - 17:45","17:45 - 18:00",
                                                     "18:00 - 18:15","18:15 - 18:30","18:30 - 18:45","18:45 - 19:00","19:00 - 19:15","19:15 - 19:30","19:30 - 19:45","19:45 - 20:00",                           
                                                     "20:00 - 20:15","20:15 - 20:30","20:30 - 20:45","20:45 - 21:00","21:00 - 21:15","21:15 - 21:30","21:30 - 21:45","21:45 - 22:00",
                                                     "22:00 - 22:15","22:15 - 22:30","22:30 - 22:45","22:45 - 23:00","23:00 - 23:15","23:15 - 23:30","23:30 - 23:45","23:45 - 24:00"))

  time_slots1 <- time_slots %>% mutate(tariff=c(rep(1,22),rep(2,52),rep(3,14),rep(1,8)))
  
  recommended_bidding_slots <- time_slots1 %>% mutate(predicted_prices = manual_predictions) 
  
  slots <- vector()
  
  for(i in 1:nrow(recommended_bidding_slots)){
    if (recommended_bidding_slots$tariff[i]==1) {
      slots[i] = ifelse(recommended_bidding_slots$predicted_prices[i]<2.8,"Yes","No")
    }  else if (recommended_bidding_slots$tariff[i]==2) {
      slots[i] = ifelse(recommended_bidding_slots$predicted_prices[i]<3.8,"Yes","No")
    } else{
      slots[i] = ifelse(recommended_bidding_slots$predicted_prices[i]<5,"Yes","No")
    }
    
  }
  
  # return(recommendations <- time_slots %>% mutate(Recommendations = slots, Predicted_Prices = manual_predictions))
  
  return(recommendations <- time_slots %>% mutate(Predicted_Prices = manual_predictions))
  
} 

server <- function(input, output) {
  
  observeEvent(input$goButton,{
    #source("app.R")
    
    #reading the excel_file
    req(input$file1)
    
    inFile <- input$file1
    
    data <- read_excel(inFile$datapath, 1)
    
    result <- price_forecast(data)
    
    output$forecasted_prices <- renderTable({result})
    
    
    output$price_plot <- renderPlot({    
      # plot.ts(result$Predicted_Prices)
      
      ggplot(result,aes(x = index, y = Predicted_Prices))+geom_line()+ xlab("Time slot index") +
        ylab("Predicted price (in Rupee)") + geom_hline(yintercept = mean(result$Predicted_Prices),color = "red",linetype="dashed")+
        geom_text(aes(5,mean(result$Predicted_Prices)+0.1,label = ("avg. predicted price")))
      
    })
    
    
    tomorrow <- format((Sys.Date()+2),"%d_%B_%Y")
    
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(tomorrow,"_IEX_Forecasted_Prices", ".xlsx", sep="")
      },
      content = function(file) {
        write_xlsx(result, file)
      }
    )
    
    
  })
}
######################################### Run Application
shinyApp(ui = ui,server = server)

